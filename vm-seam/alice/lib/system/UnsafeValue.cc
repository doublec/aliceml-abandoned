//
// Authors:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt and Thorsten Brunklaus, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "generic/Tuple.hh"
#include "generic/Transients.hh"
#include "generic/ConcreteCode.hh"
#include "alice/Authoring.hh"

//
// RequestInterpreter
//
class RequestInterpreter: public Interpreter {
private:
  RequestInterpreter(): Interpreter() {}
public:
  static RequestInterpreter *self;

  static void Init() {
    self = new RequestInterpreter();
  }

  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Result Run(StackFrame *sFrame);
  virtual u_int GetInArity(ConcreteCode *concreteCode);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);

  virtual void PushCall(Closure *closure);
};

class RequestFrame: public StackFrame {
protected:
  enum { QUEUE_POS, BYNEED_POS, AGAIN_POS, SIZE };
public:
  static RequestFrame *New(Interpreter *interpreter, word queue, word byneed) {
    NEW_STACK_FRAME(frame, interpreter, SIZE);
    frame->InitArg(QUEUE_POS, queue);
    frame->InitArg(BYNEED_POS, byneed);
    frame->InitArg(AGAIN_POS, false);
    return static_cast<RequestFrame *>(frame);
  }

  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  Queue *GetQueue() {
    return Queue::FromWord(StackFrame::GetArg(QUEUE_POS));
  }
  word GetByneed() {
    return StackFrame::GetArg(BYNEED_POS);
  }
  bool GetAgain() {
    return Store::DirectWordToInt(StackFrame::GetArg(AGAIN_POS));
  }
  void SetAgain() {
    StackFrame::ReplaceArg(AGAIN_POS, true);
  }
};

RequestInterpreter *RequestInterpreter::self;

u_int RequestInterpreter::GetFrameSize(StackFrame *sFrame) {
  RequestFrame *frame = static_cast<RequestFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result RequestInterpreter::Run(StackFrame *sFrame) {
  RequestFrame *frame = static_cast<RequestFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  if (frame->GetAgain()) {
    Queue *queue = frame->GetQueue();
    Scheduler::PopFrame(frame->GetSize());
    while (!queue->IsEmpty()) {
      Thread *thread = Thread::FromWordDirect(queue->Dequeue());
      Scheduler::ResumeThread(thread);
    }
    return Worker::CONTINUE;
  } else {
    frame->SetAgain();
    Scheduler::currentData = frame->GetByneed();
    return Worker::REQUEST;
  }
}

u_int RequestInterpreter::GetInArity(ConcreteCode *) {
  return 0;
}

const char *RequestInterpreter::Identify() {
  return "RequestInterpreter";
}

void RequestInterpreter::DumpFrame(StackFrame *) {
  //--** to be done: insert useful stuff
  return;
}

void RequestInterpreter::PushCall(Closure *closure) {
  RequestFrame::New(RequestInterpreter::self, closure->Sub(0), closure->Sub(1));
}

//
// Primitives
//
DEFINE1(UnsafeValue_cast) {
  RETURN(x0);
} END

DEFINE2(UnsafeValue_same) {
  RETURN_BOOL(PointerOp::Deref(x0) == PointerOp::Deref(x1));
} END

DEFINE1(UnsafeValue_awaitRequest) {
  Transient *transient = Store::WordToTransient(x0);
  if (transient == INVALID_POINTER) RETURN(x0);
  if (transient->GetLabel() == BYNEED_LABEL) {
    Closure *closure = static_cast<Byneed *>(transient)->GetClosure();
    word wConcreteCode = closure->GetConcreteCode();
    ConcreteCode *concreteCode = ConcreteCode::FromWord(wConcreteCode);
    Queue *queue;
    Scheduler::nArgs = Scheduler::ONE_ARG;
    if (concreteCode != INVALID_POINTER &&
	concreteCode->GetInterpreter() == RequestInterpreter::self) {
      queue = Queue::FromWordDirect(closure->Sub(0));
      Scheduler::currentArgs[0] = closure->Sub(1);
    } else {
      Byneed *copy = Byneed::New(closure->ToWord());
      ConcreteCode *concCode = ConcreteCode::New(RequestInterpreter::self, 0);
      Closure *requestClosure = Closure::New(concCode->ToWord(), 2);
      queue = Queue::New(2);
      requestClosure->Init(0, queue->ToWord());
      requestClosure->Init(1, copy->ToWord());
      transient->ReplaceArg(requestClosure->ToWord());
      Scheduler::currentArgs[0] = copy->ToWord();
    }
    queue->Enqueue(Scheduler::GetCurrentThread()->ToWord());
    SUSPEND;
  }
  REQUEST(x0);
} END

DEFINE3(UnsafeValue_proj) {
  DECLARE_TUPLE(record, x0);
  x1 = x1; // ignored
  DECLARE_INT(i, x2);
  RETURN(record->Sel(i));
} END

DEFINE2(UnsafeValue_tag) {
  TagVal *tagVal = TagVal::FromWord(x0);
  x1 = x1; // ignored
  if (tagVal == INVALID_POINTER) {
    s_int i = Store::WordToInt(x0);
    if (i == INVALID_INT) REQUEST(x0);
    RETURN_INT(i);
  } else {
    RETURN_INT(tagVal->GetTag());
  }
} END

DEFINE3(UnsafeValue_projTagged) {
  DECLARE_TAGVAL(tagVal, x0);
  x1 = x1; // ignored
  DECLARE_INT(i, x2);
  RETURN(tagVal->Sel(i));
} END

DEFINE1(UnsafeValue_con) {
  DECLARE_CONVAL(conVal, x0);
  Constructor *constructor =
    conVal->IsConVal()? conVal->GetConstructor():
    static_cast<Constructor *>(static_cast<Block *>(conVal));
  RETURN(constructor->ToWord());
} END

DEFINE3(UnsafeValue_projConstructed) {
  DECLARE_CONVAL(conVal, x0);
  x1 = x1; // ignored
  DECLARE_INT(i, x2);
  Assert(conVal->IsConVal());
  RETURN(conVal->Sel(i));
} END

DEFINE2(UnsafeValue_projPoly) {
  DECLARE_RECORD(record, x0);
  DECLARE_TAGVAL(tagVal, x1);
  switch (tagVal->GetTag()) {
  case Types::ALPHA:
    {
      word wLabel = tagVal->Sel(0);
      DECLARE_STRING(label, wLabel);
      RETURN(record->PolySel(UniqueString::New(label)));
    }
  case Types::NUM:
    Error("UnsafeValue.projPoly: numeric labels not supported");
  default:
    Error("UnsafeValue.projPoly: unknown tag");
  }
} END

DEFINE1(UnsafeValue_prod) {
  DECLARE_VECTOR(labelValueVec, x0);
  u_int length = labelValueVec->GetLength();
  if (length == 0) RETURN_UNIT;
  Tuple *tuple = Tuple::New(length);
  for (u_int i = length; i--; ) {
    Tuple *labelValuePair = Tuple::FromWord(labelValueVec->Sub(i));
    if (labelValuePair == INVALID_POINTER) REQUEST(labelValueVec->Sub(i));
    tuple->Init(i, labelValuePair->Sel(1));
  }
  RETURN(tuple->ToWord());
} END

DEFINE1(UnsafeValue_tuple) {
  DECLARE_VECTOR(values, x0);
  u_int length = values->GetLength();
  if (length == 0) RETURN_UNIT;
  Tuple *tuple = Tuple::New(length);
  for (u_int i = length; i--; )
    tuple->Init(i, values->Sub(i));
  RETURN(tuple->ToWord());
} END

DEFINE3(UnsafeValue_tagged) {
  x0 = x0; // ignored
  DECLARE_INT(tag, x1);
  DECLARE_VECTOR(labelValueVec, x2);
  u_int length = labelValueVec->GetLength();
  if (length == 0) RETURN_INT(tag);
  TagVal *tagVal = TagVal::New(tag, length);
  for (u_int i = length; i--; ) {
    Tuple *labelValuePair = Tuple::FromWord(labelValueVec->Sub(i));
    if (labelValuePair == INVALID_POINTER) REQUEST(labelValueVec->Sub(i));
    tagVal->Init(i, labelValuePair->Sel(1));
  }
  RETURN(tagVal->ToWord());
} END

DEFINE3(UnsafeValue_taggedTuple) {
  x0 = x0; // ignored
  DECLARE_INT(tag, x1);
  DECLARE_VECTOR(values, x2);
  u_int length = values->GetLength();
  if (length == 0) RETURN_INT(tag);
  TagVal *tagVal = TagVal::New(tag, length);
  for (u_int i = length; i--; )
    tagVal->Init(i, values->Sub(i));
  RETURN(tagVal->ToWord());
} END

DEFINE2(UnsafeValue_closure) {
  DECLARE_TAGVAL(abstractCode, x0);
  DECLARE_VECTOR(vector, x1);
  u_int nglobals = vector->GetLength();
  word wConcreteCode =
    AliceLanguageLayer::concreteCodeConstructor(abstractCode);
  Closure *closure = Closure::New(wConcreteCode, nglobals);
  for (u_int i = nglobals; i--; )
    closure->Init(i, vector->Sub(i));
  RETURN(closure->ToWord());
} END

DEFINE1(UnsafeValue_prim) {
  DECLARE_STRING(name, x0);
  RETURN(PrimitiveTable::LookupValue(static_cast<Chunk *>(name)));
} END

DEFINE1(UnsafeValue_conName) {
  DECLARE_CONSTRUCTOR(constructor, x0);
  String *name = constructor->GetName();
  TagVal *exId = TagVal::New(Types::ExId, 1);
  exId->Init(0, name->ToWord());
  RETURN(exId->ToWord());
} END

DEFINE1(UnsafeValue_inArity) {
  DECLARE_CLOSURE(closure, x0);
  word wConcreteCode = closure->GetConcreteCode();
  ConcreteCode *concreteCode = ConcreteCode::FromWord(wConcreteCode);
  if (concreteCode == INVALID_POINTER) REQUEST(wConcreteCode);
  Interpreter *interpreter = concreteCode->GetInterpreter();
  u_int arity = interpreter->GetInArity(concreteCode);
  RETURN_INT(arity == static_cast<u_int>(INVALID_INT)? -2:
	     arity == Scheduler::ONE_ARG? -1: static_cast<s_int>(arity));
} END

DEFINE1(UnsafeValue_outArity) {
  x0 = x0; // ignored
  RETURN_INT(-2); //--** try to do better
} END

word UnsafeValue() {
  Record *record = Record::New(21);
  INIT_STRUCTURE(record, "UnsafeValue", "cast",
		 UnsafeValue_cast, 1);
  INIT_STRUCTURE(record, "UnsafeValue", "same",
		 UnsafeValue_same, 2);
  INIT_STRUCTURE(record, "UnsafeValue", "awaitRequest",
		 UnsafeValue_awaitRequest, 1);
  INIT_STRUCTURE(record, "UnsafeValue", "proj",
		 UnsafeValue_proj, 3);
  INIT_STRUCTURE(record, "UnsafeValue", "projTuple",
		 UnsafeValue_proj, 3);
  INIT_STRUCTURE(record, "UnsafeValue", "tag",
		 UnsafeValue_tag, 2);
  INIT_STRUCTURE(record, "UnsafeValue", "projTagged",
		 UnsafeValue_projTagged, 3);
  INIT_STRUCTURE(record, "UnsafeValue", "projTaggedTuple",
		 UnsafeValue_projTagged, 3);
  INIT_STRUCTURE(record, "UnsafeValue", "con",
		 UnsafeValue_con, 1);
  INIT_STRUCTURE(record, "UnsafeValue", "projConstructed",
		 UnsafeValue_projConstructed, 3);
  INIT_STRUCTURE(record, "UnsafeValue", "projConstructedTuple",
		 UnsafeValue_projConstructed, 3);
  INIT_STRUCTURE(record, "UnsafeValue", "projPoly",
		 UnsafeValue_projPoly, 2);
  INIT_STRUCTURE(record, "UnsafeValue", "prod",
		 UnsafeValue_prod, 1);
  INIT_STRUCTURE(record, "UnsafeValue", "tuple",
		 UnsafeValue_tuple, 1);
  INIT_STRUCTURE(record, "UnsafeValue", "tagged",
		 UnsafeValue_tagged, 3);
  INIT_STRUCTURE(record, "UnsafeValue", "taggedTuple",
		 UnsafeValue_taggedTuple, 3);
  INIT_STRUCTURE(record, "UnsafeValue", "closure",
		 UnsafeValue_closure, 2);
  INIT_STRUCTURE(record, "UnsafeValue", "prim",
		 UnsafeValue_prim, 1);
  INIT_STRUCTURE(record, "UnsafeValue", "conName",
		 UnsafeValue_conName, 1);
  INIT_STRUCTURE(record, "UnsafeValue", "inArity",
		 UnsafeValue_inArity, 1);
  INIT_STRUCTURE(record, "UnsafeValue", "outArity",
		 UnsafeValue_outArity, 1);
  RETURN_STRUCTURE("UnsafeValue$", record);
}
