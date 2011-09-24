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

#include "alice/Authoring.hh"
#include "generic/Debug.hh"


namespace {

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
    virtual u_int GetOutArity(ConcreteCode *concreteCode);
    virtual const char *Identify();
    virtual void DumpFrame(StackFrame *sFrame);

    virtual void PushCall(Closure *closure);
  };

  class RequestFrame: private StackFrame {
  protected:
    enum { FUTURE_POS, CLOSURE_POS, SIZE };
  public:
    static RequestFrame *New(Interpreter *interpreter, word future, word closure) {
      NEW_STACK_FRAME(frame, interpreter, SIZE);
      frame->InitArg(FUTURE_POS, future);
      frame->InitArg(CLOSURE_POS, closure);
      return static_cast<RequestFrame *>(frame);
    }

    u_int GetSize() {
      return StackFrame::GetSize() + SIZE;
    }
    Future *GetFuture() {
      word wFuture = StackFrame::GetArg(FUTURE_POS);
      return static_cast<Future *>(Store::WordToTransient(wFuture));
    }
    word GetClosure() {
      return StackFrame::GetArg(CLOSURE_POS);
    }
  };

  RequestInterpreter *RequestInterpreter::self;

  u_int RequestInterpreter::GetFrameSize(StackFrame *sFrame) {
    RequestFrame *frame = reinterpret_cast<RequestFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);
    return frame->GetSize();
  }

  Worker::Result RequestInterpreter::Run(StackFrame *sFrame) {
    RequestFrame *frame = reinterpret_cast<RequestFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);
    Construct(); // TODO: Is this really necessary?
    Future *future = frame->GetFuture();
    future->ScheduleWaitingThreads();
    future->Become(REF_LABEL, Store::IntToWord(0));
    Scheduler::PopFrame(frame->GetSize());
    Scheduler::SetNArgs(1);
    Scheduler::SetCurrentArg(0, Store::IntToWord(0));
    return Scheduler::PushCall(frame->GetClosure());
  }

  u_int RequestInterpreter::GetInArity(ConcreteCode *) {
    return 1;
  }

  u_int RequestInterpreter::GetOutArity(ConcreteCode *) {
    return 1;
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
  if (transient == INVALID_POINTER)
{
    RETURN_INT(0);
}
  if (transient->GetLabel() == BYNEED_LABEL) {
    Closure *closure = static_cast<Byneed *>(transient)->GetClosure();
    ConcreteCode *concreteCode =
      ConcreteCode::FromWord(closure->GetConcreteCode());
    word wFuture;
    if ((concreteCode != INVALID_POINTER) &&
	(concreteCode->GetInterpreter() == RequestInterpreter::self)) {
      wFuture = closure->Sub(0);
    } else {
      wFuture = Future::New()->ToWord();
      ConcreteCode *requestConcreteCode =
	ConcreteCode::New(RequestInterpreter::self, 0);
      Closure *requestClosure = Closure::New(requestConcreteCode->ToWord(), 2);
      requestClosure->Init(0, wFuture);
      requestClosure->Init(1, closure->ToWord());
      transient->ReplaceArg(requestClosure->ToWord());
    }
    Scheduler::SetNArgs(1);
    Scheduler::SetCurrentArg(0, Store::IntToWord(0));
    REQUEST(wFuture);
  }
  else {
    Scheduler::SetNArgs(1);
    Scheduler::SetCurrentArg(0, Store::IntToWord(0));
    REQUEST(x0);
  }
} END

DEFINE1(UnsafeValue_realToVector) {
  DECLARE_REAL(r, x0);
  Word8Vector *vector = Word8Vector::New(sizeof(double));
  u_char *c = r->GetNetworkRepresentation();
  for (u_int i = sizeof(double); i--; )
    vector->Init(i, Store::IntToWord(c[i]));
  RETURN(vector->ToWord());
} END

DEFINE3(UnsafeValue_proj) {
  DECLARE_TUPLE(record, x0);
  x1 = x1; // ignored
  DECLARE_INT(i, x2);
  RETURN(record->Sel(i));
} END

DEFINE2(UnsafeValue_tag) {
  s_int i = Store::WordToInt(x0);
  if (i != INVALID_INT) RETURN(x0);
  DECLARE_BLOCK(block, x0);
  if (block->GetLabel() == Alice::BIG_TAG) {
    RETURN_INT(BigTagVal::FromWord(x0)->GetTag());
  } else {
    RETURN_INT(TagVal::FromWord(x0)->GetTag());
  }
} END

DEFINE3(UnsafeValue_projTagged) {
  DECLARE_BLOCK(block, x0);
  DECLARE_INT(i, x2);
  if (block->GetLabel() == Alice::BIG_TAG) {
    RETURN(BigTagVal::FromWord(x0)->Sel(i));
  } else {
    RETURN(TagVal::FromWord(x0)->Sel(i));
  }
} END

DEFINE3(UnsafeValue_projTaggedTuple) {
  DECLARE_BLOCK(block, x0);
  DECLARE_INT(i, x2);
  if (block->GetLabel() == Alice::BIG_TAG) {
    RETURN(BigTagVal::FromWord(x0)->Sel(i));
  } else {
    RETURN(TagVal::FromWord(x0)->Sel(i));
  }
} END

DEFINE1(UnsafeValue_con) {
  DECLARE_CONVAL(conVal, x0);
  if (conVal->IsConVal()) {
    RETURN(conVal->GetConstructor()->ToWord());
  } else {
    RETURN(conVal->ToWord());
  }
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

DEFINE1(UnsafeValue_prodPoly) {
  DECLARE_VECTOR(labelValueVec, x0);
  u_int length = labelValueVec->GetLength();
  Record *record = Record::New(length);
  for (u_int i = length; i--; ) {
    Tuple *labelValuePair = Tuple::FromWord(labelValueVec->Sub(i));
    if (labelValuePair == INVALID_POINTER) REQUEST(labelValueVec->Sub(i));
    // TODO: Do we really need to request Label.t and the label string here?
    TagVal *tagVal = TagVal::FromWord(labelValuePair->Sel(0));
    if (tagVal == INVALID_POINTER) REQUEST(labelValuePair->Sel(0));
    switch (tagVal->GetTag()) {
    case Types::ALPHA:
      {
	word wLabel = tagVal->Sel(0);
	DECLARE_STRING(label, wLabel);
	record->Init(label->ExportC(), labelValuePair->Sel(1));
	break;
      }
    case Types::NUM:
      Error("UnsafeValue.prodPoly: numeric labels not supported");
    default:
      Error("UnsafeValue.prodPoly: unknown tag");
    }
  }
  RETURN(record->ToWord());
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

// TagVal Template
#define RETURN_TAGGED(TAGVAL_TYPE) {						\
  TAGVAL_TYPE *tagVal = TAGVAL_TYPE::New(tag, length);				\
  for (u_int i = length; i--; ) {						\
    Tuple *labelValuePair = Tuple::FromWord(labelValueVec->Sub(i));		\
    if (labelValuePair == INVALID_POINTER) REQUEST(labelValueVec->Sub(i));	\
    tagVal->Init(i, labelValuePair->Sel(1));					\
  }										\
  RETURN(tagVal->ToWord());							\
}

DEFINE3(UnsafeValue_tagged) {
  DECLARE_VECTOR(labels, x0);
  DECLARE_INT(tag, x1);
  DECLARE_VECTOR(labelValueVec, x2);
  u_int length = labelValueVec->GetLength();
  if (length == 0) RETURN_INT(tag);
  if (Alice::IsBigTagVal(labels->GetLength())) {
    RETURN_TAGGED(BigTagVal);
  } else {
    RETURN_TAGGED(TagVal);
  }
} END

// TagVal Template
#define RETURN_TAGGED_TUPLE(TAGVAL_TYPE) {		\
  TAGVAL_TYPE *tagVal = TAGVAL_TYPE::New(tag, length);	\
  for (u_int i = length; i--; )				\
    tagVal->Init(i, values->Sub(i));			\
  RETURN(tagVal->ToWord());				\
}

DEFINE3(UnsafeValue_taggedTuple) {
  DECLARE_VECTOR(labels, x0);
  DECLARE_INT(tag, x1);
  DECLARE_VECTOR(values, x2);
  u_int length = values->GetLength();
  if (length == 0) RETURN_INT(tag);
  if (Alice::IsBigTagVal(labels->GetLength())) {
    RETURN_TAGGED_TUPLE(BigTagVal);
  } else {
    RETURN_TAGGED_TUPLE(TagVal);
  }
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
  Chunk *nameChunk = Store::DirectWordToChunk(name->ToWord());
  RETURN(PrimitiveTable::LookupValue(nameChunk));
} END

DEFINE1(UnsafeValue_conName) {
  DECLARE_BLOCK(constructor, x0);
  String *name;
  if (constructor->GetLabel() == UNIQUESTRING_LABEL)
    name = UniqueString::FromWordDirect(constructor->ToWord())->ToString();
  else
    name = Constructor::FromWordDirect(constructor->ToWord())->GetName();
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
	     arity == 1? -1: static_cast<s_int>(arity));
} END

DEFINE1(UnsafeValue_outArity) {
  DECLARE_CLOSURE(closure, x0);
  word wConcreteCode = closure->GetConcreteCode();
  ConcreteCode *concreteCode = ConcreteCode::FromWord(wConcreteCode);
  if (concreteCode == INVALID_POINTER) REQUEST(wConcreteCode);
  Interpreter *interpreter = concreteCode->GetInterpreter();
  u_int arity = interpreter->GetOutArity(concreteCode);
  RETURN_INT(arity == static_cast<u_int>(INVALID_INT)? -2:
	     arity == 1? -1: static_cast<s_int>(arity));
} END

AliceDll word UnsafeValue() {
  RequestInterpreter::Init();
  Record *record = Record::New(23);
  INIT_STRUCTURE(record, "UnsafeValue", "cast",
		 UnsafeValue_cast, 1);
  INIT_STRUCTURE(record, "UnsafeValue", "same",
		 UnsafeValue_same, 2);
  INIT_STRUCTURE(record, "UnsafeValue", "awaitRequest",
		 UnsafeValue_awaitRequest, 1);
  INIT_STRUCTURE(record, "UnsafeValue", "realToVector",
		 UnsafeValue_realToVector, 1);
  INIT_STRUCTURE(record, "UnsafeValue", "proj",
		 UnsafeValue_proj, 3);
  INIT_STRUCTURE(record, "UnsafeValue", "projTuple",
		 UnsafeValue_proj, 3);
  INIT_STRUCTURE(record, "UnsafeValue", "tag",
		 UnsafeValue_tag, 2);
  INIT_STRUCTURE(record, "UnsafeValue", "projTagged",
		 UnsafeValue_projTagged, 3);
  INIT_STRUCTURE(record, "UnsafeValue", "projTaggedTuple",
		 UnsafeValue_projTaggedTuple, 3);
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
  INIT_STRUCTURE(record, "UnsafeValue", "prodPoly",
		 UnsafeValue_prodPoly, 1);
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
