//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000-2002
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstdio>
#include "generic/Closure.hh"
#include "alice/Authoring.hh"

// Vector.tabulate Frame
class VectorTabulateFrame: private StackFrame {
private:
  enum { VECTOR_POS, CLOSURE_POS, INDEX_POS, NUMBEROFELEMENTS_POS, SIZE };
public:
  using StackFrame::ToWord;
  // VectorTabulateFrame Accessors
  Vector *GetVector() {
    return Vector::FromWord(GetArg(VECTOR_POS));
  }
  word GetClosure() {
    return GetArg(CLOSURE_POS);
  }
  u_int GetIndex() {
    return Store::WordToInt(GetArg(INDEX_POS));
  }
  void UpdateIndex(u_int i) {
    ReplaceArg(INDEX_POS, i);
  }
  u_int GetNumberOfElements() {
    return Store::WordToInt(GetArg(NUMBEROFELEMENTS_POS));
  }
  // VectorTabulateFrame Constructor
  static VectorTabulateFrame *New(Worker *worker, Vector *vector, word closure,
				  u_int index, u_int numberOfElements) {
    StackFrame *frame = StackFrame::New(VECTOR_TABULATE_FRAME, worker, SIZE);
    frame->InitArg(VECTOR_POS, vector->ToWord());
    frame->InitArg(CLOSURE_POS, closure);
    frame->InitArg(INDEX_POS, index);
    frame->InitArg(NUMBEROFELEMENTS_POS, numberOfElements);
    return static_cast<VectorTabulateFrame *>(frame);
  }
  // VectorTabulateFrame Untagging
  static VectorTabulateFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == VECTOR_TABULATE_FRAME);
    return static_cast<VectorTabulateFrame *>(p);
  }
};

// Vector.tabulate Worker
class VectorTabulateWorker: public Worker {
private:
  static VectorTabulateWorker *self;
  // VectorTabulateWorker Constructor
  VectorTabulateWorker(): Worker() {}
public:
  // VectorTabulateWorker Static Constructor
  static void Init() {
    self = new VectorTabulateWorker();
  }
  // Frame Handling
  static void PushFrame(Vector *vector, word closure, u_int i, u_int n);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// VectorTabulateWorker Functions
//
VectorTabulateWorker *VectorTabulateWorker::self;

void VectorTabulateWorker::PushFrame(Vector *vector,
				     word closure, u_int i, u_int n) {
  VectorTabulateFrame *frame =
    VectorTabulateFrame::New(self, vector, closure, i, n);
  Scheduler::PushFrame(frame->ToWord());
}

Worker::Result VectorTabulateWorker::Run() {
  VectorTabulateFrame *frame =
    VectorTabulateFrame::FromWordDirect(Scheduler::GetFrame());
  Vector *vector = frame->GetVector();
  word closure   = frame->GetClosure();
  u_int i        = frame->GetIndex();
  u_int n        = frame->GetNumberOfElements();
  Construct();
  vector->LateInit(i, Scheduler::currentArgs[0]);
  if (++i == n) {
    Scheduler::PopFrame();
    Scheduler::nArgs = Scheduler::ONE_ARG;
    Scheduler::currentArgs[0] = vector->ToWord();
    return Worker::CONTINUE;
  } else {
    frame->UpdateIndex(i);
    Scheduler::nArgs = Scheduler::ONE_ARG;
    Scheduler::currentArgs[0] = Store::IntToWord(i);
    return Scheduler::PushCall(closure);
  }
}

const char *VectorTabulateWorker::Identify() {
  return "VectorTabulateWorker";
}

void VectorTabulateWorker::DumpFrame(word frameWord) {
  VectorTabulateFrame *frame = VectorTabulateFrame::FromWordDirect(frameWord);
  std::fprintf(stderr, "Vector Tabulate %d of %d\n",
	       frame->GetIndex(), frame->GetNumberOfElements());
}

DEFINE1(Vector_fromList) {
  DECLARE_LIST(tagVal, length, x0);
  if (length > Vector::maxLen)
    RAISE(PrimitiveTable::General_Size);
  Vector *vector = Vector::New(length);
  u_int i = 0;
  while (tagVal != INVALID_POINTER) {
    vector->Init(i++, tagVal->Sel(0));
    tagVal = TagVal::FromWord(tagVal->Sel(1));
  }
  Assert(i == length);
  RETURN(vector->ToWord());
} END

DEFINE1(Vector_length) {
  DECLARE_VECTOR(vector, x0);
  RETURN_INT(vector->GetLength());
} END

DEFINE2(Vector_sub) {
  DECLARE_VECTOR(vector, x0);
  DECLARE_INT(index, x1);
  if (static_cast<u_int>(index) >= vector->GetLength())
    RAISE(PrimitiveTable::General_Subscript);
  RETURN(vector->Sub(index));
} END

DEFINE2(Vector_tabulate) {
  DECLARE_INT(length, x0);
  if (length == 0) {
    RETURN(Vector::New(0)->ToWord());
  } else if ((length < 0) || ((u_int) length > Vector::maxLen)) {
    RAISE(PrimitiveTable::General_Size);
  } else {
    word wClosure = x1;
    Vector *vector = Vector::New(length);
    VectorTabulateWorker::PushFrame(vector, wClosure, 0, length);
    Scheduler::nArgs = Scheduler::ONE_ARG;
    Scheduler::currentArgs[0] = Store::IntToWord(0);
    return Scheduler::PushCall(wClosure);
  }
} END

void PrimitiveTable::RegisterVector() {
  VectorTabulateWorker::Init();
  Register("Vector.fromList", Vector_fromList, 1);
  Register("Vector.maxLen", Store::IntToWord(Vector::maxLen));
  Register("Vector.length", Vector_length, 1);
  Register("Vector.sub", Vector_sub, 2);
  Register("Vector.tabulate", Vector_tabulate, 2);
}
