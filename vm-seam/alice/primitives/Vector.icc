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
#include "alice/primitives/Authoring.hh"

// Vector.tabulate Frame
class VectorTabulateFrame : private StackFrame {
private:
  static const u_int VECTOR_POS  = 0;
  static const u_int CLOSURE_POS = 1;
  static const u_int INDEX_POS   = 2;
  static const u_int NUMELEM_POS = 3;
  static const u_int SIZE        = 4;
public:
  using StackFrame::ToWord;
  // VectorTabulateFrame Accessors
  Vector *GetVector() {
    return Vector::FromWord(GetArg(VECTOR_POS));
  }
  word GetClosure() {
    return GetArg(CLOSURE_POS);
  }
  int GetIndex() {
    return Store::WordToInt(GetArg(INDEX_POS));
  }
  void UpdateIndex(int i) {
    ReplaceArg(INDEX_POS, i);
  }
  int GetNumElems() {
    return Store::WordToInt(GetArg(NUMELEM_POS));
  }
  // VectorTabulateFrame Constructor
  static VectorTabulateFrame *New(Interpreter *interpreter,
				  Vector *vector, word closure,
				  int index, int numelems) {
    StackFrame *frame =
      StackFrame::New(VECTOR_TABULATE_FRAME, interpreter, SIZE);
    frame->InitArg(VECTOR_POS, vector->ToWord());
    frame->InitArg(CLOSURE_POS, closure);
    frame->InitArg(INDEX_POS, index);
    frame->InitArg(NUMELEM_POS, numelems);
    return static_cast<VectorTabulateFrame *>(frame);
  }
  // VectorTabulateFrame Untagging
  static VectorTabulateFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == VECTOR_TABULATE_FRAME);
    return static_cast<VectorTabulateFrame *>(p);
  }
};

// Vector.tabulate Interpreter
class VectorTabulateInterpreter : public Interpreter {
private:
  static VectorTabulateInterpreter *self;
  // VectorTabulateInterpreter Constructor
  VectorTabulateInterpreter() : Interpreter() {}
public:
  // VectorTabulateInterpreter Static Constructor
  static void Init() {
    self = new VectorTabulateInterpreter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack,
			Vector *vector, word closure, int i, int n);
  // Execution
  virtual Result Run(TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// VectorTabulateInterpreter Functions
//
VectorTabulateInterpreter *VectorTabulateInterpreter::self;

void VectorTabulateInterpreter::PushFrame(TaskStack *taskStack,
					  Vector *vector,
					  word closure, int i, int n) {
  VectorTabulateFrame *frame =
    VectorTabulateFrame::New(self, vector, closure, i, n);
  taskStack->PushFrame(frame->ToWord());
}

Interpreter::Result
VectorTabulateInterpreter::Run(TaskStack *taskStack) {
  VectorTabulateFrame *frame =
    VectorTabulateFrame::FromWordDirect(taskStack->GetFrame());
  Vector *vector = frame->GetVector();
  word closure   = frame->GetClosure();
  int i          = frame->GetIndex();
  int n          = frame->GetNumElems();
  Construct();
  vector->LateInit(i, Scheduler::currentArgs[0]);
  if (++i == n) {
    taskStack->PopFrame();
    Scheduler::nArgs = Scheduler::ONE_ARG;
    Scheduler::currentArgs[0] = vector->ToWord();
    return Interpreter::CONTINUE;
  } else {
    frame->UpdateIndex(i);
    Scheduler::nArgs = Scheduler::ONE_ARG;
    Scheduler::currentArgs[0] = Store::IntToWord(i);
    return taskStack->PushCall(closure);
  }
}

const char *VectorTabulateInterpreter::Identify() {
  return "VectorTabulateInterpreter";
}

void VectorTabulateInterpreter::DumpFrame(word frameWord) {
  VectorTabulateFrame *frame = VectorTabulateFrame::FromWordDirect(frameWord);
  std::fprintf(stderr, "Vector Tabulate %d of %d\n",
	       frame->GetIndex(), frame->GetNumElems());
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
  //--** should only request closure if vector not empty
  DECLARE_INT(length, x0);
  DECLARE_CLOSURE(closure, x1);
  if (length == 0) {
    RETURN(Vector::New(0)->ToWord());
  }
  else if ((length < 0) || ((u_int) length > Vector::maxLen)) {
    RAISE(PrimitiveTable::General_Size);
  }
  else {
    word cl = closure->ToWord();
    Vector *vector = Vector::New(length);
    VectorTabulateInterpreter::PushFrame(taskStack, vector, cl, 0, length);
    Scheduler::nArgs = Scheduler::ONE_ARG;
    Scheduler::currentArgs[0] = Store::IntToWord(0);
    return taskStack->PushCall(cl);
  }
} END

void PrimitiveTable::RegisterVector() {
  VectorTabulateInterpreter::Init();
  Register("Vector.fromList", Vector_fromList, 1);
  Register("Vector.maxLen", Store::IntToWord(Vector::maxLen));
  Register("Vector.length", Vector_length, 1);
  Register("Vector.sub", Vector_sub, 2);
  Register("Vector.tabulate", Vector_tabulate, 2);
}
