//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "emulator/VectorTabulateInterpreter.hh"
#endif

#include <cstdio>
#include "emulator/Authoring.hh"
#include "emulator/VectorTabulateInterpreter.hh"

// VectorTabulate Frame
class VectorTabulateFrame : private StackFrame {
private:
  static const u_int VECTOR_POS  = 0;
  static const u_int FUN_POS     = 1;
  static const u_int INDEX_POS   = 2;
  static const u_int NUMELEM_POS = 3;
  static const u_int SIZE        = 4;
public:
  using StackFrame::ToWord;
  // VectorTabulateFrame Accessors
  Vector *GetVector() {
    return Vector::FromWord(StackFrame::GetArg(VECTOR_POS));
  }
  word GetClosure() {
    return StackFrame::GetArg(FUN_POS);
  }
  int GetIndex() {
    return Store::WordToInt(StackFrame::GetArg(INDEX_POS));
  }
  int GetNumElems() {
    return Store::WordToInt(StackFrame::GetArg(NUMELEM_POS));
  }
  // VectorTabulateFrame Constructor
  static VectorTabulateFrame *New(Interpreter *interpreter,
				  Vector *vector,
				  word closure,
				  int index, int numelems) {
    StackFrame *frame =
      StackFrame::New(VECTOR_TABULATE_FRAME, interpreter, SIZE);
    frame->InitArg(VECTOR_POS, vector->ToWord());
    frame->InitArg(FUN_POS, closure);
    frame->InitArg(INDEX_POS, Store::IntToWord(index));
    frame->InitArg(NUMELEM_POS, Store::IntToWord(numelems));
    return (VectorTabulateFrame *) frame;
  }
  // VectorTabulateFrame Untagging
  static VectorTabulateFrame *FromWord(word frame) {
    Block *p = Store::WordToBlock(frame);
    Assert(p == INVALID_POINTER ||
	   p->GetLabel() == (BlockLabel) VECTOR_TABULATE_FRAME);
    return (VectorTabulateFrame *) p;
  }
};

//
// VectorTabulateInterpreter Functions
//
VectorTabulateInterpreter *VectorTabulateInterpreter::self;

void VectorTabulateInterpreter::PushFrame(TaskStack *taskStack,
					  Vector *vector,
					  word fun, int i, int n) {
  VectorTabulateFrame *frame =
    VectorTabulateFrame::New(self, vector, fun, i, n);
  taskStack->PushFrame(frame->ToWord());
}

Interpreter::Result
VectorTabulateInterpreter::Run(word args, TaskStack *taskStack) {
  VectorTabulateFrame *frame =
    VectorTabulateFrame::FromWord(taskStack->GetFrame());
  Vector *vector = frame->GetVector();
  word fun       = frame->GetClosure();
  int i          = frame->GetIndex();
  int n          = frame->GetNumElems();
  vector->LateInit(i, Interpreter::Construct(args));
  taskStack->PopFrame(); // Discard Frame
  if (++i == n) {
    Scheduler::currentArgs = Interpreter::OneArg(vector->ToWord());
    return Interpreter::CONTINUE;
  }
  else {
    VectorTabulateFrame *newFrame =
      VectorTabulateFrame::New(this, vector, fun, i, n);
    taskStack->PushFrame(newFrame->ToWord());
    Scheduler::currentArgs = Interpreter::OneArg(Store::IntToWord(i));
    return taskStack->PushCall(fun);
  }
}

const char *VectorTabulateInterpreter::Identify() {
  return "VectorTabulateInterpreter";
}

void VectorTabulateInterpreter::DumpFrame(word frameWord) {
  VectorTabulateFrame *frame = VectorTabulateFrame::FromWord(frameWord);
  Assert(frame != INVALID_POINTER);
  fprintf(stderr, "Vector Tabulate %d of %d\n",
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
    Scheduler::currentArgs = Interpreter::OneArg(Store::IntToWord(0));
    return taskStack->PushCall(cl);
  }
} END

void PrimitiveTable::RegisterVector() {
  Register("Vector.fromList", Vector_fromList, 1);
  Register("Vector.maxLen", Store::IntToWord(Vector::maxLen));
  Register("Vector.length", Vector_length, 1);
  Register("Vector.sub", Vector_sub, 2);
  Register("Vector.tabulate", Vector_tabulate, 2);
}
