//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "builtin/Primitive.hh"
#endif

#include "scheduler/Closure.hh"
#include "scheduler/TaskStack.hh"
#include "builtins/Primitive.hh"
#include "builtins/GlobalPrimitives.hh"

class PrimitiveInterpreter: public Interpreter {
private:
  Primitive::function function;
  int arity;
public:
  PrimitiveInterpreter(Primitive::function f, int n):
    function(f), arity(n == 1? -1: n) {}

  virtual ConcreteCode *Prepare(word abstractCode);
  virtual void PushCall(TaskStack *taskStack, Closure *closure);
  virtual void PopFrame(TaskStack *taskStack);
  virtual Result Run(TaskStack *taskStack, int nargs);
};

ConcreteCode *PrimitiveInterpreter::Prepare(word /*abstractCode*/) {
  Error("PrimitiveInterpreter::Prepare must never be called");
}

void PrimitiveInterpreter::PushCall(TaskStack *taskStack, Closure *w) {
  Assert(Closure::FromWord(w)->GetConcreteCode()->GetInterpreter() == this);
  taskStack->PushFrame(1);
  taskStack->PutUnmanagedPointer(0, this);
}

void PrimitiveInterpreter::PopFrame(TaskStack *taskStack) {
  taskStack->PopFrame(1);
}

Interpreter::Result
PrimitiveInterpreter::Run(TaskStack *taskStack, int nargs) {
  if (arity != nargs) {
    if (nargs == -1) {
      word suspendWord = taskStack->GetWord(0);
      if (arity == 0) { // await unit
	if (Store::WordToInt(suspendWord) == INVALID_INT) {
	  taskStack->PopFrame(1);
	  taskStack->
	    PushCall(Closure::FromWord(GlobalPrimitives::Future_await));
	  taskStack->PushFrame(1);
	  taskStack->PutWord(0, suspendWord);
	  return Result(Result::CONTINUE, 1);
	}
	Assert(Store::WordToInt(suspendWord) == 0); // unit
      } else { // deconstruct
	Tuple *tuple = Tuple::FromWord(suspendWord);
	if (tuple == INVALID_POINTER) {
	  taskStack->PopFrame(1);
	  taskStack->
	    PushCall(Closure::FromWord(GlobalPrimitives::Future_await));
	  taskStack->PushFrame(1);
	  taskStack->PutWord(0, suspendWord);
	  return Result(Result::CONTINUE, 1);
	}
	taskStack->PushFrame(arity - 1);
	Assert(tuple->GetWidth() == arity);
	for (u_int i = arity; i--; )
	  taskStack->PutWord(i, tuple->Sel(i));
      }
    } else if (nargs == 0) {
      taskStack->PushFrame(1);
      taskStack->PutWord(0, Store::IntToWord(0)); // unit
    } else { // construct
      Tuple *tuple = Tuple::New(nargs);
      for (u_int i = nargs; i--; )
	tuple->Init(i, taskStack->GetWord(i));
      taskStack->PopFrame(nargs - 1);
      taskStack->PutWord(0, tuple->ToWord());
    }
  }
  return function(taskStack);
}

void Primitive::Init() {
  RegisterInternal();
  RegisterUnqualified();
  RegisterArray();
  RegisterChar();
  RegisterFuture();
  RegisterGeneral();
  RegisterGlobalStamp();
  RegisterHole();
  RegisterInt();
  RegisterList();
  RegisterMath();
  RegisterOption();
  RegisterReal();
  RegisterString();
  RegisterThread();
  RegisterUnsafe();
  RegisterVector();
  RegisterWord();
}

void Primitive::Register(const char *name, word value) {
  //--** implement - use a hash table (which is another root for GC)
}

void Primitive::Register(const char *name, function value, u_int arity) {
  word abstractCode = Store::IntToWord(0); //--** this has to be revisited
  ConcreteCode *concreteCode =
    ConcreteCode::New(abstractCode, new PrimitiveInterpreter(value, arity), 0);
  Closure *closure = Closure::New(concreteCode, 0);
  Register(name, closure->ToWord());
}

//--** word Primitive::Lookup(const char *name);
