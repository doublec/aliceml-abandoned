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

#include "builtins/Primitive.hh"

class PrimitiveInterpreter: public Interpreter {
private:
  Primitive::function function;
  u_int arity;
public:
  PrimitiveInterpreter(Primitive::function f, u_int n):
    function(f), arity(n == 1? -1: n) {}

  ConcreteCode *Prepare(word abstractCode);
  void PushCall(TaskStack *taskStack, word closure);
  void PopFrame(TaskStack *taskStack);
  Result Run(TaskStack *taskStack, int nargs);
};

ConcreteCode *PrimitiveInterpreter::Prepare(word abstractCode) {
  Error("PrimitiveInterpreter::Prepare must never be called");
}

void PrimitiveInterpreter::PushCall(TaskStack *taskStack, word w) {
  Closure *closure = Closure::FromWord(w);
  Assert(closure->GetConcreteCode()->GetInterpreter() == this);
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
      //--** deconstruct one tuple from the task stack to (arity) arguments
    } else {
      //--** construct (nargs) arguments from the task stack into one tuple
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
    ConcreteCode::New(abstractCode, new PrimitiveInterpreter(value, arity), 1);
  //--** this reinterpret_cast breaks if sizeof(int) != sizeof(function):
  concreteCode->
    Init(0, Store::FunctionPointerToWord(reinterpret_cast<int>(value)));
  Closure *closure = Closure::New(concreteCode, Vector::New(0));
  Register(name, closure->ToWord());
};

//--** word Primitive::Lookup(const char *name);
