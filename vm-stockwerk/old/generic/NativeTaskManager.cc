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
public:
  // Handling code:
  virtual ConcreteCode *Prepare(word abstractCode) {
    Assert(0);
    return NULL;
  }

  // Handling stack frames:
  void PushCall(TaskStack *taskStack, word closure); //--** implement
  void PopFrame(TaskStack *taskStack); //--** implement

  // Execution:
  Result Run(TaskStack *taskStack, int nargs); //--** implement
};

static PrimitiveInterpreter *primitiveInterpreter = new PrimitiveInterpreter();

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

//--** void Primitive::Register(const char *name, word value);

void Primitive::Register(const char *name, builtin value) {
  word abstractCode = Store::IntToWord(0); //--** this has to be revisited
  ConcreteCode *concreteCode =
    ConcreteCode::New(abstractCode, primitiveInterpreter, 1);
  concreteCode->
    Init(0, Store::FunctionPointerToWord(reinterpret_cast<int>(value)));
  Closure *closure = Closure::New(concreteCode, Vector::New(0));
  Register(name, closure->ToWord());
};

//--** word Primitive::Lookup(const char *name);
