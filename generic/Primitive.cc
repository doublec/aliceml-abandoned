//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "generic/Primitive.hh"
#endif

#include <cstdio>
#include "generic/Closure.hh"
#include "generic/ConcreteCode.hh"
#include "generic/Scheduler.hh"
#include "generic/RootSet.hh"
#include "generic/StackFrame.hh"
#include "generic/Transform.hh"
#include "generic/Primitive.hh"
#include "alice/Data.hh"
#include "alice/AliceLanguageLayer.hh"

// Primitive Frame
class PrimitiveFrame: private StackFrame {
private:
  enum { SIZE };
public:
  using Block::ToWord;
  using StackFrame::GetInterpreter;
  // PrimitiveFrame Constructor
  static PrimitiveFrame *New(Interpreter *interpreter) {
    StackFrame *frame = StackFrame::New(PRIMITIVE_FRAME, interpreter, SIZE);
    return static_cast<PrimitiveFrame *>(frame);
  }
  // PrimitiveFrame Untagging
  static PrimitiveFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == PRIMITIVE_FRAME);
    return static_cast<PrimitiveFrame *>(p);
  }
};

// PrimitiveInterpreter: An interpreter that runs primitives
class PrimitiveInterpreter: public Interpreter {
private:
  const char *name;
  Interpreter::function function;
  word frame;
  u_int arity;
  bool sited;
public:
  PrimitiveInterpreter(const char *_name, Interpreter::function _function,
		       u_int _arity, bool _sited):
    name(_name), function(_function), arity(_arity), sited(_sited) {
    frame = PrimitiveFrame::New(this)->ToWord();
    RootSet::Add(frame);
  }
  Interpreter::function GetFunction() {
    return function;
  }
  word GetFrame() {
    return frame;
  }
  static Interpreter::Result Run(PrimitiveInterpreter *interpreter);
  // Handler Methods
  virtual Block *GetAbstractRepresentation(Block *blockWithHandler);
  // Frame Handling
  virtual void PushCall(Closure *closure);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
  // Runtime compilation
  virtual u_int GetArity();
  virtual Interpreter::function GetCFunction();
};

//
// PrimitiveInterpreter Functions
//
inline Interpreter::Result
PrimitiveInterpreter::Run(PrimitiveInterpreter *interpreter) {
  switch (interpreter->arity) {
  case 0:
    if (Scheduler::nArgs == Scheduler::ONE_ARG) {
      Transient *t = Store::WordToTransient(Scheduler::currentArgs[0]);
      if (t == INVALID_POINTER) { // is determined
	return interpreter->function();
      } else { // need to request
	Scheduler::currentData = Scheduler::currentArgs[0];
	return Interpreter::REQUEST;
      }
    }
  case 1:
    interpreter->Construct();
    return interpreter->function();
  default:
    if (interpreter->Deconstruct()) {
      // Deconstruct has set Scheduler::currentData as a side-effect
      return Interpreter::REQUEST;
    } else {
      Assert(Scheduler::nArgs == interpreter->arity);
      return interpreter->function();
    }
  }
}

Block *
PrimitiveInterpreter::GetAbstractRepresentation(Block *blockWithHandler) {
  if (sited) {
    return INVALID_POINTER;
  } else {
    ConcreteCode *concreteCode = static_cast<ConcreteCode *>(blockWithHandler);
    return Store::DirectWordToBlock(concreteCode->Get(0));
  }
}

void PrimitiveInterpreter::PushCall(Closure *closure) {
  Assert(ConcreteCode::FromWord(closure->GetConcreteCode())->
	 GetInterpreter() == this); closure = closure;
  Scheduler::PushFrame(GetFrame());
}

Interpreter::Result PrimitiveInterpreter::Run() {
  return Run(this);
}

const char *PrimitiveInterpreter::Identify() {
  return name? name: "PrimitiveInterpreter";
}

void PrimitiveInterpreter::DumpFrame(word) {
  if (name)
    std::fprintf(stderr, "Primitive %s\n", name);
  else
    std::fprintf(stderr, "Primitive\n");
}

u_int PrimitiveInterpreter::GetArity() {
  return arity;
}

Interpreter::function PrimitiveInterpreter::GetCFunction() {
  return GetFunction();
}
//
// Primitive Functions
//

word Primitive::MakeFunction(const char *name, Interpreter::function function,
			     u_int arity, bool sited) {
  PrimitiveInterpreter *interpreter =
    new PrimitiveInterpreter(name, function, arity, sited);
  ConcreteCode *concreteCode = ConcreteCode::New(interpreter, 1);
  //--** use a more direct representation and avoid Alice dependency:
  TagVal *tagVal = TagVal::New(0, 1);
  tagVal->Init(0, String::New(name)->ToWord());
  word transformName = AliceLanguageLayer::TransformNames::primitiveFunction;
  Transform *transform =
    Transform::New(Store::DirectWordToChunk(transformName), tagVal->ToWord());
  concreteCode->Init(0, transform->ToWord());
  return concreteCode->ToWord();
}

word Primitive::MakeClosure(const char *name, Interpreter::function function,
			    u_int arity, bool sited) {
  word concreteCode = MakeFunction(name, function, arity, sited);
  return Closure::New(concreteCode, 0)->ToWord();
}

Interpreter::Result Primitive::Execute(Interpreter *interpreter) {
  PrimitiveInterpreter *primitive =
    static_cast<PrimitiveInterpreter *>(interpreter);
  Scheduler::PushFrame(primitive->GetFrame());
  return PrimitiveInterpreter::Run(primitive);
}

Interpreter::Result Primitive::ExecuteNoCCC(Interpreter *interpreter) {
  PrimitiveInterpreter *primitive =
    static_cast<PrimitiveInterpreter *>(interpreter);
  Scheduler::PushFrame(primitive->GetFrame());
  Interpreter::function function = primitive->GetFunction();
  return function();
}
