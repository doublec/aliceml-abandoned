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

// Primitive Frame
class PrimitiveFrame: private StackFrame {
private:
  enum { SIZE };
public:
  using Block::ToWord;

  // PrimitiveFrame Constructor
  static PrimitiveFrame *New(Worker *worker) {
    StackFrame *frame = StackFrame::New(PRIMITIVE_FRAME, worker, SIZE);
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
  static Result Run(PrimitiveInterpreter *interpreter);
  // Handler Methods
  virtual Transform *GetAbstractRepresentation(ConcreteRepresentation *);
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
inline Worker::Result
PrimitiveInterpreter::Run(PrimitiveInterpreter *interpreter) {
  switch (interpreter->arity) {
  case 0:
    if (Scheduler::nArgs == Scheduler::ONE_ARG) {
      Transient *t = Store::WordToTransient(Scheduler::currentArgs[0]);
      if (t == INVALID_POINTER) { // is determined
	Scheduler::nArgs = 0;
	return interpreter->function();
      } else { // need to request
	Scheduler::currentData = Scheduler::currentArgs[0];
	return Worker::REQUEST;
      }
    } else {
      Assert(Scheduler::nArgs == 0);
      return interpreter->function();
    }
  case 1:
    Construct();
    return interpreter->function();
  default:
    if (Deconstruct()) {
      // Deconstruct has set Scheduler::currentData as a side-effect
      return Worker::REQUEST;
    } else {
      Assert(Scheduler::nArgs == interpreter->arity);
      return interpreter->function();
    }
  }
}

Transform *
PrimitiveInterpreter::GetAbstractRepresentation(ConcreteRepresentation *b) {
  if (sited) {
    return INVALID_POINTER;
  } else {
    ConcreteCode *concreteCode = static_cast<ConcreteCode *>(b);
    return Transform::FromWordDirect(concreteCode->Get(0));
  }
}

void PrimitiveInterpreter::PushCall(Closure *closure) {
  Assert(ConcreteCode::FromWord(closure->GetConcreteCode())->
	 GetInterpreter() == this); closure = closure;
  Scheduler::PushFrame(GetFrame());
}

Worker::Result PrimitiveInterpreter::Run() {
  return Run(this);
}

const char *PrimitiveInterpreter::Identify() {
  return "PrimitiveInterpreter";
}

void PrimitiveInterpreter::DumpFrame(word) {
  std::fprintf(stderr, "%s\n", name);
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
word Primitive::MakeFunction(Transform *abstract, const char *name,
			     Interpreter::function function,
			     u_int arity, bool sited) {
  PrimitiveInterpreter *interpreter =
    new PrimitiveInterpreter(name, function, arity, sited);
  ConcreteCode *concreteCode = ConcreteCode::New(interpreter, 1);
  concreteCode->Init(0, abstract->ToWord());
  return concreteCode->ToWord();
}

Worker::Result Primitive::Execute(Interpreter *interpreter) {
  PrimitiveInterpreter *primitive =
    static_cast<PrimitiveInterpreter *>(interpreter);
  Scheduler::PushFrame(primitive->GetFrame());
  Interpreter::function function = primitive->GetFunction();
  return function();
}
