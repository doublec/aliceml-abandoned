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
#pragma implementation "emulator/Primitive.hh"
#endif

#include "emulator/Closure.hh"
#include "emulator/ConcreteCode.hh"
#include "emulator/TaskStack.hh"
#include "emulator/Scheduler.hh"
#include "emulator/StackFrame.hh"
#include "emulator/Alice.hh"
#include "emulator/Primitive.hh"

// Primitive Frame
class PrimitiveFrame : private StackFrame {
private:
  static const u_int SIZE = 0;
public:
  using Block::ToWord;
  using StackFrame::GetInterpreter;
  // PrimitiveFrame Constructor
  static PrimitiveFrame *New(Interpreter *interpreter) {
    StackFrame *frame = StackFrame::New(PRIMITIVE_FRAME, interpreter, SIZE);
    return (PrimitiveFrame *) frame;
  }
  // PrimitiveFrame Untagging
  static PrimitiveFrame *FromWord(word frame) {
    Block *p = Store::DirectWordToBlock(frame);
    Assert(p != INVALID_POINTER &&
	   p->GetLabel() == (BlockLabel) PRIMITIVE_FRAME);
    return (PrimitiveFrame *) p;
  }
};

// PrimitiveInterpreter: An interpreter that runs primitives
class PrimitiveInterpreter : public Interpreter {
private:
  Primitive::function function;
  u_int arity;
  u_int frameSize;
public:
  PrimitiveInterpreter(Primitive::function f, u_int n, u_int m):
    function(f), arity(n), frameSize(m + 1) {}
  // Frame Handling
  virtual void PushCall(TaskStack *taskStack, word closure);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual const char *ToString(word args, TaskStack *taskStack);
};

//
// PrimitiveInterpreter Functions
//
void PrimitiveInterpreter::PushCall(TaskStack *taskStack,  word closure) {
  Closure *cl = Closure::FromWord(closure);
  Assert(ConcreteCode::FromWord(cl->GetConcreteCode())->
	 GetInterpreter() == this);
  taskStack->PushFrame(PrimitiveFrame::New(this)->ToWord());
}

Interpreter::Result PrimitiveInterpreter::Run(word args, TaskStack *taskStack) {
  if (arity == 0) {
    Transient *t = Store::WordToTransient(args);
    if (t == INVALID_POINTER) {
      return function(INVALID_POINTER, taskStack);
    }
    else {
      Scheduler::currentData = args;
      return Interpreter::REQUEST;
    }
  }
  else if (arity == 1) {
    return function(Store::WordToBlock(Interpreter::Construct(args)),
		    taskStack);
  }
  else {
    word deconstructed_args = Interpreter::Deconstruct(args);
    if (deconstructed_args == Store::IntToWord(0)) {
      // Deconstruct already preset Scheduler::currentData
      return Interpreter::REQUEST;
    }
    Block *p = Store::WordToBlock(deconstructed_args);
    Assert(p->GetSize() == arity);
    return function(p, taskStack);
  }
}

const char *PrimitiveInterpreter::Identify() {
  return "PrimitiveInterpreter";
}

const char *PrimitiveInterpreter::ToString(word args, TaskStack *taskStack) {
  return "PrimitiveInterpreter::ToString";
}

//
// Primitive Functions
//
word Primitive::MakeFunction(Primitive::function value, u_int arity) {
  // to be done (transforms)
  ConcreteCode *concreteCode =
    ConcreteCode::New(new PrimitiveInterpreter(value, arity, 0), 0);
  return concreteCode->ToWord();
}
