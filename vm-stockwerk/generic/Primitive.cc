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

#include <cstdio>
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
  const char *name;
  Primitive::function function;
  u_int arity;
  u_int frameSize;
public:
  PrimitiveInterpreter(const char *s, Primitive::function f,
		       u_int n, u_int m):
    name(s), function(f), arity(n), frameSize(m + 1) {}
  // Frame Handling
  virtual void PushCall(TaskStack *taskStack, Closure *closure);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// PrimitiveInterpreter Functions
//
void PrimitiveInterpreter::PushCall(TaskStack *taskStack, Closure *closure) {
  Assert(ConcreteCode::FromWord(closure->GetConcreteCode())->
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
    return function(Interpreter::Construct(args), taskStack);
  }
  else {
    word deconstructed_args = Interpreter::Deconstruct(args);
    if (deconstructed_args == Store::IntToWord(0)) {
      // Deconstruct already preset Scheduler::currentData
      return Interpreter::REQUEST;
    }
    Assert(Store::WordToBlock(deconstructed_args)->GetSize() == arity);
    return function(deconstructed_args, taskStack);
  }
}

const char *PrimitiveInterpreter::Identify() {
  return name? name: "PrimitiveInterpreter";
}

void PrimitiveInterpreter::DumpFrame(word) {
  if (name)
    fprintf(stderr, "Primitive %s\n", name);
  else
    fprintf(stderr, "Primitive\n");
}

//
// Primitive Functions
//
word Primitive::MakeFunction(const char *name,
			     Primitive::function value, u_int arity) {
  // to be done (transforms)
  ConcreteCode *concreteCode =
    ConcreteCode::New(new PrimitiveInterpreter(name, value, arity, 0), 0);
  return concreteCode->ToWord();
}

word Primitive::MakeClosure(const char *name, 
			    Primitive::function value, u_int arity) {
  word concreteCode = MakeFunction(name, value, arity);
  return Closure::New(concreteCode, 0)->ToWord();
}

word Primitive::MakeClosure(Primitive::function value, u_int arity) {
  return MakeClosure(NULL, value, arity);
}
