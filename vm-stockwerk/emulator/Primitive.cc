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
#include "emulator/RootSet.hh"
#include "emulator/StackFrame.hh"
#include "emulator/Alice.hh"
#include "emulator/Primitive.hh"
#include "emulator/Transform.hh"

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
  bool sited;
public:
  PrimitiveInterpreter(const char *s, Primitive::function f,
		       u_int n, u_int m, bool local):
    name(s), function(f), arity(n), frameSize(m + 1), sited(local) {}
  // Handler Methods
  virtual Block *GetAbstractRepresentation(Block *blockWithHandler);
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
Block *
PrimitiveInterpreter::GetAbstractRepresentation(Block *blockWithHandler) {
  if (sited) {
    return INVALID_POINTER;
  }
  else {
    ConcreteCode *concreteCode = static_cast<ConcreteCode *>(blockWithHandler);
    return Store::DirectWordToBlock(concreteCode->Get(0));
  }
}

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
word Primitive::aliceTransformName;

word Primitive::MakeFunction(const char *name,
			     Primitive::function value, u_int arity, bool s) {
  // to be done (transforms)
  ConcreteCode *concreteCode =
    ConcreteCode::New(new PrimitiveInterpreter(name, value, arity, 0, s), 1);
  Transform *transform =
    Transform::New(Store::DirectWordToChunk(aliceTransformName),
		   String::New(name)->ToWord());
  concreteCode->Init(0, transform->ToWord());
  return concreteCode->ToWord();
}

word Primitive::MakeClosure(const char *name, 
			    Primitive::function value, u_int arity, bool s) {
  word concreteCode = MakeFunction(name, value, arity, s);
  return Closure::New(concreteCode, 0)->ToWord();
}

void Primitive::Init() {
  RootSet::Add(aliceTransformName);
  aliceTransformName = String::New("Alice.primitive.function")->ToWord();
}
