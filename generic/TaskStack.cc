//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "emulator/TaskStack.hh"
#endif

#include <cstdio>
#include "emulator/RootSet.hh"
#include "emulator/TaskStack.hh"
#include "emulator/Interpreter.hh"
#include "emulator/PushCallInterpreter.hh"
#include "emulator/Scheduler.hh"
#include "emulator/ConcreteCode.hh"

// Empty Interpreter
class EmptyTaskInterpreter : public Interpreter {
public:
  // EmptyTaskInterpreter Constructor
  EmptyTaskInterpreter() : Interpreter() {}
  // Execution
  virtual Result Run(word, TaskStack *);
  virtual Result Handle(word, word, TaskStack *);
  // Debugging
  virtual const char *Identify();
  virtual const char *ToString(word, TaskStack *);
};

Interpreter::Result EmptyTaskInterpreter::Handle(word, word, TaskStack *) {
  //--** output information about the unhandled exception
  fprintf(stderr, "uncaught exception\n");
  return Interpreter::TERMINATE;
}

Interpreter::Result EmptyTaskInterpreter::Run(word, TaskStack *) {
  return Interpreter::TERMINATE;
}

const char *EmptyTaskInterpreter::Identify() {
  return "EmptyTaskInterpreter";
}

const char *EmptyTaskInterpreter::ToString(word, TaskStack *) {
  return "EmptyTaskInterpreter::ToString";
}

word TaskStack::emptyTask;

void TaskStack::Init() {
  Interpreter *interpreter = new EmptyTaskInterpreter();
  StackFrame *frame = StackFrame::New(PRIMITIVE_FRAME, interpreter);
  emptyTask = frame->ToWord();
  RootSet::Add(emptyTask);
}

// Core PushCall Function
Interpreter::Result TaskStack::PushCall(word closure) {
  Transient *transient = Store::WordToTransient(closure);
  // Found Closure
  if (transient == INVALID_POINTER) {
    Closure *cl = Closure::FromWord(closure);
    word code = cl->GetConcreteCode();
    transient = Store::WordToTransient(code);
    // Found Code Block
    if (transient == INVALID_POINTER) {
      ConcreteCode *cc = ConcreteCode::FromWord(code);
      cc->GetInterpreter()->PushCall(this, cl);
      return Interpreter::CONTINUE;
    }
    // Code not yet available
    else {
      // Create CallFrame on top
      PushCallInterpreter::PushFrame(this, closure);
      Scheduler::currentData = transient->ToWord();
      return Interpreter::REQUEST;
    }
  }
  // Need to wait for closure
  else {
    Scheduler::currentData = transient->ToWord();
    // Create CallFrame on top
    PushCallInterpreter::PushFrame(this, Scheduler::currentData);
    return Interpreter::REQUEST;
  }
}
