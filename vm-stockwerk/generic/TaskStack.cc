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
#include "emulator/Backtrace.hh"
#include "emulator/ConcreteCode.hh"
#include "emulator/Properties.hh"
#include "emulator/Debug.hh"

void TaskStack::Dump() {
  u_int size = GetStackSize();
  for (u_int i = size; i--;) {
    word frame = GetAbsoluteArg(i);
    Interpreter *interpreter =
      StackFrame::FromWordDirect(frame)->GetInterpreter();
    interpreter->DumpFrame(frame);
  }
}

// Empty Interpreter
class EmptyTaskInterpreter : public Interpreter {
public:
  // EmptyTaskInterpreter Constructor
  EmptyTaskInterpreter() : Interpreter() {}
  // Execution
  virtual Result Run(word, TaskStack *);
  virtual Result Handle(word, Backtrace *, TaskStack *);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

Interpreter::Result EmptyTaskInterpreter::Handle(word exn, Backtrace *trace,
						 TaskStack *taskStack) {
  if (Properties::atExn == Store::IntToWord(0)) {
    fprintf(stderr, "uncaught exception:\n");
    Debug::Dump(exn);
    fprintf(stderr, "backtrace:\n");
    trace->Dump();
    exit(1);
  } else {
    return taskStack->PushCall(Properties::atExn);
  }
}

Interpreter::Result EmptyTaskInterpreter::Run(word, TaskStack *) {
  return Interpreter::TERMINATE;
}

const char *EmptyTaskInterpreter::Identify() {
  return "EmptyTaskInterpreter";
}

void EmptyTaskInterpreter::DumpFrame(word) {
  // do nothing
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
  Assert(Store::WordToInt(closure) == INVALID_INT);
  Transient *transient = Store::WordToTransient(closure);
  // Found Closure
  if (transient == INVALID_POINTER) {
    Closure *cl = Closure::FromWord(closure);
    Assert(cl != INVALID_POINTER);
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
