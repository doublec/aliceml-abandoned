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
#include "emulator/Closure.hh"
#include "emulator/Properties.hh"
#include "emulator/Debug.hh"

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

Interpreter::Result EmptyTaskInterpreter::Run(word, TaskStack *) {
  return Interpreter::TERMINATE;
}

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

const char *EmptyTaskInterpreter::Identify() {
  return "EmptyTaskInterpreter";
}

void EmptyTaskInterpreter::DumpFrame(word) {
  return; // do nothing
}

// TaskStack Implementation
word TaskStack::emptyTask;

void TaskStack::Init() {
  Interpreter *interpreter = new EmptyTaskInterpreter();
  StackFrame *frame = StackFrame::New(PRIMITIVE_FRAME, interpreter);
  emptyTask = frame->ToWord();
  RootSet::Add(emptyTask);
}

Interpreter::Result TaskStack::PushCall(word closureWord) {
  Assert(Store::WordToInt(closureWord) == INVALID_INT);
  Transient *transient = Store::WordToTransient(closureWord);
  if (transient == INVALID_POINTER) { // Closure is determined
    Closure *closure = Closure::FromWord(closureWord);
    Assert(closure != INVALID_POINTER);
    word concreteCodeWord = closure->GetConcreteCode();
    transient = Store::WordToTransient(concreteCodeWord);
    if (transient == INVALID_POINTER) { // ConcreteCode is determined
      ConcreteCode *concreteCode = ConcreteCode::FromWord(concreteCodeWord);
      Assert(concreteCode != INVALID_POINTER);
      concreteCode->GetInterpreter()->PushCall(this, closure);
      return Interpreter::CONTINUE;
    } else { // Request ConcreteCode
      PushCallInterpreter::PushFrame(this, closureWord);
      Scheduler::currentData = transient->ToWord();
      return Interpreter::REQUEST;
    }
  } else { // Request Closure
    PushCallInterpreter::PushFrame(this, closureWord);
    Scheduler::currentData = transient->ToWord();
    return Interpreter::REQUEST;
  }
}

void TaskStack::Purge() {
  u_int size = GetStackSize();
  for (u_int i = size; i--; ) {
    word frame = GetAbsoluteArg(i);
    Interpreter *interpreter =
      StackFrame::FromWordDirect(frame)->GetInterpreter();
    interpreter->PurgeFrame(frame);
  }
}

void TaskStack::Dump() {
  u_int size = GetStackSize();
  for (u_int i = size; i--; ) {
    word frame = GetAbsoluteArg(i);
    Interpreter *interpreter =
      StackFrame::FromWordDirect(frame)->GetInterpreter();
    interpreter->DumpFrame(frame);
  }
}
