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
#pragma implementation "generic/TaskStack.hh"
#endif

#include <cstdio>
#include "generic/RootSet.hh"
#include "generic/TaskStack.hh"
#include "generic/Interpreter.hh"
#include "generic/PushCallInterpreter.hh"
#include "generic/Scheduler.hh"
#include "generic/Backtrace.hh"
#include "generic/ConcreteCode.hh"
#include "generic/Closure.hh"
#include "generic/Properties.hh"
#include "generic/Debug.hh"

#if defined(ALICE_PROFILE)
#include "generic/Profiler.hh"
#endif

// Empty Interpreter
class EmptyTaskInterpreter : public Interpreter {
public:
  // EmptyTaskInterpreter Constructor
  EmptyTaskInterpreter() : Interpreter() {}
  // Execution
  virtual Result Run(TaskStack *);
  virtual Result Handle(word, Backtrace *, TaskStack *);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

Interpreter::Result EmptyTaskInterpreter::Run(TaskStack *) {
  Scheduler::nArgs = 0;
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
  StackFrame *frame = StackFrame::New(BOTTOM_FRAME, interpreter);
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
#if defined(ALICE_PROFILE)
      StackFrame *frame = StackFrame::FromWordDirect(GetFrame());
      Profiler::IncCalls(frame);
#endif
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

Interpreter::Result
TaskStack::PushCall(TaskStack *taskStack, word closureWord) {
  return taskStack->PushCall(closureWord);
}

void TaskStack::Purge() {
  Blank(INITIAL_SIZE);
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
