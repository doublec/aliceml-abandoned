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
#include "generic/Scheduler.hh"
#include "generic/Backtrace.hh"
#include "generic/Properties.hh"
#include "generic/Debug.hh"

#if PROFILE
#include "generic/Profiler.hh"
#endif

// Empty Interpreter
class EmptyTaskInterpreter: public Interpreter {
public:
  // EmptyTaskInterpreter Constructor
  EmptyTaskInterpreter(): Interpreter() {}
  // Execution
  virtual Result Run();
  virtual Result Handle();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

Interpreter::Result EmptyTaskInterpreter::Run() {
  Scheduler::nArgs = 0;
  return Interpreter::TERMINATE;
}

Interpreter::Result EmptyTaskInterpreter::Handle() {
  if (Properties::atExn == Store::IntToWord(0)) {
    fprintf(stderr, "uncaught exception:\n");
    Debug::Dump(Scheduler::currentData);
    fprintf(stderr, "backtrace:\n");
    Scheduler::currentBacktrace->Dump();
    exit(1);
  } else {
    return Scheduler::PushCall(Properties::atExn);
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

TaskStack *TaskStack::Enlarge() {
  u_int size = GetSize();
  u_int newSize = size * 3 / 2;
  TaskStack *newTaskStack = TaskStack::New(newSize);
  std::memcpy(newTaskStack->GetBase(), GetBase(), size * sizeof(u_int));
  return newTaskStack;
}

void TaskStack::Purge(u_int nFrames) {
  // Shrink stack to a reasonable size:
  u_int size = GetSize();
  Assert(nFrames <= size);
  u_int newSize = nFrames + INITIAL_SIZE;
  if (newSize < size) {
    size = newSize;
    HeaderOp::EncodeSize(this, size);
  }
  for (u_int i = nFrames; i < size; i++)
    InitArg(i, 0);
  // Purge all frames:
  for (u_int i = nFrames; i--; ) {
    StackFrame *frame = StackFrame::FromWordDirect(GetArg(i));
    frame->GetInterpreter()->PurgeFrame(frame->ToWord());
  }
}

void TaskStack::Dump(u_int nFrames) {
  for (u_int i = nFrames; i--; ) {
    StackFrame *frame = StackFrame::FromWordDirect(GetArg(i));
    frame->GetInterpreter()->DumpFrame(frame->ToWord());
  }
}
