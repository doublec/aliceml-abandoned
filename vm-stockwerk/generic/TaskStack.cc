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
#include "generic/Worker.hh"
#include "generic/Scheduler.hh"
#include "generic/Backtrace.hh"
#include "generic/Properties.hh"
#include "generic/Debug.hh"

#if PROFILE
#include "generic/Profiler.hh"
#endif

// Empty Task Worker
class EmptyTaskWorker: public Worker {
public:
  // EmptyTaskWorker Constructor
  EmptyTaskWorker(): Worker() {}
  // Execution
  virtual Result Run();
  virtual Result Handle(word data);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

Worker::Result EmptyTaskWorker::Run() {
  Scheduler::nArgs = 0;
  return Worker::TERMINATE;
}

Worker::Result EmptyTaskWorker::Handle(word) {
  if (Properties::atExn == Store::IntToWord(0)) {
    std::fprintf(stderr, "uncaught exception:\n");
    Debug::Dump(Scheduler::currentData);
    std::fprintf(stderr, "backtrace:\n");
    Scheduler::currentBacktrace->Dump();
    return Worker::TERMINATE;
  } else {
    return Scheduler::PushCall(Properties::atExn);
  }
}

const char *EmptyTaskWorker::Identify() {
  return "EmptyTaskWorker";
}

void EmptyTaskWorker::DumpFrame(word) {
  return; // do nothing
}

// TaskStack Implementation
word TaskStack::emptyTask;

void TaskStack::Init() {
  Worker *interpreter = new EmptyTaskWorker();
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
    frame->GetWorker()->PurgeFrame(frame->ToWord());
  }
}

void TaskStack::Dump(u_int nFrames) {
  for (u_int i = nFrames; i--; ) {
    StackFrame *frame = StackFrame::FromWordDirect(GetArg(i));
    frame->GetWorker()->DumpFrame(frame->ToWord());
  }
}
