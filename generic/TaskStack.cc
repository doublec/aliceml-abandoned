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
#include "store/Store.hh"
#include "store/GCHelper.hh"
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
  // Frame Handling
  virtual u_int GetFrameSize(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *sFrame);
  virtual Result Handle(word data);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

u_int EmptyTaskWorker::GetFrameSize(StackFrame *sFrame) {
  Assert(sFrame->GetWorker() == this);
  return sFrame->GetSize();
}

Worker::Result EmptyTaskWorker::Run(StackFrame *sFrame) {
  Assert(sFrame->GetWorker() == this);
  sFrame = sFrame; // Ignored
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

void EmptyTaskWorker::DumpFrame(StackFrame *) {
#if defined(DEBUG)
  fprintf(stderr, "Empty Task\n");
#else
  return; // do nothing
#endif
}

// TaskStack Implementation
word TaskStack::emptyTask;
word TaskStack::emptyStack;

void TaskStack::Init() {
  Worker *interpreter = new EmptyTaskWorker();
  emptyTask  = Store::UnmanagedPointerToWord(interpreter);
  emptyStack = Store::AllocBlock(MIN_DATA_LABEL, 1)->ToWord();
  RootSet::Add(emptyStack);
}

void TaskStack::SetTop(u_int top) {
  SetActiveSize(top);
  Block *p = (Block *) this;
  if (!HeaderOp::IsChildish(p))
    Store::AddToIntgenSet(p);
}

TaskStack *TaskStack::New(u_int size) {
  Assert(size >= 4); // required for Enlarge to work correctly
  DynamicBlock *b = Store::AllocDynamicBlock(size, 1);
  // Create Empty Task
  b->InitArg(0, emptyTask);
  Store::AddToIntgenSet((Block *) b);
  return static_cast<TaskStack *>(b);
}

TaskStack *TaskStack::Enlarge() {
  u_int size = GetSize();
  u_int newSize = size * 3 / 2;
  TaskStack *newTaskStack = TaskStack::New(newSize);
  std::memcpy(newTaskStack->GetBase(), GetBase(), size * sizeof(u_int));
  Block *p = (Block *) this;
  // Prevent scanning of old stack
  if (HeaderOp::IsChildish(p))
    GCHelper::MarkMoved(p, Store::DirectWordToBlock(emptyStack));
  return newTaskStack;
}

void TaskStack::Purge() {
  // Shrink stack to a reasonable size:
  // to be done: find policy here (when to shrink grown stacks)
  // Purge all frames:
  word *base = (word *) GetFrame(0);
  word *top  = base + GetTop();
  // to be done: maybe PurgeFrame should return frame size
  while (top > base) {
    StackFrame *frame = (StackFrame *) (top - 1);
    Worker *worker = frame->GetWorker();
    top -= worker->GetFrameSize(frame);
    worker->PurgeFrame(frame);
  }
}

void TaskStack::Dump(u_int stackTop) {
  word *base = (word *) GetFrame(0);
  word *top  = base + stackTop;
  while (top > base) {
    StackFrame *frame = (StackFrame *) (top - 1);
    Worker *worker = frame->GetWorker();
    top -= worker->GetFrameSize(frame);
    worker->DumpFrame(frame);
  }
}
