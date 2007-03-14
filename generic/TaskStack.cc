//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2000-2003
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
#include "generic/Tuple.hh"
#include "generic/Worker.hh"
#include "generic/Scheduler.hh"
#include "generic/StackFrame.hh"
#include "generic/Backtrace.hh"
#include "generic/Debug.hh"
#include <csetjmp>

#if PROFILE
#include "generic/Profiler.hh"
#endif

// UncaughtExceptionFrame
class UncaughtExceptionFrame : public StackFrame {
protected:
  enum { EXN_POS, BACKTRACE_POS, EXN_HANDLERS_POS, SIZE };
public:
  // UncaughtExceptionFrame Accessors
  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  word GetExn() {
    return GetArg(EXN_POS);
  }
  word GetBacktrace() {
    return GetArg(BACKTRACE_POS);
  }
  word GetExnHandlers() {
    return GetArg(EXN_HANDLERS_POS);
  }
  void SetExnHandlers(word exnHandlers) {
    ReplaceArg(EXN_HANDLERS_POS, exnHandlers);
  }
  // UncaughtExceptionFrame Constructor
  static UncaughtExceptionFrame *New(word exn, word backtrace, word exnHandlers);
};

// Uncaught Exception Worker
class UncaughtExceptionWorker : public Worker {
public:
  static UncaughtExceptionWorker *self;
  // UncaughtExceptionWorker Constructor
  UncaughtExceptionWorker() : Worker() {}
  // Static constructor
  static void Init() {
    self = new UncaughtExceptionWorker();
  }
  static void PushFrame(word exn, word backtrace, word exnHandlers);
  // Frame Handling
  virtual u_int GetFrameSize(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *sFrame);
  virtual Result Handle(word data);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

UncaughtExceptionWorker *UncaughtExceptionWorker::self;

UncaughtExceptionFrame *
UncaughtExceptionFrame::New(word exn, word backtrace, word exnHandlers) {
  NEW_STACK_FRAME(frame, UncaughtExceptionWorker::self, SIZE);
  frame->InitArg(EXN_POS, exn);
  frame->InitArg(BACKTRACE_POS, backtrace);
  frame->InitArg(EXN_HANDLERS_POS, exnHandlers);
  return STATIC_CAST(UncaughtExceptionFrame *, frame);
}

void 
UncaughtExceptionWorker::PushFrame(word exn, word backtrace, word exnHandlers) {
  UncaughtExceptionFrame::New(exn, backtrace, exnHandlers);
}

u_int UncaughtExceptionWorker::GetFrameSize(StackFrame *sFrame) {
  Assert(sFrame->GetWorker() == this);
  UncaughtExceptionFrame *uncaughtExceptionFrame =
    STATIC_CAST(UncaughtExceptionFrame *, sFrame);
  return uncaughtExceptionFrame->GetSize();
}

Worker::Result UncaughtExceptionWorker::Run(StackFrame *sFrame) {
  Assert(sFrame->GetWorker() == this);
  UncaughtExceptionFrame *uncaughtExceptionFrame =
    STATIC_CAST(UncaughtExceptionFrame *, sFrame);
  word exnHandlers = uncaughtExceptionFrame->GetExnHandlers();
  if (exnHandlers == Store::IntToWord(0)) {
    Scheduler::PopFrame(uncaughtExceptionFrame->GetSize());
    Scheduler::SetNArgs(0);
    return Worker::CONTINUE;
  } else {
    Tuple *cons = Tuple::FromWordDirect(exnHandlers);
    uncaughtExceptionFrame->SetExnHandlers(cons->Sel(1));
    Scheduler::SetNArgs(2);
    Scheduler::SetCurrentArg(0, uncaughtExceptionFrame->GetExn());
    Scheduler::SetCurrentArg(1, uncaughtExceptionFrame->GetBacktrace());
    word closure = cons->Sel(0);
    return Scheduler::PushCall(closure);
  }
}

Worker::Result UncaughtExceptionWorker::Handle(word) {
  // Silently ignore exceptions caused by uncaught exception handlers
  Scheduler::SetNArgs(0);
  return Worker::CONTINUE;
}

const char *UncaughtExceptionWorker::Identify() {
  return "UncaughtExceptionWorker";
}

void UncaughtExceptionWorker::DumpFrame(StackFrame *) {
#if defined(DEBUG)
  fprintf(stderr, "Uncaught Exception\n");
#else
  return; // do nothing
#endif
}

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
  Scheduler::SetNArgs(0);
  return Worker::TERMINATE;
}

Worker::Result EmptyTaskWorker::Handle(word) {
  if (TaskStack::uncaughtExceptionClosures == Store::IntToWord(0)) {
    std::fprintf(stderr, "uncaught exception:\n");
    Debug::Dump(Scheduler::GetCurrentData());
    std::fprintf(stderr, "backtrace:\n");
    Scheduler::GetCurrentBacktrace()->Dump();
    // Flush stderr (needed for redirection on windows (e.g. rxvt))
    std::fflush(stderr);
    Scheduler::SetNArgs(0);
    return Worker::TERMINATE;
  } else {
    word exn = Scheduler::GetCurrentData();
    word backtrace = Scheduler::GetCurrentBacktrace()->ToWord();
    word exnHandlers = TaskStack::uncaughtExceptionClosures;
    UncaughtExceptionWorker::PushFrame(exn, backtrace, exnHandlers);
    Scheduler::SetNArgs(0);
    return Worker::CONTINUE;
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
word TaskStack::uncaughtExceptionClosures;
jmp_buf *TaskStack::overflowJmp = NULL;

void TaskStack::AddExnClosure(word closure) {
  Tuple *cons = Tuple::New(2);
  cons->Init(0, closure);
  cons->Init(1, uncaughtExceptionClosures);
  uncaughtExceptionClosures = cons->ToWord();
}

void TaskStack::Init() {
  Worker *interpreter = new EmptyTaskWorker();
  emptyTask  = Store::UnmanagedPointerToWord(interpreter);
  emptyStack = Store::AllocMutableBlock(MIN_DATA_LABEL, 1)->ToWord();
  RootSet::Add(emptyStack);

  UncaughtExceptionWorker::Init();
  uncaughtExceptionClosures = Store::IntToWord(0);
  RootSet::Add(uncaughtExceptionClosures);
}

void TaskStack::SetOverflowJump(jmp_buf *jmpbuf) {
  overflowJmp = jmpbuf;
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
  return STATIC_CAST(TaskStack *, b);
}

TaskStack *TaskStack::Enlarge() {
  u_int size = GetSize();
  u_int newSize = size * 3 / 2;
  if (newSize > MAX_BIGBLOCKSIZE) {
    if (overflowJmp)
      longjmp(*overflowJmp, 1);
    else {
      fprintf(stderr, "Stack limit exceeded. Aborting.\n");
      exit(2);
    }
  }
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
  word *top  = base + GetTop() - 1;
  // to be done: maybe PurgeFrame should return frame size
  while (top >= base) {
    StackFrame *frame = (StackFrame *) top;
    Worker *worker = frame->GetWorker();
    top -= worker->GetFrameSize(frame);
    worker->PurgeFrame(frame);
  }
}

void TaskStack::Dump(u_int stackTop) {
  word *base = (word *) GetFrame(0);
  word *top  = base + stackTop - 1;
  while (top >= base) {
    StackFrame *frame = (StackFrame *) top;
    Worker *worker = frame->GetWorker();
    top -= worker->GetFrameSize(frame);
    worker->DumpFrame(frame);
  }
}
