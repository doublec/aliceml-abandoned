//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstdio>
#include "generic/Closure.hh"
#include "generic/Thread.hh"
#include "generic/Scheduler.hh"
#include "generic/StackFrame.hh"
#include "generic/Transients.hh"
#include "alice/Authoring.hh"

class RaiseFrame: private StackFrame {
private:
  enum { EXN_POS, SIZE };
public:
  using StackFrame::ToWord;
  // RaiseFrame Accessors
  word GetExn() {
    return GetArg(EXN_POS);
  }
  // RaiseFrame Constructor
  static RaiseFrame *New(Worker *worker, word exn) {
    StackFrame *frame = StackFrame::New(RAISE_FRAME, worker, SIZE);
    frame->InitArg(EXN_POS, exn);
    return static_cast<RaiseFrame *>(frame);
  }
  // VectorTabulateFrame Untagging
  static RaiseFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == RAISE_FRAME);
    return static_cast<RaiseFrame *>(p);
  }
};

class RaiseWorker: public Worker {
private:
  static RaiseWorker *self;
public:
  // RaiseWorker Constructor
  RaiseWorker(): Worker() {}
  // RaiseWorker Static Constructor
  static void Init() {
    self = new RaiseWorker();
  }
  // Frame Handling
  static void PushFrame(Thread *thread, word exn) {
    thread->PushFrame(RaiseFrame::New(self, exn)->ToWord());
  }
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// RaiseWorker Functions
//
RaiseWorker *RaiseWorker::self;

Worker::Result RaiseWorker::Run() {
  RaiseFrame *frame = RaiseFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  Scheduler::currentData = frame->GetExn();
  Scheduler::currentBacktrace = Backtrace::New(frame->ToWord());
  return Worker::RAISE;
}

const char *RaiseWorker::Identify() {
  return "RaiseWorker";
}

void RaiseWorker::DumpFrame(word) {
  fprintf(stderr, "Raise\n");
}

// Builtins

DEFINE0(Thread_current) {
  RETURN(Scheduler::GetCurrentThread()->ToWord());
} END

DEFINE1(Thread_isSuspended) {
  DECLARE_THREAD(thread, x0);
  RETURN_BOOL(thread->IsSuspended());
} END

DEFINE2(Thread_raiseIn) {
  DECLARE_THREAD(thread, x0);
  if (thread == Scheduler::GetCurrentThread()) {
    RAISE(x1);
  } else {
    Thread::state state = thread->GetState();
    if (state == Thread::TERMINATED) {
      RAISE(PrimitiveTable::Thread_Terminated);
    } else {
      RaiseWorker::PushFrame(thread, x1);
      thread->SetArgs(0, Store::IntToWord(0));
      if (state == Thread::BLOCKED) {
	Future *future = static_cast<Future *>
	  (Store::WordToTransient(thread->GetFuture()));
	Assert(future != INVALID_POINTER);
	future->RemoveFromWaitQueue(thread);
	Scheduler::WakeupThread(thread);
      }
      RETURN_UNIT;
    }
  }
} END

DEFINE1(Thread_resume) {
  DECLARE_THREAD(thread, x0);
  Scheduler::ResumeThread(thread);
  RETURN_UNIT;
} END

DEFINE1(Thread_state) {
  DECLARE_THREAD(thread, x0);
  switch (thread->GetState()) {
  case Thread::BLOCKED:
    RETURN_INT(0);
  case Thread::RUNNABLE:
    RETURN_INT(1);
  case Thread::TERMINATED:
    RETURN_INT(2);
  }
  Error("Thread_state: unknown state returned by Thread::GetState");
} END

DEFINE1(Thread_suspend) {
  DECLARE_THREAD(thread, x0);
  if (thread == Scheduler::GetCurrentThread()) {
    Scheduler::nArgs = 0;
    SUSPEND;
  } else {
    Scheduler::SuspendThread(thread);
    RETURN_UNIT;
  }
} END

DEFINE0(Thread_yield) {
  PREEMPT0;
} END

void PrimitiveTable::RegisterThread() {
  RaiseWorker::Init();
  PrimitiveTable::Thread_Terminated =
    UniqueConstructor::New(String::New("Thread.Terminated"))->ToWord();
  RegisterUniqueConstructor("Thread.Terminate");
  Register("Thread.Terminated", PrimitiveTable::Thread_Terminated);
  Register("Thread.current", Thread_current, 0);
  Register("Thread.isSuspended", Thread_isSuspended, 1);
  Register("Thread.raiseIn", Thread_raiseIn, 2);
  Register("Thread.resume", Thread_resume, 1);
  Register("Thread.state", Thread_state, 1);
  Register("Thread.suspend", Thread_suspend, 1);
  Register("Thread.yield", Thread_yield, 0);
}
