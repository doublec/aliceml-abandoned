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
#pragma implementation "generic/BindFutureWorker.hh"
#endif

#include <cstdio>
#include "generic/Backtrace.hh"
#include "generic/Scheduler.hh"
#include "generic/Transients.hh"
#include "generic/BindFutureWorker.hh"
#include "generic/Thread.hh"

#if DEBUGGER
#include "generic/Debugger.hh"
#include "generic/GenericDebuggerEvent.hh"

static word GenerateUnCaughtEvent() {
  GenericDebuggerEvent *event = 
    GenericDebuggerEvent::New(GenericDebuggerEvent::UNCAUGHT,
			      Scheduler::GetCurrentThread()->ToWord(),
			      Scheduler::currentData);
  return event->ToWord();
}
#endif

// BindFutureFrame
class BindFutureFrame: private StackFrame {
private:
  enum { FUTURE_POS, SIZE };
public:
  // BindFutureFrame Constructor
  static BindFutureFrame *New(Thread *thread,
			      Worker *worker, Transient *future) {
    NEW_THREAD_STACK_FRAME(frame, thread, worker, SIZE);
    frame->InitArg(FUTURE_POS, future->ToWord());
    return STATIC_CAST(BindFutureFrame *, frame);
  }
  // BindFutureFrame Accessors
  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  Future *GetFuture() {
    Transient *transient =
      Store::WordToTransient(StackFrame::GetArg(FUTURE_POS));
    Assert(transient != INVALID_POINTER &&
	   transient->GetLabel() == FUTURE_LABEL);
    return STATIC_CAST(Future *, transient);
  }
};

//
// BindFutureWorker Functions
//
BindFutureWorker *BindFutureWorker::self;

void BindFutureWorker::PushFrame(Thread *thread, Transient *future) {
  BindFutureFrame::New(thread, self, future);
  thread->PushHandler(Store::IntToWord(0));
}

static inline bool IsCyclic(word x, Future *future) {
  return STATIC_CAST(Future *, Store::WordToTransient(x)) == future;
}

u_int BindFutureWorker::GetFrameSize(StackFrame *sFrame) {
  BindFutureFrame *frame = STATIC_CAST(BindFutureFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result BindFutureWorker::Run(StackFrame *sFrame) {
  BindFutureFrame *frame = STATIC_CAST(BindFutureFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  Scheduler::PopHandler();
  Future *future = frame->GetFuture();
  Scheduler::PopFrame(frame->GetSize());
  future->ScheduleWaitingThreads();
  Construct();
  word arg = Scheduler::currentArgs[0];
  if (IsCyclic(arg, future)) { // cancel future with Cyclic exception
    future->Become(CANCELLED_LABEL, Hole::cyclicExn);
    Assert(Scheduler::nArgs == 1);
    Scheduler::currentArgs[0] = future->ToWord();
    return Worker::CONTINUE;
  } else { // actually bind the future
    future->Become(REF_LABEL, arg);
    Assert(Scheduler::nArgs == 1);
    // Scheduler::currentArgs[0] is already set to `arg'
    return Worker::CONTINUE;
  }
}

Worker::Result BindFutureWorker::Handle(word) {
#if DEBUGGER
  // Uncaught Event Generation
  if (Scheduler::GetCurrentThread()->GetDebugMode() == Thread::DEBUG) {
    word event = GenerateUnCaughtEvent();
    Debugger::SendEvent(event);
  }
#endif
  StackFrame *sFrame = Scheduler::GetFrame();
  BindFutureFrame *frame = STATIC_CAST(BindFutureFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  Future *future = frame->GetFuture();
  Scheduler::PopFrame(frame->GetSize());
  future->ScheduleWaitingThreads();
  future->Become(CANCELLED_LABEL, Scheduler::currentData);
  Scheduler::nArgs = 1;
  Scheduler::currentArgs[0] = future->ToWord();
  return Worker::CONTINUE;
}

const char *BindFutureWorker::Identify() {
  return "BindFutureWorker";
}

void BindFutureWorker::DumpFrame(StackFrame *) {
  std::fprintf(stderr, "Bind future\n");
}
