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

// BindFutureFrame
class BindFutureFrame: private StackFrame {
private:
  enum { FUTURE_POS, SIZE };
public:
  using Block::ToWord;

  // BindFutureFrame Constructor
  static BindFutureFrame *New(Worker *worker, Transient *future) {
    StackFrame *frame = StackFrame::New(BYNEED_FRAME, worker, SIZE);
    frame->InitArg(FUTURE_POS, future->ToWord());
    return static_cast<BindFutureFrame *>(frame);
  }
  // BindFutureFrame Untagging
  static BindFutureFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == BYNEED_FRAME);
    return static_cast<BindFutureFrame *>(p);
  }

  // BindFutureFrame Accessors
  Future *GetFuture() {
    Transient *transient =
      Store::WordToTransient(StackFrame::GetArg(FUTURE_POS));
    Assert(transient != INVALID_POINTER &&
	   transient->GetLabel() == FUTURE_LABEL);
    return static_cast<Future *>(transient);
  }
};

//
// BindFutureWorker Functions
//
BindFutureWorker *BindFutureWorker::self;

void BindFutureWorker::PushFrame(Thread *thread, Transient *future) {
  thread->PushFrame(BindFutureFrame::New(self, future)->ToWord());
  thread->PushHandler(Store::IntToWord(0));
}

static inline bool IsCyclic(word x, Future *future) {
  return static_cast<Future *>(Store::WordToTransient(x)) == future;
}

Worker::Result BindFutureWorker::Run() {
  BindFutureFrame *frame =
    BindFutureFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  Scheduler::PopHandler();
  Future *future = frame->GetFuture();
  future->ScheduleWaitingThreads();
  Construct();
  word arg = Scheduler::currentArgs[0];
  if (IsCyclic(arg, future)) { // cancel future with Cyclic exception
    future->Become(CANCELLED_LABEL, Hole::cyclicExn);
    Assert(Scheduler::nArgs == Scheduler::ONE_ARG);
    Scheduler::currentArgs[0] = future->ToWord();
    return Worker::CONTINUE;
  } else { // actually bind the future
    future->Become(REF_LABEL, arg);
    Assert(Scheduler::nArgs == Scheduler::ONE_ARG);
    // Scheduler::currentArgs[0] is already set to `arg'
    return Worker::CONTINUE;
  }
}

Worker::Result BindFutureWorker::Handle(word) {
  Future *future =
    BindFutureFrame::FromWordDirect(Scheduler::GetAndPopFrame())->GetFuture();
  future->ScheduleWaitingThreads();
  future->Become(CANCELLED_LABEL, Scheduler::currentData);
  Scheduler::nArgs = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = future->ToWord();
  return Worker::CONTINUE;
}

const char *BindFutureWorker::Identify() {
  return "BindFutureWorker";
}

void BindFutureWorker::DumpFrame(word) {
  std::fprintf(stderr, "Bind future\n");
}
