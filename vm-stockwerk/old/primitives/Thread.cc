//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "scheduler/Thread.hh"
#include "scheduler/Scheduler.hh"
#include "builtins/Authoring.hh"

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
    switch (thread->GetState()) {
    case Thread::BLOCKED:
      Scheduler::AddThread(thread);
      // fall through
    case Thread::RUNNABLE:
      {
	TaskStack *otherTaskStack = thread->GetTaskStack();
	int nargs = otherTaskStack->GetInt(0);
	int frameSize = nargs == -1? 2: nargs + 1;
	otherTaskStack->PopFrame(frameSize);
	Closure::FromWord(GlobalPrimitives::Internal_raise)->
	  PushCall(otherTaskStack);
	otherTaskStack->PushFrame(1);
	otherTaskStack->PutWord(0, x1);
	break;
      }
    case Thread::TERMINATED:
      break;
    }
  }
  RETURN_UNIT;
} END

DEFINE1(Thread_resume) {
  DECLARE_THREAD(thread, x0);
  thread->Resume();
  RETURN_UNIT;
} END

DEFINE1(Thread_state) {
  DECLARE_THREAD(thread, x0);
  switch (thread->GetState()) {
  case Thread::BLOCKED:
    RETURN(Store::IntToWord(0));
  case Thread::RUNNABLE:
    RETURN(Store::IntToWord(1));
  case Thread::TERMINATED:
    RETURN(Store::IntToWord(2));
  default:
    Assert(0);
    break;
  };
} END

DEFINE1(Thread_suspend) {
  DECLARE_THREAD(thread, x0);
  thread->Resume();
  RETURN_UNIT;
} END

DEFINE0(Thread_yield) {
  out = Store::IntToWord(0);
  return Interpreter::PREEMPT;
} END

void Primitive::RegisterThread() {
  Register("Thread.Terminate", Constructor::New()->ToWord()); //--** unique
  Register("Thread.current", Thread_current);
  Register("Thread.isSuspended", Thread_isSuspended);
  Register("Thread.raiseIn", Thread_raiseIn);
  Register("Thread.resume", Thread_resume);
  Register("Thread.state", Thread_state);
  Register("Thread.suspend", Thread_suspend);
  Register("Thread.yield", Thread_yield);
};
