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

#include "scheduler/Closure.hh"
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
	otherTaskStack->
	  PushCall(Closure::FromWordDirect(GlobalPrimitives::Internal_raise));
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
  //--** add to runnable queue if applicable
  DECLARE_THREAD(thread, x0);
  thread->Resume();
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
  //--** remove from runnable queue if it's in there
  //--** check if argument is current thread
  DECLARE_THREAD(thread, x0);
  thread->Suspend();
  thread->GetTaskStack()->Purge();
  RETURN_UNIT;
} END

DEFINE0(Thread_yield) {
  return Interpreter::Result(Interpreter::Result::PREEMPT, 0);
} END

void Primitive::RegisterThread() {
  RegisterUniqueConstructor("Thread.Terminate");
  Register("Thread.current", Thread_current, 0);
  Register("Thread.isSuspended", Thread_isSuspended, 1);
  Register("Thread.raiseIn", Thread_raiseIn, 2);
  Register("Thread.resume", Thread_resume, 1);
  Register("Thread.state", Thread_state, 1);
  Register("Thread.suspend", Thread_suspend, 1);
  Register("Thread.yield", Thread_yield, 0);
}
