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
    Thread::state state = thread->GetState();
    if (state != Thread::TERMINATED) {
      TaskStack *otherTaskStack = thread->GetTaskStack();
      int nargs = otherTaskStack->GetInt(0);
      int frameSize = nargs == -1? 2: nargs + 1;
      otherTaskStack->PopFrame(frameSize);
      otherTaskStack->
	PushCall(Closure::FromWordDirect(GlobalPrimitives::Internal_raise));
      otherTaskStack->PushFrame(2);
      otherTaskStack->PutWord(1, x1);
      otherTaskStack->PutInt(0, 1);
      if (state == Thread::BLOCKED)
	Scheduler::WakeupThread(thread);
    }
    RETURN_UNIT;
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
  Scheduler::SuspendThread(thread);
  if (thread == Scheduler::GetCurrentThread()) {
    taskStack->PopFrame(1); // pop the Interpreter
    return Interpreter::Result(Interpreter::Result::PREEMPT, 0);
  } else {
    RETURN_UNIT;
  }
} END

DEFINE0(Thread_yield) {
  taskStack->PopFrame(1); // pop the Interpreter
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
