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

#ifndef __SCHEDULER__SCHEDULER_HH__
#define __SCHEDULER__SCHEDULER_HH__

#if defined(INTERFACE)
#pragma interface "scheduler/Scheduler.hh"
#endif

#include "scheduler/ThreadQueue.hh"
#include "scheduler/Thread.hh"

//
// Scheduler Interface
//

class Scheduler {
private:
  static ThreadQueue *threadQueue;
  static Thread *currentThread;
  static bool preempt;

  static void Timer();
public:
  static void Init();
  static void Run();

  static Thread *GetCurrentThread() {
    return currentThread;
  }

  static void AddThread(Thread *thread) {
    Assert(thread->GetState() == Thread::RUNNABLE);
    threadQueue->Enqueue(thread);
  }
  static void WakeupThread(Thread *thread) {
    if (thread->GetState() == Thread::BLOCKED) {
      // The thread can already have been woken up
      // if it awaited more than one future.
      thread->SetState(Thread::RUNNABLE);
      AddThread(thread);
    }
  }
  static void SuspendThread(Thread *thread) {
    //--** remove from runnable queue if it's in there
    thread->Suspend();
    thread->GetTaskStack()->Purge();
  }
  static void ResumeThread(Thread *thread) {
    if (thread->IsSuspended()) {
      thread->Resume();
      if (thread->GetState() == Thread::RUNNABLE)
	AddThread(thread);
    }
  }

  static bool TestPreempt() {
    return preempt;
  }
};

#endif __SCHEDULER__SCHEDULER_HH__
