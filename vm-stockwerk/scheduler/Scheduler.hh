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

#ifndef __SCHEDULER_HH__
#define __SCHEDULER_HH__

#include "scheduler/ThreadPool.hh"
#include "scheduler/Thread.hh"

//
// Scheduler Interface
//

class Scheduler {
private:
  static StoreConfig *storeConfig; //--** probably not the correct place
  static ThreadPool *threadPool;
  static Thread *currentThread;
  static bool preempt;

  static void Timer();
public:
  static void Run();

  static void AddThread(Thread *thread) {
    if (thread->GetState() != Thread::BLOCKED) {
      // The thread could also be TERMINATED or RUNNABLE because
      // it had been in the wait queues of several transients.
      thread->SetState(Thread::RUNNABLE);
      threadPool->Enqueue(thread);
    }
  }
  static Thread *GetCurrentThread() {
    return currentThread;
  }

  static bool TestPreempt() {
    return preempt;
  }
};

#endif
