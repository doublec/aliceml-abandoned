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

#include "ThreadPool.hh"

//
// Scheduler Interface
//

class Scheduler {
private:
  static ThreadPool *threadPool;
  static bool preempt;

  static void Timer();
public:
  static void Run();

  static void AddThread(Thread *thread) {
    threadPool->Enqueue(thread);
  }
  static bool TestPreempt() {
    return preempt;
  }
};

#endif
