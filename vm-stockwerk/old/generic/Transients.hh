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

#ifndef __SCHEDULER__TRANSIENTS_HH__
#define __SCHEDULER__TRANSIENTS_HH__

#if defined(INTERFACE)
#pragma interface "scheduler/Transients.hh"
#endif

#include "store/Store.hh"
#include "scheduler/Scheduler.hh"
#include "adt/Queue.hh"

//
// Transient Representation:
//
//    Label             Argument
//    -----             --------
//    HOLE_LABEL        associated future (or an integer if not created)
//    FUTURE_LABEL      wait queue (or an integer if not created)
//    CANCELLED_LABEL   exception
//    BYNEED_LABEL      closure
//    REF_LABEL         value
//

class Future: private Transient {
public:
  using Transient::ToWord;

  static Future *New() {
    Transient *transient = Store::AllocTransient(FUTURE_LABEL);
    return static_cast<Future *>(transient);
  }

  void AddToWaitQueue(Thread *thread) {
    Queue *waitQueue = Queue::FromWord(GetArg());
    if (waitQueue == INVALID_POINTER) {
      waitQueue = Queue::New(2);
      ReplaceArg(waitQueue->ToWord());
    }
    waitQueue->Enqueue(thread->ToWord());
  }
  void ScheduleWaitingThreads() {
    Queue *waitQueue = Queue::FromWord(GetArg());
    if (waitQueue != INVALID_POINTER)
      while (!waitQueue->IsEmpty())
	Scheduler::WakeupThread(Thread::FromWordDirect(waitQueue->Dequeue()));
  }
};

class Hole: private Transient {
public:
  using Transient::ToWord;

  static Hole *New() {
    Transient *transient = Store::AllocTransient(HOLE_LABEL);
    transient->InitArg(0);
    return static_cast<Hole *>(transient);
  }

  Future *GetFuture() {
    Future *future = static_cast<Future *>(Store::WordToTransient(GetArg()));
    if (future == INVALID_POINTER) {
      future = Future::New();
      ReplaceArg(future->ToWord());
    }
    return future;
  }
  bool Fill(word w) {
    if (Store::WordToTransient(w) == this) // cyclic bind
      return false;
    Transient *future = Store::WordToTransient(GetArg());
    if (future != INVALID_POINTER) { // eliminate associated future
      static_cast<Future *>(future)->ScheduleWaitingThreads();
      future->Become(REF_LABEL, w);
    }
    Become(REF_LABEL, w);
    return true;
  }
};

class Byneed: private Transient {
public:
  using Transient::ToWord;

  static Byneed *New(word w) {
    Transient *transient = Store::AllocTransient(BYNEED_LABEL);
    transient->InitArg(w);
    return static_cast<Byneed *>(transient);
  }
};

#endif __SCHEDULER__TRANSIENTS_HH__
