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

#ifndef __GENERIC__TRANSIENTS_HH__
#define __GENERIC__TRANSIENTS_HH__

#if defined(INTERFACE)
#pragma interface "generic/Transients.hh"
#endif

#include "adt/Queue.hh"
#include "generic/Scheduler.hh"
#include "generic/Closure.hh"
#include "generic/Tuple.hh"
#include "generic/Backtrace.hh"

//
// Transient Representation:
//
//    Label             Argument
//    -----             --------
//    HOLE_LABEL        associated future (or an integer if not created)
//    FUTURE_LABEL      wait queue (or an integer if not created)
//    CANCELLED_LABEL   tuple of exception and backtrace
//    BYNEED_LABEL      closure
//    REF_LABEL         value
//

class SeamDll Future: public Transient {
private:
  static const u_int initialWaitQueueSize = 2;
public:
  // Future Constructor
  static Future *New() {
    Transient *transient = Store::AllocTransient(FUTURE_LABEL);
    transient->InitArg(0); // empty wait queue
    return STATIC_CAST(Future *, transient);
  }

  // Future Functions
  void AddToWaitQueue(Thread *thread) {
    Queue *waitQueue = Queue::FromWord(GetArg());
    if (waitQueue == INVALID_POINTER) {
      waitQueue = Queue::New(initialWaitQueueSize);
      ReplaceArg(waitQueue->ToWord());
    }
    waitQueue->Enqueue(thread->ToWord());
  }
  void RemoveFromWaitQueue(Thread *thread) { // precondition: is member
    Queue *waitQueue = Queue::FromWordDirect(GetArg());
    waitQueue->Remove(thread->ToWord());
  }
  void ScheduleWaitingThreads() {
    Queue *waitQueue = Queue::FromWord(GetArg());
    if (waitQueue != INVALID_POINTER)
      while (!waitQueue->IsEmpty())
	Scheduler::WakeupThread(Thread::FromWordDirect(waitQueue->Dequeue()));
  }
};

class SeamDll Hole: public Transient {
public:
  // Future Static Data
  static word cyclicExn;
  static word holeExn;

  // Future Static Constructor
  static void Init();

  // Hole Constructor
  static Hole *New() {
    Transient *transient = Store::AllocTransient(HOLE_LABEL);
    transient->InitArg(0); // no associated future
    return STATIC_CAST(Hole *, transient);
  }

  // Hole Functions
  Future *GetFuture() {
    Future *future = STATIC_CAST(Future *, Store::WordToTransient(GetArg()));
    if (future == INVALID_POINTER) {
      future = Future::New();
      ReplaceArg(future->ToWord());
    }
    return future;
  }
  bool Fill(word w) {
    Transient *t = Store::WordToTransient(w);
    if (t == this) // cyclic bind
      return false;
    Future *future = STATIC_CAST(Future *, Store::WordToTransient(GetArg()));
    if (future != INVALID_POINTER) { // eliminate associated future
      if (t == future) // cyclic bind
	return false;
      future->ScheduleWaitingThreads();
      future->Become(REF_LABEL, w);
    }
    Become(REF_LABEL, w);
    return true;
  }
  void Fail(word exn) {
    Future *future = STATIC_CAST(Future *, Store::WordToTransient(GetArg()));
    Tuple *package = Tuple::New(2);
    Backtrace *backtrace = Backtrace::New(); // TODO: have primitive in BT
    package->Init(0, exn);
    package->Init(1, backtrace->ToWord());
    if (future != INVALID_POINTER) { // eliminate associated future
      future->ScheduleWaitingThreads();
      future->Become(CANCELLED_LABEL, package->ToWord());
    }
    Become(CANCELLED_LABEL, package->ToWord());
  }
};

class SeamDll Byneed: public Transient {
public:
  // Byneed Constructor
  static Byneed *New(word closure) {
    Transient *transient = Store::AllocTransient(BYNEED_LABEL);
    transient->InitArg(closure);
    return STATIC_CAST(Byneed *, transient);
  }

  // Byneed Accessors
  Closure *GetClosure() {
    return Closure::FromWordDirect(GetArg());
  }
};

#endif
