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

#ifndef __GENERIC__TRANSIENTS_HH__
#define __GENERIC__TRANSIENTS_HH__

#if defined(INTERFACE)
#pragma interface "emulator/Transients.hh"
#endif

#include "store/Store.hh"
#include "adt/Queue.hh"
#include "emulator/Scheduler.hh"

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

class Future : public Transient {
private:
  static const u_int initialWaitQueueSize = 2;
public:
  // Future Static Data
  static word cyclicExn;
  // Future Functions
  void AddToWaitQueue(Thread *thread) {
    Queue *waitQueue = Queue::FromWord(GetArg());
    if (waitQueue == INVALID_POINTER) {
      waitQueue = Queue::New(initialWaitQueueSize);
      ReplaceArg(waitQueue->ToWord());
    }
    waitQueue->Enqueue(thread->ToWord());
  }
  void RemoveFromWaitQueue(Thread *thread) {
    Queue *waitQueue = Queue::FromWord(GetArg());
    if (waitQueue != INVALID_POINTER)
      waitQueue->Remove(thread->ToWord());
  }
  void ScheduleWaitingThreads() {
    Queue *waitQueue = Queue::FromWord(GetArg());
    if (waitQueue != INVALID_POINTER)
      while (!waitQueue->IsEmpty())
	Scheduler::WakeupThread(Thread::FromWordDirect(waitQueue->Dequeue()));
  }
  // Future Constructor
  static Future *New() {
    Transient *transient = Store::AllocTransient(FUTURE_LABEL);
    return static_cast<Future *>(transient);
  }
  // Future Static Constructor
  static void Init();
};

class Hole : public Transient {
public:
  // Hole Static Data
  static word holeExn;
  // Hole Functions
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
  // Hole Constructor
  static Hole *New() {
    Transient *transient = Store::AllocTransient(HOLE_LABEL);
    transient->InitArg(0);
    return static_cast<Hole *>(transient);
  }
  // Hole Static Constructor
  static void Init();
};

class Byneed : public Transient {
public:
  // Byneed Accessors
  Closure *GetClosure() {
    return Closure::FromWordDirect(GetArg());
  }
  // Byneed Constructor
  static Byneed *New(word closure) {
    Transient *transient = Store::AllocTransient(BYNEED_LABEL);
    transient->InitArg(closure);
    return static_cast<Byneed *>(transient);
  }
};

#endif
