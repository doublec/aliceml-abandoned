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
//    HOLE_LABEL        associated future (or the integer 0 if not created)
//    FUTURE_LABEL      wait queue
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
    thread->SetState(Thread::BLOCKED);
    thread->GetTaskStack()->Purge();
    Queue::FromWord(GetArg())->Enqueue(thread->ToWord());
  }
  void ScheduleWaitingThreads() {
    Queue *waitQueue = Queue::FromWord(GetArg());
    while (!waitQueue->IsEmpty()) {
      Thread *thread = Thread::FromWord(waitQueue->Dequeue());
      Scheduler::AddThread(thread);
    }
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
    Transient *transient = Store::WordToTransient(GetArg());
    if (transient == INVALID_POINTER) {
      Future *future = Future::New();
      ReplaceArg(future->ToWord());
      return future;
    } else {
      return static_cast<Future *>(transient);
    }
  }
  bool Fill(word w) {
    if (Store::WordToTransient(w) == this) // cyclic bind
      return false;
    Transient *future = Store::WordToTransient(GetArg());
    if (future != INVALID_POINTER) // eliminate associated future (if created)
      future->Become(REF_LABEL, w);
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
  static Byneed *FromWord(word w) {
    Transient *t = Store::WordToTransient(w);
    Assert(t == INVALID_POINTER || t->GetLabel() == BYNEED_LABEL);
    return static_cast<Byneed *>(t);
  }
};

#endif __SCHEDULER__TRANSIENTS_HH__
