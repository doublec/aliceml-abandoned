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

#pragma interface "scheduler/Transients.hh"

#include "store/store.hh"
#include "scheduler/Scheduler.hh"

//
// Transient Representation:
//
//    Label       Argument
//    -----       --------
//    HOLE        associated future (or the integer 0 if not created)
//    FUTURE      wait queue
//    CANCELLED   exception
//    BYNEED      closure
//    REF         value
//

class Queue: private Block {
  //--** Queue implementation missing and should go elsewhere
public:
  using Block::ToWord;

  static Queue *New();
  static Queue *FromWord(word w);

  void Enqueue(word w);
  bool IsEmpty();
  word Dequeue();
};

class Future: private Transient {
public:
  using Transient::ToWord;

  static Future *New() {
    Transient *transient = Store::AllocTransient(FUTURE);
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
    Transient *transient = Store::AllocTransient(HOLE);
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
      future->Become(REF, w);
    Become(REF, w);
    return true;
  }
};

class Byneed: private Transient {
public:
  using Transient::ToWord;

  static Byneed *New(word w) {
    Transient *transient = Store::AllocTransient(BYNEED);
    transient->InitArg(w);
    return static_cast<Byneed *>(transient);
  }
  static Byneed *FromWord(word w) {
    Transient *t = Store::WordToTransient(w);
    Assert(t == INVALID_POINTER || t->GetLabel() == BYNEED);
    return static_cast<Byneed *>(t);
  }
};

#endif __SCHEDULER__TRANSIENTS_HH__
