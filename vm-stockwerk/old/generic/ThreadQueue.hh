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

#ifndef __SCHEDULER__THREADQUEUE_HH__
#define __SCHEDULER__THREADQUEUE_HH__

#if defined(INTERFACE)
#pragma interface "scheduler/ThreadQueue.hh"
#endif

#include "adt/Queue.hh"
#include "scheduler/Thread.hh"

//--** thread priorities ignored for now

class ThreadQueue: private Queue {
private:
  static const int threshold = 8;
public:
  using Queue::ToWord;

  static ThreadQueue *New() {
    return static_cast<ThreadQueue *>(Queue::New(threshold));
  }
  static ThreadQueue *FromWord(word x) {
    return static_cast<ThreadQueue *>(Queue::FromWord(x));
  }
  static ThreadQueue *FromWordDirect(word x) {
    return static_cast<ThreadQueue *>(Queue::FromWordDirect(x));
  }

  Thread *Dequeue() {
    if (IsEmpty())
      return INVALID_POINTER;
    else
      return Thread::FromWordDirect(Queue::Dequeue());
  }
  void Enqueue(Thread *thread) {
    Queue::Enqueue(thread->ToWord());
  }
  void Remove(Thread *thread) {
    Queue::Remove(thread->ToWord());
  }
  void PurgeAll() {
    Blank();
    //--** walk through queue and apply Purge to all elements
  }
};

#endif __SCHEDULER__THREADQUEUE_HH__
