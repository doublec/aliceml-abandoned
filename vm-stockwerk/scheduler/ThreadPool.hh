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

#ifndef __SCHEDULER__THREADPOOL_HH__
#define __SCHEDULER__THREADPOOL_HH__

#if defined(INTERFACE)
#pragma interface "scheduler/ThreadPool.hh"
#endif

#include "adt/Queue.hh"
#include "scheduler/Thread.hh"

//--** thread priorities ignored for now

class ThreadPool: private Queue {
private:
  static const int threshold = 8;
public:
  using Queue::ToWord;

  static ThreadPool *New() {
    return static_cast<ThreadPool *>(Queue::New(threshold));
  }
  static ThreadPool *FromWord(word x) {
    return static_cast<ThreadPool *>(Queue::FromWord(x));
  }

  Thread *Dequeue() {
    if (IsEmpty())
      return INVALID_POINTER;
    else
      return Thread::FromWord(Queue::Dequeue());
  }
  void Enqueue(Thread *thread) {
    Queue::Enqueue(thread->ToWord());
  }
  void PurgeAll() {
    Blank(threshold);
    //--** walk through queue and apply Purge to all elements
  }
};

#endif __SCHEDULER__THREADPOOL_HH__
