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

#ifndef __GENERIC__THREADQUEUE_HH__
#define __GENERIC__THREADQUEUE_HH__

#if defined(INTERFACE)
#pragma interface "generic/ThreadQueue.hh"
#endif

#include "adt/Queue.hh"
#include "generic/Thread.hh"

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
    //--** should respect thread priorities
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
    for (u_int i = GetNumberOfElements(); i--; )
      Thread::FromWordDirect(GetNthElement(i))->Purge();
  }
};

#endif __GENERIC__THREADQUEUE_HH__
