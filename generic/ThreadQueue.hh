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

#ifndef __GENERIC__THREAD_QUEUE_HH__
#define __GENERIC__THREAD_QUEUE_HH__

#if defined(INTERFACE)
#pragma interface "generic/ThreadQueue.hh"
#endif

#include "adt/Queue.hh"
#include "generic/Thread.hh"

class SeamDll ThreadQueue: private Queue {
private:
  static const int threshold = 8; //--** to be checked
public:
  using Queue::ToWord;

  // ThreadQueue Constructor
  static ThreadQueue *New() {
    return STATIC_CAST(ThreadQueue *, Queue::New(threshold));
  }
  // ThreadQueue Untagging
  static ThreadQueue *FromWord(word x) {
    return STATIC_CAST(ThreadQueue *, Queue::FromWord(x));
  }
  static ThreadQueue *FromWordDirect(word x) {
    return STATIC_CAST(ThreadQueue *, Queue::FromWordDirect(x));
  }

  // ThreadQueue Functions
  Thread *Dequeue() { //--** should respect thread priorities
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
  void Purge() {
    Blank();
    for (u_int i = GetNumberOfElements(); i--; )
      Thread::FromWordDirect(GetNthElement(i))->Purge();
  }
};

#endif
