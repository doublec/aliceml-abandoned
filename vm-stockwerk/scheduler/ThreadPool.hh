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

#ifndef __THREADPOOL_HH__
#define __THREADPOOL_HH__

#include "store/store.hh"

class Thread;

class ThreadPool: private Block { // priority queue
public:
  using Block::ToWord;

  static ThreadPool *New();
  Thread *Dequeue(); // returns INVALID_POINTER if queue empty
  void Enqueue(Thread *thread);

  static ThreadPool *FromWord(word w) {
    return static_cast<ThreadPool *>(Store::WordToBlock(w));
  }
};

#endif
