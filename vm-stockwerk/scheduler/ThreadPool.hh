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

#include "store/store.hh"

class Thread;

class ThreadPool: private Block { // priority queue
public:
  using Block::ToWord;

  static ThreadPool *New(); //--** implement
  static ThreadPool *FromWord(word w); //--** implement

  Thread *Dequeue(); //--** implement; returns INVALID_POINTER if queue empty
  void Enqueue(Thread *thread); //--** implement
  void PurgeAll(); //--** implement
};

#endif __SCHEDULER__THREADPOOL_HH__
