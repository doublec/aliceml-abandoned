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

#ifndef __THREAD_HH__
#define __THREAD_HH__

#include "store/store.hh"

class TaskStack;

class Thread: private Block {
public:
  enum {
    HIGH, MEDIUM, LOW
  } priority;

  using Block::ToWord;

  static Thread *New(Thread::priority, TaskStack *);
  Thread::priority GetPriority();
  TaskStack *GetTaskStack();
  void UpdateTaskStack(TaskStack *);
};

#endif __THREAD_HH__
