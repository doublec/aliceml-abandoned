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
  enum priority {
    HIGH, NORMAL, LOW
  };

  enum state {
    BLOCKED, RUNNABLE, TERMINATED
  };

  using Block::ToWord;

  static Thread *New(priority);
  static Thread *FromWord(word);
  priority GetPriority();
  TaskStack *GetTaskStack();
  void Suspend();
  void Resume();
  bool IsSuspended();
  void SetState(state);
  state GetState();
};

#endif __THREAD_HH__
