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

#ifndef __SCHEDULER__THREAD_HH__
#define __SCHEDULER__THREAD_HH__

#if defined(INTERFACE)
#pragma interface "scheduler/Thread.hh"
#endif

#include "scheduler/TaskStack.hh"

class Thread: private Block {
private:
  static const u_int SIZE = 4;
  static const u_int PRIORITY_POS = 1;
  static const u_int TASK_STACK_POS = 2;
  static const u_int STATE_POS = 3;
  static const u_int IS_SUSPENDED_POS = 4;
public:
  enum priority {
    HIGH, NORMAL, LOW
  };

  enum state {
    BLOCKED, RUNNABLE, TERMINATED
  };

  using Block::ToWord;

  static Thread *New(priority p) {
    Block *b = Store::AllocBlock(THREAD_LABEL, SIZE);
    b->InitArg(PRIORITY_POS, p);
    b->InitArg(TASK_STACK_POS, TaskStack::New()->ToWord());
    b->InitArg(STATE_POS, RUNNABLE);
    b->InitArg(IS_SUSPENDED_POS, false);
    return static_cast<Thread *>(b);
  }
  static Thread *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == THREAD_LABEL);
    return static_cast<Thread *>(b);
  }
  static Thread *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == THREAD_LABEL);
    return static_cast<Thread *>(b);
  }

  priority GetPriority() {
    return static_cast<priority>(Store::DirectWordToInt(GetArg(PRIORITY_POS)));
  }
  TaskStack *GetTaskStack() {
    return TaskStack::FromWordDirect(GetArg(TASK_STACK_POS));
  }
  void SetState(state s) {
    ReplaceArg(STATE_POS, s);
  }
  state GetState() {
    return static_cast<state>(Store::DirectWordToInt(GetArg(STATE_POS)));
  }
  void Suspend() {
    ReplaceArg(IS_SUSPENDED_POS, true);
  }
  void Resume() {
    ReplaceArg(IS_SUSPENDED_POS, false);
  }
  bool IsSuspended() {
    return Store::DirectWordToInt(GetArg(IS_SUSPENDED_POS));
  }
};

#endif __SCHEDULER__THREAD_HH__
