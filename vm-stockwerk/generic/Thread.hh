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

#ifndef __GENERIC__THREAD_HH__
#define __GENERIC__THREAD_HH__

#if defined(INTERFACE)
#pragma interface "generic/Thread.hh"
#endif

#include "store/Store.hh"
#include "generic/TaskStack.hh"

class Thread: private Block {
  friend class Scheduler;
public:
  enum priority {
    HIGH, NORMAL, LOW
  };

  enum state {
    BLOCKED, RUNNABLE, TERMINATED
  };
private:
  static const u_int PRIORITY_POS     = 0;
  static const u_int TASK_STACK_POS   = 1;
  static const u_int STATE_POS        = 2;
  static const u_int IS_SUSPENDED_POS = 3;
  static const u_int NARGS_POS        = 4;
  static const u_int ARGS_POS         = 5;
  static const u_int FUTURE_POS       = 6;
  static const u_int SIZE             = 7;

  void SetState(state s) {
    InitArg(STATE_POS, s);
    InitArg(FUTURE_POS, 0);
  }
  void Suspend() {
    ReplaceArg(IS_SUSPENDED_POS, true);
  }
  void Resume() {
    ReplaceArg(IS_SUSPENDED_POS, false);
  }
public:
  using Block::ToWord;

  // Thread Constructor
  static Thread *New(u_int nArgs, word args, TaskStack *taskStack) {
    Block *b = Store::AllocBlock(THREAD_LABEL, SIZE);
    b->InitArg(PRIORITY_POS, NORMAL);
    b->InitArg(TASK_STACK_POS, taskStack->ToWord());
    b->InitArg(STATE_POS, RUNNABLE);
    b->InitArg(IS_SUSPENDED_POS, false);
    b->InitArg(NARGS_POS, nArgs);
    b->InitArg(ARGS_POS, args);
    b->InitArg(FUTURE_POS, 0);
    return static_cast<Thread *>(b);
  }
  // Thread Untagging
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

  // Thread Accessors
  TaskStack *GetTaskStack() { //--** should be private
    return TaskStack::FromWordDirect(GetArg(TASK_STACK_POS));
  }
  void SetArgs(u_int nArgs, word args) {
    ReplaceArg(NARGS_POS, nArgs);
    ReplaceArg(ARGS_POS, args);
  }
  word GetArgs(u_int &nArgs) {
    nArgs = Store::DirectWordToInt(GetArg(NARGS_POS));
    return GetArg(ARGS_POS);
  }
  priority GetPriority() {
    return static_cast<priority>(Store::DirectWordToInt(GetArg(PRIORITY_POS)));
  }
  bool IsSuspended() {
    return Store::DirectWordToInt(GetArg(IS_SUSPENDED_POS));
  }
  state GetState() {
    return static_cast<state>(Store::DirectWordToInt(GetArg(STATE_POS)));
  }
  void SetTerminated() {
    SetState(TERMINATED);
  }
  void BlockOn(word future) {
    // Store the future we're blocking on, for unregistering:
    SetState(BLOCKED);
    ReplaceArg(FUTURE_POS, future);
  }
  word GetFuture() {
    return GetArg(FUTURE_POS);
  }
  void Wakeup() {
    ReplaceArg(FUTURE_POS, 0);
    SetState(RUNNABLE);
  }
  void Purge() {
    GetTaskStack()->Purge();
  }
};

#endif
