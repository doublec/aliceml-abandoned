//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000-2001
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__THREAD_HH__
#define __GENERIC__THREAD_HH__

#if defined(INTERFACE)
#pragma interface "emulator/Thread.hh"
#endif

#include "store/Store.hh"
#include "emulator/TaskStack.hh"

class Thread : private Block {
  friend class Scheduler;
public:
  enum priority {
    HIGH, NORMAL, LOW
  };

  enum state {
    BLOCKED, RUNNABLE, TERMINATED
  };
private:
  static const u_int PRIORITY_POS = 0;
  static const u_int TASK_STACK_POS = 1;
  static const u_int STATE_POS = 2;
  static const u_int IS_SUSPENDED_POS = 3;
  static const u_int ARGS_POS = 4;
  static const u_int BLOCK_POS = 5;
  static const u_int SIZE = 6;

  void SetState(state s) {
    InitArg(STATE_POS, s);
    InitArg(BLOCK_POS, 0);
  }
  void Suspend() {
    ReplaceArg(IS_SUSPENDED_POS, true);
  }
  void Resume() {
    ReplaceArg(IS_SUSPENDED_POS, false);
  }
public:
  using Block::ToWord;
  // Thread Accessors
  word GetArgs() {
    return GetArg(ARGS_POS);
  }
  TaskStack *GetTaskStack() { //--** should be private
    return TaskStack::FromWordDirect(GetArg(TASK_STACK_POS));
  }
  priority GetPriority() {
    return static_cast<priority>(Store::DirectWordToInt(GetArg(PRIORITY_POS)));
  }
  state GetState() {
    return static_cast<state>(Store::DirectWordToInt(GetArg(STATE_POS)));
  }
  word GetTransient() {
    return GetArg(BLOCK_POS);
  }
  bool IsSuspended() {
    return Store::DirectWordToInt(GetArg(IS_SUSPENDED_POS));
  }
  void Purge() {
    GetTaskStack()->Purge();
  }
  void SetArgs(word args) {
    ReplaceArg(ARGS_POS, args);
  }
  void SetTerminated() {
    SetState(TERMINATED);
  }
  void Block(word transient) {
    SetState(BLOCKED);
    ReplaceArg(BLOCK_POS, transient);
  }
  void Wakeup() {
    SetState(RUNNABLE);
  }
  void Unregister() {
    Error("Thread::Unregister not implemented"); //--** to be done
  }
  // Thread Constructor
  static Thread *New(word args, TaskStack *taskstack) {
    Thread *b = (Thread *) Store::AllocBlock(THREAD_LABEL, SIZE);
    b->InitArg(PRIORITY_POS, NORMAL);
    b->ReplaceArg(TASK_STACK_POS, taskstack->ToWord());
    b->InitArg(STATE_POS, RUNNABLE);
    b->InitArg(IS_SUSPENDED_POS, false);
    b->ReplaceArg(ARGS_POS, args);
    return b;
  }
  // Thread Untagging
  static Thread *FromWord(word x) {
    Thread *b = (Thread *) Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == THREAD_LABEL);
    return b;
  }
  static Thread *FromWordDirect(word x) {
    Thread *b = (Thread *) Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == THREAD_LABEL);
    return b;
  }
};

#endif
