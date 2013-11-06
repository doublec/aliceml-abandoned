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

#include <cstring>
#include "store/Store.hh"
#include "generic/Tuple.hh"
#include "generic/TaskStack.hh"
#if DEBUGGER
#include "generic/GenericDebuggerEvent.hh"
#include "generic/Debugger.hh"
#endif


class SeamDll Thread: private Block {
  friend class Scheduler;
public:
  enum priority { HIGH, NORMAL, LOW };
  
#if DEBUGGER
  enum debugMode { NONE, DEBUG, DETACH};
#endif
  
  enum state { BLOCKED, RUNNABLE, TERMINATED };
  
  /**
   * A handler starts off at TRYING, and becomes HANDLING if
   * an exception is thrown during the time it remains at the
   * top of the stack. The data stored for trying is handler-specific,
   * but the data for HANDLING is always (exn * backtrace).
   */
  enum handler_state { TRYING, HANDLING };

private:
  static const u_int HANDLER_FRAME_SIZE = 3;
  static const u_int INITIAL_HANDLERS_SIZE = 10; // to be done
  
  enum {
    PRIORITY_POS, STATE_POS, IS_SUSPENDED_POS,
    TASK_STACK_POS, NARGS_POS, ARGS_POS,
    FUTURE_POS, EXN_HANDLER_STACK_POS,
#if DEBUGGER
    DEBUG_MODE_POS,
#endif
    SIZE
  };

  void SetState(state s) {
    ReplaceArg(STATE_POS, s);
    ReplaceArg(FUTURE_POS, static_cast<s_int>(0));
  }
  
  void Suspend() {
    ReplaceArg(IS_SUSPENDED_POS, static_cast<s_int>(true));
  }
  
  void Resume() {
    ReplaceArg(IS_SUSPENDED_POS, static_cast<s_int>(false));
  }
  
public:
  using Block::ToWord;

  // Thread Constructor
  static Thread *New(u_int nArgs, word args) {
    Block *b = Store::AllocMutableBlock(THREAD_LABEL, SIZE);
    b->InitArg(PRIORITY_POS, NORMAL);
    b->InitArg(STATE_POS, RUNNABLE);
    b->InitArg(IS_SUSPENDED_POS, static_cast<s_int>(false));
    b->InitArg(TASK_STACK_POS, TaskStack::New()->ToWord());
    b->InitArg(NARGS_POS, nArgs);
    b->InitArg(ARGS_POS, args);
    b->InitArg(FUTURE_POS, static_cast<s_int>(0));
    DynamicBlock *exnHandlerStack =
      Store::AllocDynamicBlock(INITIAL_HANDLERS_SIZE, 3);
    // to be done: this belongs to TaskStack
    exnHandlerStack->InitArg(0, static_cast<s_int>(0));
    exnHandlerStack->InitArg(1, TRYING); 
    exnHandlerStack->InitArg(2, Store::IntToWord(0));
    b->InitArg(EXN_HANDLER_STACK_POS, exnHandlerStack->ToWord());
#if DEBUGGER
    b->InitArg(DEBUG_MODE_POS, NONE);
#endif
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
  priority GetPriority() {
    return static_cast<priority>(Store::DirectWordToInt(GetArg(PRIORITY_POS)));
  }
  
#if DEBUGGER
  debugMode GetDebugMode() {
    return static_cast<debugMode>(Store::DirectWordToInt(GetArg(DEBUG_MODE_POS)));
  }

  void SetDebugMode(debugMode mode) {
    ReplaceArg(DEBUG_MODE_POS, mode);
  }
#endif

  state GetState() {
    return static_cast<state>(Store::DirectWordToInt(GetArg(STATE_POS)));
  }
  
  bool IsSuspended() {
    return Store::DirectWordToInt(GetArg(IS_SUSPENDED_POS));
  }
  
  TaskStack *GetTaskStack() {
    TaskStack *taskStack = TaskStack::FromWordDirect(GetArg(TASK_STACK_POS));
#ifdef DEBUG_CHECK
    ReplaceArg(TASK_STACK_POS, static_cast<s_int>(0));
#endif
    return taskStack;
  }
  
  void SetTaskStack(TaskStack *taskStack) {
    Assert(GetArg(TASK_STACK_POS) == Store::IntToWord(0));
    ReplaceArg(TASK_STACK_POS, taskStack->ToWord());
  }
  
  word GetArgs(u_int &nArgs) {
    nArgs = Store::DirectWordToInt(GetArg(NARGS_POS));
    return GetArg(ARGS_POS);
  }
  
  void SetArgs(u_int nArgs, word args) {
    ReplaceArg(NARGS_POS, nArgs);
    ReplaceArg(ARGS_POS, args);
  }
  
  void SetTerminated() {
    SetArgs(0, Store::IntToWord(0));
    SetState(TERMINATED);
#if DEBUGGER
    if (GetDebugMode() == DEBUG) {
      GenericDebuggerEvent *event = 
	GenericDebuggerEvent::New(GenericDebuggerEvent::TERMINATED, 
				  this->ToWord(),
				  Store::IntToWord(0));
      Debugger::SendEvent(event->ToWord());
    }
#endif
  }
  
  void BlockOn(word future) {
    // Store the future we're blocking on, for unregistering:
    SetState(BLOCKED);
    ReplaceArg(FUTURE_POS, future);
#if DEBUGGER
    if(GetDebugMode() == DEBUG) {
      GenericDebuggerEvent *event = 
	GenericDebuggerEvent::New(GenericDebuggerEvent::BLOCKED, 
				  this->ToWord(), 
				  Store::IntToWord(0));
      Debugger::SendEvent(event->ToWord());
    }
#endif
  }
  
  word GetFuture() {
    return GetArg(FUTURE_POS);
  }
  
  void Wakeup() {
    ReplaceArg(FUTURE_POS, static_cast<s_int>(0));
    SetState(RUNNABLE);
#if DEBUGGER
    if(GetDebugMode() == DEBUG) {
      GenericDebuggerEvent *event = 
	GenericDebuggerEvent::New(GenericDebuggerEvent::RUNNABLE, 
				  this->ToWord(),
				  Store::IntToWord(0));
      Debugger::SendEvent(event->ToWord());
    }
#endif
  }

  // Task Stack Operations
  StackFrame *PushFrame(u_int size) {
    TaskStack *taskStack = TaskStack::FromWordDirect(GetArg(TASK_STACK_POS));
    u_int top = taskStack->GetTop();
    u_int newTop = top + size;
    if (newTop >= taskStack->GetSize()) {
      taskStack = taskStack->Enlarge();
      ReplaceArg(TASK_STACK_POS, taskStack->ToWord());
    }
    Assert(newTop < taskStack->GetSize());
    taskStack->SetTop(newTop);
    return taskStack->GetFrame(newTop - 1);
  }
  
  void PushHandler(u_int frame, word data) {
    DynamicBlock *exnHandlerStack =
      DynamicBlock::FromWordDirect(GetArg(EXN_HANDLER_STACK_POS));
    u_int top  = exnHandlerStack->GetActiveSize();
    u_int size = exnHandlerStack->GetSize();
    if ((top + HANDLER_FRAME_SIZE) >= size) {
      u_int newSize = size * 3 / 2;
      DynamicBlock *newExnHandlerStack =
	Store::AllocDynamicBlock(newSize, top);
      std::memcpy(newExnHandlerStack->GetBase(), exnHandlerStack->GetBase(),
		  size * sizeof(u_int));
      ReplaceArg(EXN_HANDLER_STACK_POS, newExnHandlerStack->ToWord());
      exnHandlerStack = newExnHandlerStack;
    }
    Assert((top + HANDLER_FRAME_SIZE) < exnHandlerStack->GetSize());
    exnHandlerStack->SetActiveSize(top + HANDLER_FRAME_SIZE);
    exnHandlerStack->ReplaceArg(top, frame);
    exnHandlerStack->ReplaceArg(top + 1, TRYING);
    exnHandlerStack->ReplaceArg(top + 2, data);
  }
  
  void PushHandler(word data) {
    TaskStack *taskStack = TaskStack::FromWordDirect(GetArg(TASK_STACK_POS));
    u_int top = taskStack->GetTop();
    Assert(top >= HANDLER_FRAME_SIZE);
    PushHandler(top - 1, data);
  }
  
  void ActivateNextHandler(u_int &frame, word &data, Tuple *package) {
    DynamicBlock *exnHandlerStack =
      DynamicBlock::FromWordDirect(GetArg(EXN_HANDLER_STACK_POS));
    
    while (true) {
      u_int top = exnHandlerStack->GetActiveSize();
      Assert(top >= HANDLER_FRAME_SIZE);
      
      data = exnHandlerStack->GetArg(top - 1);
      u_int state = Store::DirectWordToInt(exnHandlerStack->GetArg(top - 2));
      frame = Store::DirectWordToInt(exnHandlerStack->GetArg(top - 3));
      
      if (state == TRYING) {
	exnHandlerStack->ReplaceArg(top - 1, package->ToWord());
	exnHandlerStack->ReplaceArg(top - 2, HANDLING);
	return;
      }
      else { // state == HANDLING
	exnHandlerStack->SetActiveSize(top - HANDLER_FRAME_SIZE);
      }
    }
  }
  
  /**
   * @return The package of the exn that is currently being handled, if any.
   */
  Tuple *GetCurrentPackage() {
    DynamicBlock *exnHandlerStack =
      DynamicBlock::FromWordDirect(GetArg(EXN_HANDLER_STACK_POS));
    
    u_int cur = exnHandlerStack->GetActiveSize();
    while(cur > 0){
      Assert(cur >= HANDLER_FRAME_SIZE);
      
      word data = exnHandlerStack->GetArg(cur - 1);
      u_int state = Store::DirectWordToInt(exnHandlerStack->GetArg(cur - 2));
      
      if (state == HANDLING) {
	return Tuple::FromWordDirect(data);
      }
      cur -= 3;
    }
    
    return INVALID_POINTER;
  }
  
  void PopHandler() {
    DynamicBlock *exnHandlerStack =
      DynamicBlock::FromWordDirect(GetArg(EXN_HANDLER_STACK_POS));
    Assert(exnHandlerStack->GetActiveSize() >= HANDLER_FRAME_SIZE);
    exnHandlerStack->SetActiveSize(exnHandlerStack->GetActiveSize() - HANDLER_FRAME_SIZE);
  }
  
  void Purge() {
    TaskStack::FromWordDirect(GetArg(TASK_STACK_POS))->Purge();
  }
};

#endif
