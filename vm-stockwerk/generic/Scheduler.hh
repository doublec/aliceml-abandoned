//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__SCHEDULER_HH__
#define __GENERIC__SCHEDULER_HH__

#if defined(INTERFACE)
#pragma interface "emulator/Scheduler.hh"
#endif

#include "emulator/ThreadQueue.hh"
#include "emulator/Thread.hh"

class Scheduler {
private:
  static word root;
  static ThreadQueue *threadQueue;
  static Thread *currentThread;
  static bool preempt;

  static void Timer();
public:
  // Scheduler public data
  static word currentData; // Transient or Exception
  static word currentArgs; // Arguments
  static word vmGUID;
  // Scheduler Main Function
  static void Run();
  // Scheduler Accessors
  static Thread *GetCurrentThread() {
    return currentThread;
  }
  // Scheduler Functions
  static void NewThread(word closure, word args, TaskStack *taskStack0) {
    static word zero     = Store::IntToWord(0);
    TaskStack *taskStack = taskStack0;
    if (closure != zero) {
      taskStack->PushCall(closure);
    }
    Thread *thread = Thread::New(args, taskStack);
    threadQueue->Enqueue(thread);
  }
  static void AddThread(Thread *thread) {
    Assert(thread->GetState() == Thread::RUNNABLE);
    threadQueue->Enqueue(thread);
  }
  static void WakeupThread(Thread *thread) {
    if (thread->GetState() == Thread::BLOCKED) {
      // The thread can already have been woken up
      // if it awaited more than one future.
      thread->Wakeup();
      if (!thread->IsSuspended())
	AddThread(thread);
    }
  }
  static void CondEnqueue(Thread *thread) {
    if ((thread->GetState() == Thread::RUNNABLE) &&
	(!threadQueue->Member(thread))) {
      threadQueue->Enqueue(thread);
    }
  }
  static void SuspendThread(Thread *thread) {
    thread->Suspend();
    thread->GetTaskStack()->Purge();
    if (thread->GetState() == Thread::RUNNABLE)
      threadQueue->Remove(thread);
 }
  static void ResumeThread(Thread *thread) {
    if (thread->IsSuspended()) {
      thread->Resume();
      if (thread->GetState() == Thread::RUNNABLE)
	AddThread(thread);
    }
  }
  static bool TestPreempt() {
    return preempt;
  }
  // Scheduler Static Constructor
  static void Init();
};

#endif
