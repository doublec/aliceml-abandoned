//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__SCHEDULER_HH__
#define __GENERIC__SCHEDULER_HH__

#if defined(INTERFACE)
#pragma interface "generic/Scheduler.hh"
#endif

#include "generic/Thread.hh"
#include "generic/ThreadQueue.hh"
#include "generic/PushCallWorker.hh"

class Backtrace;

#define SCHEDULER_THREAD_PREEMPT_STATUS 1

class DllExport Scheduler {
private:
  static word root;
  static ThreadQueue *threadQueue;
  static Thread *currentThread;

  static void SwitchToThread();
  static void FlushThread();
public:
  static const u_int maxArgs = 16;
  static const u_int ONE_ARG = maxArgs + 1;

  // Scheduler public data
  static u_int nFrames;               // Numer of frames on task stack
  static TaskStack *currentTaskStack; // Task stack
  static u_int nArgs;                 // Number of arguments
  static word currentArgs[maxArgs];   // Arguments
  static word currentData;            // Transient or exception
  static Backtrace *currentBacktrace; // Backtrace
  // Scheduler Static Constructor
  static void Init();

  // Scheduler Main Function
  static int Run();

  // Scheduler Accessors
  static Thread *GetCurrentThread() {
    return currentThread;
  }

  // Scheduler Thread Functions
  static Thread *NewThread(u_int nArgs, word args) {
    Thread *thread = Thread::New(nArgs, args);
    threadQueue->Enqueue(thread);
    return thread;
  }
  static Thread *NewThread(word closure, u_int nArgs, word args) {
    Thread *thread = NewThread(nArgs, args);
    PushCallWorker::PushFrame(thread, closure);
    return thread;
  }
  static void ScheduleThread(Thread *thread) {
    //--** precondition: must not be scheduled
    Assert(thread->GetState() == Thread::RUNNABLE);
    threadQueue->Enqueue(thread);
  }
  static void WakeupThread(Thread *thread) {
    Assert(thread->GetState() == Thread::BLOCKED);
    thread->Wakeup();
    if (!thread->IsSuspended())
      ScheduleThread(thread);
  }
  static void SuspendThread(Thread *thread) {
    thread->Suspend();
    thread->Purge();
    if (thread->GetState() == Thread::RUNNABLE)
      threadQueue->Remove(thread);
  }
  static void ResumeThread(Thread *thread) {
    if (thread->IsSuspended()) {
      thread->Resume();
      if (thread->GetState() == Thread::RUNNABLE)
	ScheduleThread(thread);
    }
  }
  // Scheduler Task Stack Functions
  static void PushFrameNoCheck(word frame) {
    Assert(nFrames < currentTaskStack->GetSize());
    currentTaskStack->ReplaceArg(nFrames++, frame);
  }
  static void PushFrame(word frame) {
    if (nFrames == currentTaskStack->GetSize())
      currentTaskStack = currentTaskStack->Enlarge();
    PushFrameNoCheck(frame);
  }
  static word GetFrame() {
    return currentTaskStack->GetArg(nFrames - 1);
  }
  static word GetAndPopFrame() {
    return currentTaskStack->GetArg(--nFrames);
  }
  static void PopFrame() {
    nFrames--;
  }
  static Worker::Result PushCall(word closure);
  // Other Scheduler Functions
  static u_int PreemptStatus() {
    return (1 << SCHEDULER_THREAD_PREEMPT_STATUS);
  }
};

#endif
