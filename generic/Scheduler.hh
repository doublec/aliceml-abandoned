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

#include "generic/StackFrame.hh"
#include "generic/Thread.hh"
#include "generic/ThreadQueue.hh"
#include "generic/PushCallWorker.hh"

class Backtrace;

#define SCHEDULER_THREAD_PREEMPT_STATUS 1

class SeamDll Scheduler {
private:
  static word root;
  static ThreadQueue *threadQueue;
  static Thread *currentThread;

  static void SwitchToThread();
  static void FlushThread();
public:
  static const u_int maxArgs = 16;

  // Scheduler public data
  static TaskStack *currentTaskStack; // Task stack
  static word *stackTop;              // Task stack top
  static word *stackMax;              // Task stack max
  static u_int nArgs;                 // Number of arguments
  static word currentArgs[maxArgs];   // Arguments
  static word currentData;            // Transient or exception
  static Backtrace *currentBacktrace; // Backtrace
#if PROFILE
  static double gcTime;
#endif
  // Scheduler Static Constructor
  static void Init();

  // Scheduler Main Function
  static int Run();

  // Scheduler Accessors
  static Thread *GetCurrentThread() {
    return currentThread;
  }
  static u_int GetCurrentStackTop() {
    word *base = (word *) currentTaskStack->GetFrameBase();
    Assert(stackTop < stackMax);
    return STATIC_CAST(u_int, stackTop - base);
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
  static void EnlargeTaskStack() {
    u_int top = GetCurrentStackTop();
    currentTaskStack = currentTaskStack->Enlarge();
    word *base = (word *) currentTaskStack->GetFrameBase();
    stackTop = base + top;
    stackMax = base + currentTaskStack->GetSize();
  }
  static StackFrame *PushFrame(u_int size) {
  loop:
    word *newTop = stackTop + size;
    if (newTop >= stackMax) {
      Scheduler::EnlargeTaskStack();
      goto loop;
    }
    stackTop = newTop;
    return (StackFrame *) newTop;
  }
  static StackFrame *GetFrame() {
    return (StackFrame *) stackTop;
  }
  // We need two PopFrame's: one for known frame size and generic
  static void PopFrame(u_int size) {
    Assert((u_int) (stackTop - 1 - size) >= 
	   (u_int) currentTaskStack->GetFrameBase());
    stackTop -= size;
  }
  static void PopFrame() {
    StackFrame *frame = GetFrame();
    u_int size = frame->GetWorker()->GetFrameSize(frame);
    stackTop -= size;
  }
  static void PushHandler(word data) {
    u_int top = GetCurrentStackTop();
    currentThread->PushHandler(top - 1, data); // was top
  }
  static void PopHandler() {
    currentThread->PopHandler();
  }
  static Worker::Result PushCall(word closure);
  // Other Scheduler Functions
  static u_int PreemptStatus() {
    return (1 << SCHEDULER_THREAD_PREEMPT_STATUS);
  }
};

void DumpCurrentTaskStack();

#endif
