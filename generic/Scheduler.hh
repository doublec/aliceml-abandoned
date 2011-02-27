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
#include <csetjmp>

class Backtrace;

#define SCHEDULER_THREAD_PREEMPT_STATUS 1

class SeamDll Scheduler {
  friend class JITGeneric;
public:
  static const u_int maxArgs = 16;
private:
  // ThreadQueue is root
  static word wThreadQueue;
  static Thread *currentThread;

  static void SwitchToThread();
  static void FlushThread();

  static u_int nArgs;                 // Number of arguments
  static word currentArgs[maxArgs];   // Arguments
  static word currentData;            // Transient or exception
  static Backtrace *currentBacktrace; // Backtrace

  static TaskStack *currentTaskStack; // Task stack
  static word *stackTop;              // Task stack top
  static word *stackMax;              // Task stack max
  static jmp_buf stackOverflowJmp;    // Used to signal stack overflow
public:
  static word StackError;             // Stack overflow exception

#if PROFILE
  static double gcTime;
#endif

  static void SetNArgs(u_int n) { 
    Assert(n<=maxArgs);
    nArgs = n;
  }
  static u_int GetNArgs(void) { return nArgs; }
  static void SetCurrentArg(u_int n, word w) {
    Assert(n < nArgs);
    currentArgs[n] = w;
  }
  static word GetCurrentArg(u_int n) {
    Assert(n < nArgs);
    return currentArgs[n];
  }
  static void SetCurrentData(word w) {
    currentData = w;
  }
  static word GetCurrentData(void) {
    return currentData;
  }
  static void SetCurrentBacktrace(Backtrace *b) {
    currentBacktrace = b;
  }
  static Backtrace *GetCurrentBacktrace(void) {
    return currentBacktrace;
  }
  static void SetCurrentTaskStack(TaskStack *t) {
    currentTaskStack = t;
  }
  static TaskStack *GetCurrentTaskStack(void) {
    return currentTaskStack;
  }
  static void SetStackTop(word *w) {
    stackTop = w;
  }
  static word *GetStackTop(void) {
    return stackTop;
  }
  static void SetStackMax(word *w) {
    stackMax = w;
  }
  static word *GetStackMax(void) {
    return stackMax;
  }

  // Scheduler Static Constructor
  static void Init();

  // Scheduler Main Function
  static int Run();

  // Scheduler Accessors
  static Thread *GetCurrentThread() {
    return currentThread;
  }
  static u_int GetCurrentStackTop() {
    word *base = reinterpret_cast<word *>(currentTaskStack->GetFrameBase());
    Assert(stackTop < stackMax);
    return static_cast<u_int>(stackTop - base);
  }

  // Scheduler Thread Functions
  static Thread *NewThread(u_int nArgs, word args) {
    Thread *thread = Thread::New(nArgs, args);
    ThreadQueue::FromWordDirect(wThreadQueue)->Enqueue(thread);
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
    ThreadQueue::FromWordDirect(wThreadQueue)->Enqueue(thread);
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
      ThreadQueue::FromWordDirect(wThreadQueue)->Remove(thread);
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
    word *base = reinterpret_cast<word *>(currentTaskStack->GetFrameBase());
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
    return reinterpret_cast<StackFrame *>(newTop);
  }
  static StackFrame *GetFrame() {
    return reinterpret_cast<StackFrame *>(stackTop);
  }
  // We need two PopFrame's: one for known frame size and generic
  static void PopFrame(u_int size) {
    Assert(reinterpret_cast<u_int>(stackTop - 1 - size) >=
           reinterpret_cast<u_int>(currentTaskStack->GetFrameBase()));
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
