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

#if defined(INTERFACE)
#pragma implementation "generic/Scheduler.hh"
#endif

#include "generic/SignalHandler.hh"
#include <cstdio>
#include "generic/RootSet.hh"
#include "generic/ConcreteCode.hh"
#include "generic/Closure.hh"
#include "generic/Transients.hh"
#include "generic/StackFrame.hh"
#include "generic/BindFutureWorker.hh"
#include "generic/PushCallWorker.hh"
#include "generic/IOHandler.hh"
#include "generic/Backtrace.hh"

#if PROFILE
#include "generic/Profiler.hh"
#endif

word Scheduler::root;
ThreadQueue *Scheduler::threadQueue;
Thread *Scheduler::currentThread;
TaskStack *Scheduler::currentTaskStack;
word *Scheduler::stackTop;
word *Scheduler::stackMax;
#if PROFILE
double Scheduler::gcTime;
#endif

u_int Scheduler::nArgs;
word Scheduler::currentArgs[Scheduler::maxArgs];
word Scheduler::currentData;
Backtrace *Scheduler::currentBacktrace;

void Scheduler::Init() {
  threadQueue = ThreadQueue::New();
  root = Store::IntToWord(0);
  RootSet::Add(root);
#if PROFILE
  gcTime = 0.0;
#endif
}

inline void Scheduler::SwitchToThread() {
  // Precondition: currentThread initialized
  Assert(currentThread->GetState() == Thread::RUNNABLE);
  Assert(!currentThread->IsSuspended());
  currentTaskStack = currentThread->GetTaskStack();
  word *base = (word *) currentTaskStack->GetFrameBase();
  stackTop = base + currentTaskStack->GetTop();
  stackMax = base + currentTaskStack->GetSize();
  word args = currentThread->GetArgs(nArgs);
  Assert(nArgs == ONE_ARG || nArgs < maxArgs);
  switch (nArgs) {
  case 0:
    break;
  case Scheduler::ONE_ARG:
    currentArgs[0] = args;
    break;
  default:
    Block *b = Store::DirectWordToBlock(args);
    Assert(b->GetLabel() == ARGS_LABEL);
    for (u_int i = nArgs; i--; )
      currentArgs[i] = b->GetArg(i);
    break;
  }
}

inline void Scheduler::FlushThread() {
  u_int top = GetCurrentStackTop();
  currentTaskStack->SetTop(top);
  currentThread->SetTaskStack(currentTaskStack);
  Assert(nArgs == ONE_ARG || nArgs < maxArgs);
  switch (nArgs) {
  case 0:
    currentThread->SetArgs(0, Store::IntToWord(0));
    break;
  case Scheduler::ONE_ARG:
    currentThread->SetArgs(ONE_ARG, currentArgs[0]);
    break;
  default:
    Block *b = Store::AllocBlock(ARGS_LABEL, nArgs);
    for (u_int i = nArgs; i--; )
      b->InitArg(i, currentArgs[i]);
    currentThread->SetArgs(nArgs, b->ToWord());
    break;
  }
}

int Scheduler::Run() {
  while (true) {
    while ((currentThread = threadQueue->Dequeue()) != INVALID_POINTER) {
      SwitchToThread();
      for (bool nextThread = false; !nextThread; ) {
	StackFrame *frame = GetFrame();
	Worker *worker = frame->GetWorker();
#if PROFILE
	Profiler::SampleHeap(frame);
#endif
	Worker::Result result = worker->Run(frame);
#if PROFILE
	Profiler::AddHeap();
#endif
      interpretResult:
	switch (result) {
	case Worker::CONTINUE:
	  Assert(nArgs == ONE_ARG || nArgs < maxArgs);
	  break;
	case Worker::PREEMPT:
	  FlushThread();
	  threadQueue->Enqueue(currentThread);
	  nextThread = true;
	  break;
	case Worker::SUSPEND:
	  FlushThread();
	  currentThread->Purge();
	  currentThread->Suspend();
	  nextThread = true;
	  break;
	case Worker::RAISE:
	  {
	  raise:
	    u_int handler;
	    word data;
	    currentThread->GetHandler(handler, data);
	    Assert(GetCurrentStackTop() >= handler);
	    StackFrame *handlerFrame = currentTaskStack->GetFrame(handler);
	    // Unroll stack down to the handler frame
	    // to be done: make configurable whether to have backtrace or not
	    frame = GetFrame();
	    while (frame > handlerFrame) {
	      word wFrame = frame->Clone();
	      currentBacktrace->Enqueue(wFrame);
	      PopFrame();
	      frame = GetFrame();
	    }
	    worker = frame->GetWorker();
#if PROFILE
	    // to be done: at this position, the backtrace will not be counted
	    Profiler::SampleHeap(frame);
#endif
	    result = worker->Handle(data);
#if PROFILE
	    Profiler::AddHeap();
#endif
	    goto interpretResult;
	  }
	case Worker::REQUEST:
	  {
	    Transient *transient = Store::WordToTransient(currentData);
	    Assert(transient != INVALID_POINTER);
	    switch (transient->GetLabel()) {
	    case HOLE_LABEL:
	      currentData = Hole::holeExn;
	      goto raise;
	    case FUTURE_LABEL:
	      {
		FlushThread();
		currentThread->Purge();
		Future *future = static_cast<Future *>(transient);
		future->AddToWaitQueue(currentThread);
		currentThread->BlockOn(transient->ToWord());
		nextThread = true;
	      }
	      break;
	    case CANCELLED_LABEL:
	      currentData = transient->GetArg();
	      goto raise;
	    case BYNEED_LABEL:
	      {
		Thread *newThread = NewThread(0, Store::IntToWord(0));
		BindFutureWorker::PushFrame(newThread, transient);
		PushCallWorker::PushFrame(newThread, transient->GetArg());
		// The future's argument is an empty wait queue:
		transient->Become(FUTURE_LABEL, Store::IntToWord(0));
		FlushThread();
		currentThread->Purge();
		Future *future = static_cast<Future *>(transient);
		future->AddToWaitQueue(currentThread);
		currentThread->BlockOn(transient->ToWord());
		nextThread = true;
	      }
	      break;
	    default:
	      Error("Scheduler::Run: invalid transient label");
	      break;
	    }
	  }
	  break;
	case Worker::TERMINATE:
	  FlushThread();
	  currentThread->SetTerminated();
	  nextThread = true;
	  break;
	case Worker::EXIT:
	  return Store::DirectWordToInt(currentData);
	}
      }
      if (Store::NeedGC()) {
#if PROFILE
	double beforeGC = Profiler::SampleTime();
#endif
	threadQueue->Purge();
	IOHandler::Purge();
	root = threadQueue->ToWord();
	RootSet::DoGarbageCollection();
	threadQueue = ThreadQueue::FromWordDirect(root);
#if PROFILE
	double afterGC = Profiler::SampleTime();
	gcTime += (afterGC - beforeGC);
#endif
      }
      IOHandler::Poll();
      if (SignalHandler::GetSignalStatus())
	SignalHandler::HandlePendingSignals();
      StatusWord::ClearStatus();
    }
    // Wait for I/O and/or asynchronous signals
    IOHandler::Block();
    if (SignalHandler::GetSignalStatus())
      SignalHandler::HandlePendingSignals();
    StatusWord::ClearStatus();
  }
}

Worker::Result Scheduler::PushCall(word wClosure) {
  Assert(Store::WordToInt(wClosure) == INVALID_INT);
  Transient *transient = Store::WordToTransient(wClosure);
  if (transient == INVALID_POINTER) { // Closure is determined
    Closure *closure = Closure::FromWord(wClosure);
    Assert(closure != INVALID_POINTER);
    word concreteCodeWord = closure->GetConcreteCode();
    transient = Store::WordToTransient(concreteCodeWord);
    if (transient == INVALID_POINTER) { // ConcreteCode is determined
      ConcreteCode *concreteCode = ConcreteCode::FromWord(concreteCodeWord);
      Assert(concreteCode != INVALID_POINTER);
      concreteCode->GetInterpreter()->PushCall(closure);
#if PROFILE
      StackFrame *frame = GetFrame();
      Profiler::IncCalls(frame);
#endif
      if (StatusWord::GetStatus() != 0)
	return Worker::PREEMPT;
      else
	return Worker::CONTINUE;
    } else { // Request ConcreteCode
      PushCallWorker::PushFrame(wClosure);
      currentData = transient->ToWord();
      return Worker::REQUEST;
    }
  } else { // Request Closure
    PushCallWorker::PushFrame(wClosure);
    currentData = transient->ToWord();
    return Worker::REQUEST;
  }
}

void DumpCurrentTaskStack() {
  u_int top = Scheduler::GetCurrentStackTop();
  Scheduler::currentTaskStack->Dump(top);
}
