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

#if defined(INTERFACE)
#pragma implementation "emulator/Scheduler.hh"
#endif
#include <cstdio>
#include "emulator/RootSet.hh"
#include "emulator/Interpreter.hh"
#include "emulator/TaskStack.hh"
#include "emulator/Transients.hh"
#include "emulator/ByneedInterpreter.hh"

word Scheduler::root;
ThreadQueue *Scheduler::threadQueue;
Thread *Scheduler::currentThread;
bool Scheduler::preempt;

u_int Scheduler::nArgs;
word Scheduler::currentArgs[Scheduler::maxArgs];
word Scheduler::currentData;
Backtrace *Scheduler::currentBacktrace;
word Scheduler::vmGUID;

void Scheduler::Timer() {
  preempt = true;
}

void Scheduler::Init() {
  threadQueue = ThreadQueue::New();
  RootSet::Add(root);
  root = Store::IntToWord(0);
  vmGUID = Tuple::New(4)->ToWord(); // Hack alert: to be done
}

static inline void SetThreadArgs(Thread *thread) {
  u_int nArgs = Scheduler::nArgs;
  Assert(Scheduler::nArgs == Scheduler::ONE_ARG ||
	 Scheduler::nArgs < Scheduler::maxArgs);
  switch (nArgs) {
  case 0:
    thread->SetArgs(0, Store::IntToWord(0));
    break;
  case Scheduler::ONE_ARG:
    thread->SetArgs(Scheduler::ONE_ARG, Scheduler::currentArgs[0]);
    break;
  default:
    Block *b = Store::AllocBlock(ARGS_LABEL, nArgs);
    for (u_int i = nArgs; i--; )
      b->InitArg(i, Scheduler::currentArgs[i]);
    thread->SetArgs(nArgs, b->ToWord());
    break;
  }
}

static inline void GetThreadArgs(Thread *thread) {
  u_int nArgs;
  word args = thread->GetArgs(nArgs);
  Assert(Scheduler::nArgs == Scheduler::ONE_ARG ||
	 Scheduler::nArgs < Scheduler::maxArgs);
  Scheduler::nArgs = nArgs;
  switch (nArgs) {
  case 0:
    break;
  case Scheduler::ONE_ARG:
    Scheduler::currentArgs[0] = args;
    break;
  default:
    Block *b = Store::DirectWordToBlock(args);
    Assert(b->GetLabel() == ARGS_LABEL);
    for (u_int i = nArgs; i--; )
      Scheduler::currentArgs[i] = b->GetArg(i);
    break;
  }
}

void Scheduler::Run() {
  //--** start timer thread
  while ((currentThread = threadQueue->Dequeue()) != INVALID_POINTER) {
    // Make sure we can execute the selected thread
    Assert(currentThread->GetState() == Thread::RUNNABLE);
    Assert(!currentThread->IsSuspended());
    // Obtain thread data
    TaskStack *taskStack = currentThread->GetTaskStack();
    GetThreadArgs(currentThread);
    bool nextThread = false;
    while (!nextThread) {
      preempt = false;
      Interpreter *interpreter = taskStack->GetInterpreter();
      Interpreter::Result result = interpreter->Run(taskStack);
    interpretResult:
      switch (result) {
      case Interpreter::CONTINUE:
	Assert(Scheduler::nArgs == Scheduler::ONE_ARG ||
	       Scheduler::nArgs < Scheduler::maxArgs);
	break;
      case Interpreter::PREEMPT:
	SetThreadArgs(currentThread);
	threadQueue->Enqueue(currentThread);
	nextThread = true;
	break;
      case Interpreter::RAISE:
	{
	raise:
	  interpreter = taskStack->GetInterpreter();
	  result =
	    interpreter->Handle(currentData, currentBacktrace, taskStack);
	  goto interpretResult;
	}
      case Interpreter::REQUEST:
	{
	  Transient *transient = Store::WordToTransient(currentData);
	  Assert(transient != INVALID_POINTER);
	  switch (transient->GetLabel()) {
	  case HOLE_LABEL:
	    Scheduler::currentData = Hole::holeExn;
	    goto raise;
	  case FUTURE_LABEL:
	    {
	      taskStack->Purge();
	      SetThreadArgs(currentThread);
	      Future *future = static_cast<Future *>(transient);
	      future->AddToWaitQueue(currentThread);
	      currentThread->BlockOn(transient->ToWord());
	      nextThread = true;
	    }
	    break;
	  case CANCELLED_LABEL:
	    Scheduler::currentData = transient->GetArg();
	    goto raise;
	  case BYNEED_LABEL:
	    {
	      TaskStack *newTaskStack = TaskStack::New();
	      ByneedInterpreter::PushFrame(newTaskStack, transient);
	      NewThread(transient->GetArg(), 0, Store::IntToWord(0),
			newTaskStack);
	      // The future's argument is an empty wait queue:
	      transient->Become(FUTURE_LABEL, Store::IntToWord(0));
	      taskStack->Purge();
	      SetThreadArgs(currentThread);
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
      case Interpreter::TERMINATE:
	SetThreadArgs(currentThread);
	currentThread->SetTerminated();
	nextThread = true;
	break;
      }
    }
#if 0 //--** enable GC
    if (Store::NeedGC()) {
      //--** add threads waiting for I/O as well as properties
      threadQueue->Purge();
      root = threadQueue->ToWord();
      RootSet::DoGarbageCollection();
      threadQueue = ThreadQueue::FromWordDirect(root);
    }
#endif
  }
  //--* select(...)
}
