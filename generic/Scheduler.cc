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

word Scheduler::root;
ThreadQueue *Scheduler::threadQueue;
Thread *Scheduler::currentThread;
bool Scheduler::preempt;

word Scheduler::currentArgs;
word Scheduler::currentData;

void Scheduler::Timer() {
  preempt = true;
}

void Scheduler::Init() {
  threadQueue = ThreadQueue::New();
  RootSet::Add(root);
}

void Scheduler::Run() {
  //--** start timer thread
  while ((currentThread = threadQueue->Dequeue()) != INVALID_POINTER) {
    // Make sure we can execute the selected thread
    Assert(currentThread->GetState() == Thread::RUNNABLE);
    Assert(!currentThread->IsSuspended());
    // Obtain thread data
    TaskStack *taskStack = currentThread->GetTaskStack();
    bool nextThread = false;
    while (!nextThread) {
      preempt = false;
      Interpreter *interpreter = taskStack->GetInterpreter();
      fprintf(stderr, "Executing frame %s, %s\n",
	      interpreter->Identify(),
	      interpreter->ToString(Store::IntToWord(0), taskStack));
      Interpreter::Result result =
	interpreter->Run(currentThread->GetArgs(), taskStack);
    interpretResult:
      fprintf(stderr, "Got result %d\n", result);
      switch (result) {
      case Interpreter::CONTINUE:
	currentThread->SetArgs(currentArgs);
	break;
      case Interpreter::PREEMPT:
	currentThread->SetArgs(currentArgs);
	threadQueue->Enqueue(currentThread);
	nextThread = true;
	break;
      case Interpreter::RAISE: {
      raise:
	currentThread->SetArgs(currentArgs);
        interpreter = taskStack->GetInterpreter();
        result      = interpreter->Handle(currentThread->GetArgs(), taskStack);
        goto interpretResult;
      }
      case Interpreter::REQUEST: {
	Transient *transient = Store::WordToTransient(Scheduler::currentData);
	switch (transient->GetLabel()) {
	case HOLE_LABEL:
	  Scheduler::currentData = Hole::holeExn;
	  goto raise;
	case FUTURE_LABEL:
	  taskStack->Purge();
	  currentThread->SetArgs(currentArgs);
	  currentThread->SetState(Thread::BLOCKED);
	  ((Future *) transient)->AddToWaitQueue(currentThread);
	  nextThread = true;
	  break;
	case CANCELLED_LABEL:
	  Scheduler::currentData = transient->GetArg();
	  goto raise;
	case BYNEED_LABEL: {
	  TaskStack *newTaskStack = TaskStack::New();
	  ByneedInterpreter::PushFrame(newTaskStack, transient);
	  NewThread(transient->GetArg(), Interpreter::EmptyArg(), newTaskStack);
	  transient->Become(FUTURE_LABEL, Store::IntToWord(0)); // empty queue
	  currentThread->SetArgs(currentArgs);
	  currentThread->Block(transient->ToWord());
	}
	  break;
	default:
	  Error("Scheduler::Run: invalid transient label");
	  break;
	}
      }
      break;
      case Interpreter::TERMINATE:
	taskStack->Clear(); // now subject to garbage collection
	currentThread->SetState(Thread::TERMINATED);
	nextThread = true;
	break;
      }
    }
    if (Store::NeedGC()) {
      //--** add threads waiting for I/O as well as properties
      threadQueue->PurgeAll();
      root = threadQueue->ToWord();
      RootSet::DoGarbageCollection();
      threadQueue = ThreadQueue::FromWordDirect(root);
    }
  }
  //--* select(...)
}
