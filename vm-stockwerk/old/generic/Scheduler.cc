//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "generic/Scheduler.hh"
#endif

#include "generic/RootSet.hh"
#include "generic/TaskManager.hh"
#include "generic/Transients.hh"
#include "generic/InternalTasks.hh"

word Scheduler::root;
ThreadQueue *Scheduler::threadQueue;
Thread *Scheduler::currentThread;
bool Scheduler::preempt;

void Scheduler::Timer() {
  preempt = true;
}

void Scheduler::Init() {
  threadQueue = ThreadQueue::New();
  RootSet::Add(root);
}

//--** document invariants of thread representation

void Scheduler::Run() {
  //--** start timer thread
  while ((currentThread = threadQueue->Dequeue()) != INVALID_POINTER) {
    Assert(currentThread->GetState() == Thread::RUNNABLE);
    Assert(!currentThread->IsSuspended());
    TaskStack *taskStack = currentThread->GetTaskStack();
    int nargs = taskStack->GetInt(0);
    taskStack->PopFrame(1);
    bool nextThread = false;
    while (!nextThread) {
      int offset = nargs == -1? 1: nargs;
      TaskManager *taskManager =
	static_cast<TaskManager *>(taskStack->GetUnmanagedPointer(offset));
      preempt = false;
      TaskManager::Result result = taskManager->Run(taskStack, nargs);
    interpretResult:
      switch (result.code) {
      case TaskManager::Result::CONTINUE:
	nargs = result.nargs;
	break;
      case TaskManager::Result::PREEMPT:
	taskStack->PushFrame(1);
	taskStack->PutInt(0, result.nargs);
	threadQueue->Enqueue(currentThread);
	nextThread = true;
	break;
      case TaskManager::Result::RAISE:
      raise:
	taskManager =
	  static_cast<TaskManager *>(taskStack->GetUnmanagedPointer(1));
	result = taskManager->Handle(taskStack);
	goto interpretResult;
      case TaskManager::Result::REQUEST:
	{
	  int nvars = result.nargs;
	  Assert(nvars > 0);
	  Transient *transients[nvars];
	  for (int i = nvars; i--; )
	    transients[i] = Store::WordToTransient(taskStack->GetWord(i));
	  taskStack->PopFrame(nvars);
	  //--** this is wrong if nvars > 1 and there's at least one byneed
	  for (int i = nvars; i--; ) {
	    Transient *transient = transients[i];
	    switch (transient->GetLabel()) {
	    case HOLE_LABEL:
	      taskStack->PushFrame(1);
	      taskStack->PutWord(0, Hole::holeExn);
	      goto raise;
	    case FUTURE_LABEL:
	      taskStack->PushFrame(1);
	      taskStack->PutInt(0, 0); // nargs
	      taskStack->Purge();
	      currentThread->SetState(Thread::BLOCKED);
	      static_cast<Future *>(transient)->AddToWaitQueue(currentThread);
	      nextThread = true;
	      break;
	    case CANCELLED_LABEL:
	      taskStack->PushFrame(1);
	      taskStack->PutWord(0, transient->GetArg());
	      goto raise;
	    case BYNEED_LABEL:
	      taskStack->
		PushCall(Closure::FromWordDirect(transient->GetArg()));
	      transient->Become(FUTURE_LABEL, 0); // `0' means queue empty
	      nargs = 0;
	      break;
	    default:
	      Error("Scheduler::Run: invalid transient label");
	      break;
	    }
	  }
	}
	break;
      case TaskManager::Result::TERMINATE:
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
