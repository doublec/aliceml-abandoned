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
#pragma implementation "scheduler/Scheduler.hh"
#endif

#include "scheduler/Transients.hh"
#include "scheduler/Interpreter.hh"
#include "builtins/GlobalPrimitives.hh"

ThreadQueue *Scheduler::threadQueue;
Thread *Scheduler::currentThread;
bool Scheduler::preempt;

void Scheduler::Timer() {
  preempt = true;
}

void Scheduler::Init() {
  threadQueue = ThreadQueue::New();
}

//--** document representation of inactive threads (invariants)

void Scheduler::Run() {
  //--** start timer thread
  while ((currentThread = threadQueue->Dequeue()) != INVALID_POINTER) {
    Assert(currentThread->GetState() == Thread::RUNNABLE);
    Assert(!currentThread->IsSuspended());
    TaskStack *taskStack = currentThread->GetTaskStack();
    int nargs = taskStack->GetInt(0);
    taskStack->PopFrame(1);
    Interpreter::Result result(Interpreter::Result::CONTINUE, nargs);
    while (result.code == Interpreter::Result::CONTINUE) {
      int offset = nargs == -1? 1: nargs;
      Interpreter *interpreter =
	static_cast<Interpreter *>(taskStack->GetUnmanagedPointer(offset));
      Assert(interpreter != NULL);
      preempt = false;
      //--** reset time slice?
      result = interpreter->Run(taskStack, nargs);
      switch (result.code) {
      case Interpreter::Result::CONTINUE:
	nargs = result.nargs;
	break;
      case Interpreter::Result::PREEMPT:
	taskStack->PushFrame(1);
	taskStack->PutInt(0, result.nargs);
	threadQueue->Enqueue(currentThread);
	break;
      case Interpreter::Result::RAISE:
      raise:
	{
	  word exn = taskStack->GetWord(0);
	  taskStack->PopFrame(1);
	  while (true) {
	    Assert(!taskStack->IsEmpty());
	    interpreter =
	      static_cast<Interpreter *>(taskStack->GetUnmanagedPointer(0));
	    if (interpreter == NULL) {
	      // This is a mark that an exception handler follows.
	      // We require that there always is one that will finally
	      // handle the exception.
	      taskStack->PutWord(0, exn);
	      break;
	    }
	    interpreter->PopFrame(taskStack);
	  }
	}
	break;
      case Interpreter::Result::REQUEST:
	{
	  int nvars = result.nargs;
	  Assert(nvars > 0);
	  Transient *transients[nvars];
	  for (int i = nvars; i--; )
	    transients[i] = Store::WordToTransient(taskStack->GetWord(i));
	  taskStack->PopFrame(nvars);
	  for (int i = nvars; i--; ) {
	    Transient *transient = transients[i];
	    switch (transient->GetLabel()) {
	    case HOLE_LABEL:
	      taskStack->PushFrame(1);
	      taskStack->PutWord(0, GlobalPrimitives::Hole_Hole);
	      goto raise;
	    case FUTURE_LABEL:
	      taskStack->PushFrame(1);
	      taskStack->PutInt(0, 0);
	      static_cast<Future *>(transient)->AddToWaitQueue(currentThread);
	      break;
	    case CANCELLED_LABEL:
	      taskStack->PushFrame(1);
	      taskStack->PutWord(0, transient->GetArg());
	      goto raise;
	    case BYNEED_LABEL:
	      {
		word closure = transient->GetArg();
		transient->Become(FUTURE_LABEL, 0); // empty queue
		// Push a task that binds the transient:
		word primitive = GlobalPrimitives::Internal_bind;
		taskStack->PushFrame(1);
		taskStack->PutWord(0, transient->ToWord());
		taskStack->PushCall(Closure::FromWordDirect(primitive));
		// Push the exception handler and the mark:
		primitive = GlobalPrimitives::Internal_byneedHandler;
		taskStack->PushCall(Closure::FromWordDirect(primitive));
		taskStack->PushFrame(1);
		taskStack->PutUnmanagedPointer(0, NULL);
		// Push a task that pops the handler after the application:
		primitive = GlobalPrimitives::Internal_popHandler;
		taskStack->PushCall(Closure::FromWordDirect(primitive));
		// Push a task that applies the closure then run it:
		primitive = GlobalPrimitives::Internal_applyUnit;
		taskStack->PushCall(Closure::FromWordDirect(primitive));
		taskStack->PushFrame(1);
		taskStack->PutWord(0, closure);
		nargs = 1;
	      }
	      break;
	    default:
	      Error("Scheduler::Run: invalid transient label");
	      break;
	    }
	  }
	}
	break;
      case Interpreter::Result::TERMINATE:
	taskStack->Clear(); // now subject to garbage collection
	currentThread->SetState(Thread::TERMINATED);
	break;
      }
    }
    if (Store::NeedGC()) {
      //--** add Primitive::table
      //--** add threads waiting for I/O as well as properties
      threadQueue->PurgeAll();
      threadQueue =
	ThreadQueue::FromWordDirect(Store::DoGC(threadQueue->ToWord()));
    }
  }
  //--* select(...)
}
