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

#include "scheduler/Transients.hh"
#include "scheduler/Scheduler.hh"
#include "scheduler/Thread.hh"
#include "scheduler/TaskStack.hh"
#include "scheduler/Interpreter.hh"
#include "builtins/GlobalPrimitives.hh"

void Scheduler::Timer() {
  preempt = true;
}

void Scheduler::Run() {
  //--** start timer thread
  while ((currentThread = threadPool->Dequeue()) != INVALID_POINTER) {
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
	threadPool->Enqueue(currentThread);
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
	  currentThread->SetState(Thread::BLOCKED);
	  int nvars = result.nargs;
	  Assert(nvars > 0);
	  Transient *transient[nvars];
	  for (int i = nvars; i--; )
	    transient[i] = Store::WordToTransient(taskStack->GetWord(i));
	  taskStack->PopFrame(nvars);
	  for (int i = nvars; i--; ) {
	    switch (transient[i]->GetLabel()) {
	    HOLE:
	      taskStack->PushFrame(1);
	      taskStack->PutWord(0, GlobalPrimitives::Hole_Hole);
	      goto raise;
	    FUTURE:
	      taskStack->PushFrame(1);
	      taskStack->PutInt(0, 0);
	      static_cast<Future *>(transient[i])->
		AddToWaitQueue(currentThread);
	      break;
	    CANCELLED:
	      taskStack->PushFrame(1);
	      taskStack->PutWord(0, transient[i]->GetArg());
	      goto raise;
	    BYNEED:
	      //--** Perform application:
	      //--** How in hell can we do this without knowing what a
	      //--** closure is?
	      break;
	    default:
	      Assert(0);
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
      //--** add threads waiting for I/O as well as properties
      threadPool =
	ThreadPool::FromWord(Store::DoGC(threadPool->ToWord(),
					 storeConfig->max_gen - 1));
    }
  }
  //--* select(...)
}
