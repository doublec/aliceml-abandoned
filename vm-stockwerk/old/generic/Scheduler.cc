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

#include "Scheduler.hh"
#include "Thread.hh"
#include "TaskStack.hh"

void Scheduler::Timer() {
  preempt = true;
}

void Scheduler::Run() {
  Thread *thread;
  word data;
  //--** enable timer thread
  while ((thread = threadPool->Dequeue()) != INVALID_POINTER) {
    TaskStack *taskStack = thread->GetTaskStack();
  cont:
    Interpreter *interpreter = taskStack->GetInterpreter();
    int nargs = taskStack->GetInt(0);
    preempt = false;
    switch (interpreter->Run(nargs, taskStack, data)) {
    case Interpreter::CONTINUE:
      goto cont;
    case Interpreter::EXCEPTION:
      taskStack->PopToMark();
      if (taskStack->IsEmpty())
	; //--** invoke toplevel exception handler
      else
	; //--** handle exception given by `data'
      break;
    case Interpreter::PREEMPT:
      thread->UpdateTaskStack(taskStack);
      threadPool->Enqueue(thread);
      break;
    case Interpreter::SUSPEND:
      thread->UpdateTaskStack(taskStack);
      Store::WordToTransient(data)->AddThread(thread);
      break;
    case Interpreter::TERMINATE:
      break;
    }
    if (Store::NeedGC()) {
      //--** add threads waiting for I/O
      threadPool = Store::DoGC(threadPool->ToWord() /*--** #generations */);
    }
  }
  //--* select(...)
}
