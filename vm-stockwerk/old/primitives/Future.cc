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
#include "scheduler/Closure.hh"
#include "scheduler/Interpreter.hh"
#include "scheduler/Scheduler.hh"
#include "builtins/Authoring.hh"

DEFINE1(Future_alarmQuote) {
  RETURN_UNIT; //--** not implemented
} END

DEFINE1(Future_await) {
  if (Store::WordToTransient(x0) != INVALID_POINTER) {
    REQUEST(x0);
  } else {
    RETURN(x0);
  }
} END

DEFINE2(Future_awaitOne) {
  Transient *transient1 = Store::WordToTransient(x0);
  Transient *transient2 = Store::WordToTransient(x1);
  // We might have been woken up after having added the currentThread to
  // both wait queues.  Thus we make sure that we remove ourselves from
  // the wait queue of the other transient if necessary.
  if (transient1 == INVALID_POINTER) {
    if (transient2 != INVALID_POINTER &&
	transient2->GetLabel() == FUTURE_LABEL)
      static_cast<Future *>(transient2)->
	RemoveFromWaitQueue(Scheduler::GetCurrentThread());
    RETURN(x0);
  }
  if (transient2 == INVALID_POINTER) {
    if (transient1->GetLabel() == FUTURE_LABEL)
      static_cast<Future *>(transient1)->
	RemoveFromWaitQueue(Scheduler::GetCurrentThread());
    RETURN(x0);
  }
  REQUEST2(x0, x1);
} END

DEFINE1(Future_byneed) {
  RETURN(Byneed::New(x0)->ToWord());
} END

DEFINE1(Future_concur) {
  word byneed = Byneed::New(x0)->ToWord();
  Thread *thread = Thread::New(Scheduler::GetCurrentThread()->GetPriority());
  TaskStack *newTaskStack = thread->GetTaskStack();
  // Push the exception handler and the mark:
  word primitive = GlobalPrimitives::Internal_defaultHandler;
  newTaskStack->PushCall(Closure::FromWordDirect(primitive));
  newTaskStack->PushFrame(1);
  newTaskStack->PutUnmanagedPointer(0, NULL);
  // Push a task that terminates the thread after evaluation:
  primitive = GlobalPrimitives::Internal_terminate;
  newTaskStack->PushCall(Closure::FromWordDirect(primitive));
  // Start the computation by requesting the by-need future:
  primitive = GlobalPrimitives::Future_await;
  newTaskStack->PushCall(Closure::FromWordDirect(primitive));
  newTaskStack->PushFrame(2);
  newTaskStack->PutWord(1, byneed);
  newTaskStack->PutInt(0, 1);
  Scheduler::AddThread(thread);
  RETURN(byneed);
} END

DEFINE1(Future_isFailed) {
  Transient *transient = Store::WordToTransient(x0);
  RETURN_BOOL(transient != INVALID_POINTER &&
	      transient->GetLabel() == CANCELLED_LABEL);
} END

DEFINE1(Future_isFuture) {
  Transient *transient = Store::WordToTransient(x0);
  RETURN_BOOL(transient != INVALID_POINTER &&
	      transient->GetLabel() == FUTURE_LABEL);
} END

void Primitive::RegisterFuture() {
  RegisterUniqueConstructor("Future.Future");
  Register("Future.alarm'", Future_alarmQuote, 1);
  Register("Future.await", Future_await, 1);
  Register("Future.awaitOne", Future_awaitOne, 2);
  Register("Future.byneed", Future_byneed, 1);
  Register("Future.concur", Future_concur, 1);
  Register("Future.isFailed", Future_isFailed, 1);
  Register("Future.isFuture", Future_isFuture, 1);
}
