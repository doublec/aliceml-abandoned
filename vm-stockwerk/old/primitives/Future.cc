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
  if (transient1 == INVALID_POINTER || transient2 == INVALID_POINTER)
    RETURN(x0);
  taskStack->PushFrame(2);
  taskStack->PutWord(0, transient1->ToWord());
  taskStack->PutWord(1, transient2->ToWord());
  return Interpreter::Result(Interpreter::Result::REQUEST, 2);
} END

DEFINE1(Future_byneed) {
  RETURN(Byneed::New(x0)->ToWord());
} END

DEFINE1(Future_concur) {
  word byneed = Byneed::New(x0)->ToWord();
  Thread *thread = Thread::New(Scheduler::GetCurrentThread()->GetPriority());
  TaskStack *newTaskStack = thread->GetTaskStack();
  //--** push toplevel exception handler on taskStack
  newTaskStack->PushCall(Closure::FromWord(GlobalPrimitives::Future_await));
  newTaskStack->PushFrame(1);
  newTaskStack->PutWord(0, byneed);
  Scheduler::AddThread(thread);
  RETURN(byneed);
} END

DEFINE1(Future_isFailed) {
  Transient *transient = Store::WordToTransient(x0);
  RETURN_BOOL(transient != INVALID_POINTER &&
	      transient->GetLabel() == CANCELLED);
} END

DEFINE1(Future_isFuture) {
  Transient *transient = Store::WordToTransient(x0);
  RETURN_BOOL(transient != INVALID_POINTER &&
	      transient->GetLabel() == FUTURE);
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
