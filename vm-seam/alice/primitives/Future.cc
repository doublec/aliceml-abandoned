//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/Authoring.hh"

DEFINE1(Future_alarmQuote) {
  DECLARE_INT(milliseconds, x0);
  if (milliseconds <= 0) RETURN_UNIT;
  RETURN(SignalHandler::RegisterAlarm(milliseconds)->ToWord());
} END

DEFINE1(Future_await) {
  if (Store::WordToTransient(x0) != INVALID_POINTER) {
    REQUEST(x0);
  } else {
    RETURN(x0);
  }
} END

DEFINE1(Future_byneed) {
  RETURN(Byneed::New(x0)->ToWord());
} END

DEFINE1(Future_concur) {
  Future *future = Future::New();
  Thread *thread = Scheduler::NewThread(0, Store::IntToWord(0));
  BindFutureWorker::PushFrame(thread, future);
  PushCallWorker::PushFrame(thread, x0);
  RETURN(future->ToWord());
} END

DEFINE1(Future_isByneed) {
  Transient *transient = Store::WordToTransient(x0);
  RETURN_BOOL(transient != INVALID_POINTER &&
	      transient->GetLabel() == BYNEED_LABEL);
} END

DEFINE1(Future_status) {
  Transient *transient = Store::WordToTransient(x0);
  if (transient == INVALID_POINTER)
    RETURN_INT(Types::DETERMINED);
  switch (transient->GetLabel()) {
  case HOLE_LABEL:
    RAISE(PrimitiveTable::Hole_Hole);
  case FUTURE_LABEL:
  case BYNEED_LABEL:
    RETURN_INT(Types::FUTURE);
  case CANCELLED_LABEL:
    RETURN_INT(Types::FAILED);
  default:
    Error("invalid transient label");
  }
} END

void PrimitiveTable::RegisterFuture() {
  PrimitiveTable::Future_Cyclic = Hole::cyclicExn;

  Register("Future.Cyclic", PrimitiveTable::Future_Cyclic);
  Register("Future.alarm'", Future_alarmQuote, 1);
  Register("Future.await", Future_await, 1);
  Register("Future.byneed", Future_byneed, 1);
  Register("Future.concur", Future_concur, 1);
  Register("Future.isByneed", Future_isByneed, 1);
  Register("Future.status", Future_status, 1);
}
