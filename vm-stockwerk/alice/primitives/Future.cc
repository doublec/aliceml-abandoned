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

#include "generic/Transients.hh"
#include "generic/Closure.hh"
#include "generic/BindFutureWorker.hh"
#include "generic/PushCallWorker.hh"
#include "generic/Scheduler.hh"
#include "generic/SignalHandler.hh"
#include "alice/Authoring.hh"

DEFINE1(Future_alarmQuote) {
  DECLARE_INT(microseconds, x0);
  // RegisterAlarm expects milliseconds
  RETURN(SignalHandler::RegisterAlarm(microseconds / 1000)->ToWord());
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
    RETURN_INT(0); // DETERMINED
  switch (transient->GetLabel()) {
  case HOLE_LABEL:
  case FUTURE_LABEL:
  case BYNEED_LABEL:
    RETURN_INT(2); // FUTURE
  case CANCELLED_LABEL:
    RETURN_INT(1); // FAILED
  default:
    Error("invalid transient label");
  }
} END

void PrimitiveTable::RegisterFuture() {
  Register("Future.alarm'", Future_alarmQuote, 1);
  Register("Future.await", Future_await, 1);
  Register("Future.byneed", Future_byneed, 1);
  Register("Future.concur", Future_concur, 1);
  Register("Future.isByneed", Future_isByneed, 1);
  Register("Future.status", Future_status, 1);
}
