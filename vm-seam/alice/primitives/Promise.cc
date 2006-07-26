//
// Author:
//   Andreas Rossberg <rossberg@ps.uni-sb.de>
//
// Copyright:
//   Andreas Rossberg, 2006
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/Authoring.hh"

word PromiseConstructor;

DEFINE2(Promise_fail) {
  DECLARE_PROMISE(promise, x0);
  Transient *exnTransient = Store::WordToTransient(x1);
  if (exnTransient != INVALID_POINTER) REQUEST(exnTransient->ToWord());
  if (!promise->Fail(x1))
    RAISE(PrimitiveTable::Promise_Promise);
  RETURN_UNIT;
} END

DEFINE2(Promise_fulfill) {
  DECLARE_PROMISE(promise, x0);
  if (!promise->Fulfill(x1))
    RAISE(PrimitiveTable::Promise_Promise);
  RETURN_UNIT;
} END

DEFINE1(Promise_future) {
  DECLARE_PROMISE(promise, x0);
  RETURN(promise->GetFuture());
} END

DEFINE1(Promise_isFulfilled) {
  DECLARE_PROMISE(promise, x0);
  RETURN_BOOL(promise->IsFulfilled());
} END

DEFINE0(Promise_promise) {
  RETURN(Promise::New()->ToWord());
} END

void PrimitiveTable::RegisterPromise() {
  PrimitiveTable::Promise_Promise =
    UniqueConstructor::New("Promise", "Promise.Promise")->ToWord();

  Register("Promise.Promise", PrimitiveTable::Promise_Promise);
  Register("Promise.fail", Promise_fail, 2);
  Register("Promise.fulfill", Promise_fulfill, 2);
  Register("Promise.future", Promise_future, 1);
  Register("Promise.isFulfilled", Promise_isFulfilled, 1);
  Register("Promise.promise", Promise_promise, 0);
}
