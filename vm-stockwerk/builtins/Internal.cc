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

#include "builtins/Authoring.hh"

DEFINE1(Internal_applyUnit) {
  DECLARE_CLOSURE(closure, x0);
  taskStack->PushCall(closure);
  RETURN_UNIT;
} END

DEFINE1(Internal_bind) {
  Transient *transient = Store::WordToTransient(taskStack->GetWord(0));
  taskStack->PopFrame(1);
  transient->Become(REF_LABEL, x0);
  RETURN(x0);
} END

DEFINE1(Internal_byneedHandler) {
  Constructor *constructor =
    Constructor::FromWordDirect(GlobalPrimitives::Future_Future);
  ConVal *exn = ConVal::New(constructor, 1);
  exn->Init(0, x0);
  Transient *result = Store::AllocTransient(CANCELLED_LABEL);
  result->InitArg(exn->ToWord());
  RETURN(result->ToWord());
} END

DEFINE1(Internal_defaultHandler) {
  //--** print out information about the unhandled exception
  return Interpreter::Result(Interpreter::Result::TERMINATE);
} END

DEFINE1(Internal_popHandler) {
  Assert(taskStack->GetUnmanagedPointer(0) == NULL);
  taskStack->PopFrame(1);
  RETURN(x0);
} END

DEFINE1(Internal_raise) {
  RAISE(x0);
} END

DEFINE1(Internal_terminate) {
  return Interpreter::Result(Interpreter::Result::TERMINATE);
} END

void Primitive::RegisterInternal() {
  Register("Internal.applyUnit", Internal_applyUnit, 1);
  Register("Internal.bind", Internal_bind, 1, 1);
  Register("Internal.byneedHandler", Internal_byneedHandler, 1);
  Register("Internal.defaultHandler", Internal_defaultHandler, 1);
  Register("Internal.popHandler", Internal_popHandler, 1);
  Register("Internal.raise", Internal_raise, 1);
  Register("Internal.terminate", Internal_terminate, 1);
}
