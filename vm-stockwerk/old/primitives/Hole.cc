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

#include "generic/Transients.hh"
#include "alice/primitives/Authoring.hh"

DEFINE2(Hole_fail) {
  Transient *transient = Store::WordToTransient(x0);
  if (transient == INVALID_POINTER || transient->GetLabel() != HOLE_LABEL)
    RAISE(PrimitiveTable::Hole_Hole);
  Constructor *constructor =
    Constructor::FromWordDirect(PrimitiveTable::Future_Future);
  ConVal *exn = ConVal::New(constructor, 1);
  exn->Init(0, x1);
  transient->Become(CANCELLED_LABEL, exn->ToWord());
  RETURN_UNIT;
} END

DEFINE2(Hole_fill) {
  Transient *transient = Store::WordToTransient(x0);
  if (transient == INVALID_POINTER || transient->GetLabel() != HOLE_LABEL)
    RAISE(PrimitiveTable::Hole_Hole);
  if (!static_cast<Hole *>(transient)->Fill(x1))
    RAISE(PrimitiveTable::Hole_Cyclic);
  RETURN_UNIT;
} END

DEFINE1(Hole_future) {
  Transient *transient = Store::WordToTransient(x0);
  if (transient == INVALID_POINTER || transient->GetLabel() != HOLE_LABEL)
    RAISE(PrimitiveTable::Hole_Hole);
  RETURN(static_cast<Hole *>(transient)->GetFuture()->ToWord());
} END

DEFINE0(Hole_hole) {
  RETURN(Hole::New()->ToWord());
} END

DEFINE1(Hole_isFailed) {
  Transient *transient = Store::WordToTransient(x0);
  RETURN_BOOL(transient != INVALID_POINTER &&
	      transient->GetLabel() == CANCELLED_LABEL);
} END

DEFINE1(Hole_isHole) {
  Transient *transient = Store::WordToTransient(x0);
  RETURN_BOOL(transient != INVALID_POINTER &&
	      transient->GetLabel() == HOLE_LABEL);
} END

void PrimitiveTable::RegisterHole() {
  RegisterUniqueConstructor("Hole.Cyclic");
  RegisterUniqueConstructor("Hole.Hole");
  Register("Hole.fail", Hole_fail, 2);
  Register("Hole.fill", Hole_fill, 2);
  Register("Hole.future", Hole_future, -1);
  Register("Hole.hole", Hole_hole, 0);
  Register("Hole.isFailed", Hole_isFailed, -1);
  Register("Hole.isFailed", Hole_isHole, -1);
}
