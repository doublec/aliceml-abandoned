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

DEFINE2(Hole_fail) {
  Transient *transient = Store::WordToTransient(x0);
  if (transient == INVALID_POINTER || transient->GetLabel() != HOLE_LABEL)
    RAISE(PrimitiveTable::Hole_Hole);
  Transient *exnTransient = Store::WordToTransient(x1);
  if (exnTransient != INVALID_POINTER) REQUEST(exnTransient->ToWord());
  STATIC_CAST(Hole *, transient)->Fail(x1);
  RETURN_UNIT;
} END

DEFINE2(Hole_fill) {
  Transient *transient = Store::WordToTransient(x0);
  if (transient == INVALID_POINTER || transient->GetLabel() != HOLE_LABEL)
    RAISE(PrimitiveTable::Hole_Hole);
  Hole *hole = STATIC_CAST(Hole *, transient);
  if (!hole->Fill(x1)) {
    //hole->Fail(PrimitiveTable::Future_Cyclic);
    RAISE(PrimitiveTable::Future_Cyclic);
  }
  RETURN_UNIT;
} END

DEFINE1(Hole_future) {
  Transient *transient = Store::WordToTransient(x0);
  if (transient == INVALID_POINTER || transient->GetLabel() != HOLE_LABEL)
    //RAISE(PrimitiveTable::Hole_Hole);
    RETURN(x0);
  RETURN(STATIC_CAST(Hole *, transient)->GetFuture()->ToWord());
} END

DEFINE0(Hole_hole) {
  RETURN(Hole::New()->ToWord());
} END

DEFINE1(Hole_isHole) {
  Transient *transient = Store::WordToTransient(x0);
  RETURN_BOOL(transient != INVALID_POINTER &&
	      transient->GetLabel() == HOLE_LABEL);
} END

void PrimitiveTable::RegisterHole() {
  PrimitiveTable::Hole_Hole = Hole::holeExn;

  Register("Hole.Hole", PrimitiveTable::Hole_Hole);
  Register("Hole.fail", Hole_fail, 2);
  Register("Hole.fill", Hole_fill, 2);
  Register("Hole.future", Hole_future, 1);
  Register("Hole.hole", Hole_hole, 0);
  Register("Hole.isHole", Hole_isHole, 1);
}
