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
#include "builtins/Authoring.hh"

DEFINE2(Hole_fail) {
  Transient *transient = Store::WordToTransient(x0);
  if (transient == INVALID_POINTER || transient->GetLabel() != HOLE)
    RAISE(GlobalPrimitives::Hole_Hole);
  ConVal *exn = 
    ConVal::New(Constructor::FromWord(GlobalPrimitives::Future_Future), 1);
  exn->Init(0, x1);
  transient->Become(CANCELLED, exn->ToWord());
  RETURN_UNIT;
} END

DEFINE2(Hole_fill) {
  Transient *transient = Store::WordToTransient(x0);
  if (transient == INVALID_POINTER || transient->GetLabel() != HOLE)
    RAISE(GlobalPrimitives::Hole_Hole);
  if (!static_cast<Hole *>(transient)->Fill(x1))
    RAISE(GlobalPrimitives::Hole_Cyclic);
  RETURN_UNIT;
} END

DEFINE1(Hole_future) {
  Transient *transient = Store::WordToTransient(x0);
  if (transient == INVALID_POINTER || transient->GetLabel() != HOLE)
    RAISE(GlobalPrimitives::Hole_Hole);
  RETURN(static_cast<Hole *>(transient)->GetFuture()->ToWord());
} END

DEFINE0(Hole_hole) {
  RETURN(Hole::New()->ToWord());
} END

DEFINE1(Hole_isFailed) {
  Transient *transient = Store::WordToTransient(x0);
  RETURN_BOOL(transient != INVALID_POINTER &&
	      transient->GetLabel() == CANCELLED);
} END

DEFINE1(Hole_isHole) {
  Transient *transient = Store::WordToTransient(x0);
  RETURN_BOOL(transient != INVALID_POINTER &&
	      transient->GetLabel() == HOLE);
} END

void Primitive::RegisterHole() {
  Register("Hole.Cyclic", Constructor::New()->ToWord()); //--** unique
  Register("Hole.Hole", Constructor::New()->ToWord()); //--** unique
  Register("Hole.fail", Hole_fail, 2);
  Register("Hole.fill", Hole_fill, 2);
  Register("Hole.future", Hole_future, 1);
  Register("Hole.hole", Hole_hole, 0);
  Register("Hole.isFailed", Hole_isFailed, 1);
  Register("Hole.isFailed", Hole_isHole, 1);
};
