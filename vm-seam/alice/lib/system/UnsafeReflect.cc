//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/Authoring.hh"

DEFINE1(UnsafeReflect_cast) {
  RETURN(x0);
} END

DEFINE1(UnsafeReflect_realToVector) {
  DECLARE_REAL(r, x0);
  Word8Vector *vector = Word8Vector::New(sizeof(double));
  u_char *c = r->GetNetworkRepresentation();
  for (u_int i = sizeof(double); i--; )
    vector->Init(i, Store::IntToWord(c[i]));
  RETURN(vector->ToWord());
} END

DEFINE1(UnsafeReflect_Reflect) {
  DECLARE_RECORD(argRecord, x0);
  Record *record = Record::New(1);
  record->Init("x", argRecord->PolySel(UniqueString::New(String::New("X$"))));
  RETURN(record->ToWord());
} END

DEFINE1(UnsafeReflect_Reify) {
  DECLARE_RECORD(argRecord, x0);
  RETURN(argRecord->PolySel(UniqueString::New(String::New("x"))));
} END

DEFINE1(UnsafeReflect_ReflectSig) {
  DECLARE_RECORD(argRecord, x0);
  Record *record = Record::New(1);
  record->Init("x", argRecord->PolySel(UniqueString::New(String::New("$S$"))));
  RETURN(record->ToWord());
} END

DEFINE1(UnsafeReflect_ReifySig) {
  DECLARE_RECORD(argRecord, x0);
  Record *record = Record::New(1);
  record->Init("$S$", argRecord->PolySel(UniqueString::New(String::New("x"))));
  RETURN(record->ToWord());
} END

word UnsafeReflect() {
  Record *record = Record::New(6);
  INIT_STRUCTURE(record, "UnsafeReflect", "cast",
		 UnsafeReflect_cast, 1);
  INIT_STRUCTURE(record, "UnsafeReflect", "realToVector",
		 UnsafeReflect_realToVector, 1);
  INIT_STRUCTURE(record, "UnsafeReflect", "Reflect$",
		 UnsafeReflect_Reflect, 1);
  INIT_STRUCTURE(record, "UnsafeReflect", "Reify$",
		 UnsafeReflect_Reify, 1);
  INIT_STRUCTURE(record, "UnsafeReflect", "ReflectSig$",
		 UnsafeReflect_ReflectSig, 1);
  INIT_STRUCTURE(record, "UnsafeReflect", "ReifySig$",
		 UnsafeReflect_ReifySig, 1);
  RETURN_STRUCTURE("UnsafeReflect$", record);
}
