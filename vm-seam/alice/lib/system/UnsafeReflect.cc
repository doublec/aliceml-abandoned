//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/primitives/Authoring.hh"

static bool littleEndian;

union FloatChunk {
  unsigned char c[sizeof(double)];
  int i[sizeof(double) / sizeof(int)];
  double d;
};

DEFINE1(UnsafeReflect_cast) {
  RETURN(x0);
} END

DEFINE1(UnsafeReflect_realToVector) {
  DECLARE_REAL(r, x0);
  Vector *vector = Vector::New(sizeof(double));
  FloatChunk x;
  x.d = r->GetValue();
  if (littleEndian) {
    for (u_int i = 0; i < sizeof(double); i++)
      vector->Init(i, Store::IntToWord(x.c[sizeof(double) - 1 - i]));
  } else {
    for (u_int i = 0; i < sizeof(double); i++)
      vector->Init(i, Store::IntToWord(x.c[i]));
  }
  RETURN(vector->ToWord());
} END

DEFINE1(UnsafeReflect_Reflect) {
  DECLARE_RECORD(argRecord, x0);
  Record *record = Record::New(1);
  record->Init("x", argRecord->PolySel(UniqueString::New(String::New("X$"))));
  RETURN(record->ToWord());
} END

DEFINE1(UnsafeReflect_Unreflect) {
  DECLARE_RECORD(argRecord, x0);
  RETURN(argRecord->PolySel(UniqueString::New(String::New("x"))));
} END

DEFINE1(UnsafeReflect_ReflectSig) {
  DECLARE_RECORD(argRecord, x0);
  Record *record = Record::New(1);
  record->Init("x", argRecord->PolySel(UniqueString::New(String::New("$S$"))));
  RETURN(record->ToWord());
} END

DEFINE1(UnsafeReflect_UnreflectSig) {
  DECLARE_RECORD(argRecord, x0);
  Record *record = Record::New(1);
  record->Init("$S$", argRecord->PolySel(UniqueString::New(String::New("x"))));
  RETURN(record->ToWord());
} END

word UnsafeReflect() {
  FloatChunk x;
  x.i[0] = 1;
  for (u_int i = 1; i < sizeof(double) / sizeof(int); i++)
    x.i[i] = 0;
  if (x.c[0] == 1)
    littleEndian = true;
  else
    littleEndian = false;

  Record *record = Record::New(6);
  INIT_STRUCTURE(record, "UnsafeReflect", "cast",
		 UnsafeReflect_cast, 1, true);
  INIT_STRUCTURE(record, "UnsafeReflect", "realToVector",
		 UnsafeReflect_realToVector, 1, true);
  INIT_STRUCTURE(record, "UnsafeReflect", "Reflect$",
		 UnsafeReflect_Reflect, 1, true);
  INIT_STRUCTURE(record, "UnsafeReflect", "Unreflect$",
		 UnsafeReflect_Unreflect, 1, true);
  INIT_STRUCTURE(record, "UnsafeReflect", "ReflectSig$",
		 UnsafeReflect_ReflectSig, 1, true);
  INIT_STRUCTURE(record, "UnsafeReflect", "UnreflectSig$",
		 UnsafeReflect_UnreflectSig, 1, true);
  RETURN_STRUCTURE("UnsafeReflect$", record);
}
