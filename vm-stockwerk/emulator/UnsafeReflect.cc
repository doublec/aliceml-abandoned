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

#include "emulator/Authoring.hh"

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
  DECLARE_TUPLE(argTuple, x0);
  Tuple *tuple = Tuple::New(1);
  tuple->Init(0, argTuple->Sel(1));
  RETURN(tuple->ToWord());
} END

DEFINE1(UnsafeReflect_Unreflect) {
  DECLARE_TUPLE(argTuple, x0);
  RETURN(argTuple->Sel(1));
} END

DEFINE1(UnsafeReflect_ReflectSig) {
  DECLARE_TUPLE(argTuple, x0);
  Tuple *tuple = Tuple::New(1);
  tuple->Init(0, argTuple->Sel(0));
  RETURN(tuple->ToWord());
} END

DEFINE1(UnsafeReflect_UnreflectSig) {
  DECLARE_TUPLE(argTuple, x0);
  Tuple *tuple = Tuple::New(1);
  tuple->Init(0, argTuple->Sel(0));
  RETURN(tuple->ToWord());
} END

word UnsafeReflect(void) {
  FloatChunk x;
  x.i[0] = 1;
  for (u_int i = 1; i < sizeof(double) / sizeof(int); i++)
    x.i[i] = 0;
  if (x.c[0] == 1)
    littleEndian = true;
  else
    littleEndian = false;

  Tuple *t = Tuple::New(6);
  t->Init(0, Primitive::MakeClosure("UnsafeReflect_Reflect",
				    UnsafeReflect_Reflect, 1, true));
  t->Init(1, Primitive::MakeClosure("UnsafeReflect_ReflectSig",
				    UnsafeReflect_ReflectSig, 1, true));
  t->Init(2, Primitive::MakeClosure("UnsafeReflect_Unreflect",
				    UnsafeReflect_Unreflect, 1, true));
  t->Init(3, Primitive::MakeClosure("UnsafeReflect_UnreflectSig",
				    UnsafeReflect_UnreflectSig, 1, true));
  t->Init(4, Primitive::MakeClosure("UnsafeReflect_cast",
				    UnsafeReflect_cast, 1, true));
  t->Init(5, Primitive::MakeClosure("UnsafeReflect_realToVector",
				    UnsafeReflect_realToVector, 1, true));
  RETURN_STRUCTURE(t);
}
