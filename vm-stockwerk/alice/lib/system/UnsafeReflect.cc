//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "emulator/Authoring.hh"

DEFINE1(UnsafeReflect_cast) {
  RETURN(x0);
} END

DEFINE1(UnsafeReflect_realToVector) {
  Vector *vector = Vector::New(0);
  // to be done
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
  Tuple *t = Tuple::New(6);
  t->Init(0, Primitive::MakeFunction(UnsafeReflect_cast, 1));
  t->Init(1, Primitive::MakeFunction(UnsafeReflect_realToVector, 1));
  t->Init(2, Primitive::MakeFunction(UnsafeReflect_Reflect, 1));
  t->Init(3, Primitive::MakeFunction(UnsafeReflect_Unreflect, 1));
  t->Init(4, Primitive::MakeFunction(UnsafeReflect_ReflectSig, 1));
  t->Init(5, Primitive::MakeFunction(UnsafeReflect_UnreflectSig, 1));
  return t->ToWord();
}
