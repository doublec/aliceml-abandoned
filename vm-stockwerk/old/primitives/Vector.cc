//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "builtins/Authoring.hh"

DEFINE1(Vector_fromList) {
  DECLARE_LIST(tagVal, length, x0);
  Vector *vector = Vector::New(length);
  int i = 0;
  while (tagVal != INVALID_POINTER) {
    vector->Init(i++, tagVal->Sel(0));
    tagVal = TagVal::FromWord(tagVal->Sel(1));
  }
  RETURN(vector->ToWord());
} END

DEFINE1(Vector_length) {
  DECLARE_VECTOR(vector, x0);
  RETURN_INT(vector->GetLength());
} END

DEFINE2(Vector_sub) {
  DECLARE_VECTOR(vector, x0);
  DECLARE_INT(index, x1);
  if (index < 0 || index >= vector->GetLength())
    RAISE(GlobalPrimitives::General_Subscript);
  RETURN(vector->Sub(index));
} END

void Primitive::RegisterVector() {
  Register("Vector.fromList", Vector_fromList, 1);
  Register("Vector.maxLen", Store::IntToWord(0x3FFFFFFF));
  Register("Vector.length", Vector_length, 1);
  Register("Vector.sub", Vector_sub, 2);
  //--** tabulate
};
