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

DEFINE2(Unsafe_Array_sub) {
  DECLARE_ARRAY(array, x0);
  DECLARE_INT(index, x1);
  RETURN(array->Sub(index));
} END

DEFINE3(Unsafe_Array_update) {
  DECLARE_ARRAY(array, x0);
  DECLARE_INT(index, x1);
  array->Update(index, x2);
  RETURN_UNIT;
} END

DEFINE2(Unsafe_String_sub) {
  DECLARE_STRING(string, x0);
  DECLARE_INT(index, x1);
  RETURN_INT(string->GetValue()[index]);
} END

DEFINE2(Unsafe_Vector_sub) {
  DECLARE_VECTOR(vector, x0);
  DECLARE_INT(index, x1);
  RETURN(vector->Sub(index));
} END

DEFINE1(Unsafe_cast) {
  RETURN(x0);
} END

void Primitive::RegisterUnsafe() {
  Register("Unsafe.Array.sub", Unsafe_Array_sub);
  Register("Unsafe.Array.update", Unsafe_Array_update);
  Register("Unsafe.String.sub", Unsafe_String_sub);
  Register("Unsafe.Vector.sub", Unsafe_Vector_sub);
  Register("Unsafe.cast", Unsafe_cast);
};
