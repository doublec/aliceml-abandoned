//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/Authoring.hh"

DEFINE2(Array_array) {
  DECLARE_INT(length, x0);
  if (static_cast<u_int>(length) > Array::maxLen)
    RAISE(PrimitiveTable::General_Size);
  Array *array = Array::New(length);
  for (u_int i = length; i--; )
    array->Init(i, x1);
  RETURN(array->ToWord());
} END

DEFINE1(Array_fromList) {
  DECLARE_LIST(tagVal, length, x0);
  if (length > Array::maxLen)
    RAISE(PrimitiveTable::General_Size);
  Array *array = Array::New(length);
  u_int i = 0;
  while (tagVal != INVALID_POINTER) {
    array->Init(i++, tagVal->Sel(0));
    tagVal = TagVal::FromWord(tagVal->Sel(1));
  }
  Assert(i == length);
  RETURN(array->ToWord());
} END

DEFINE1(Array_length) {
  DECLARE_ARRAY(array, x0);
  RETURN_INT(array->GetLength());
} END

DEFINE2(Array_sub) {
  DECLARE_ARRAY(array, x0);
  DECLARE_INT(index, x1);
  if (static_cast<u_int>(index) >= array->GetLength())
    RAISE(PrimitiveTable::General_Subscript);
  RETURN(array->Sub(index));
} END

DEFINE3(Array_update) {
  DECLARE_ARRAY(array, x0);
  DECLARE_INT(index, x1);
  if (static_cast<u_int>(index) >= array->GetLength())
    RAISE(PrimitiveTable::General_Subscript);
  array->Update(index, x2);
  RETURN_UNIT;
} END

void PrimitiveTable::RegisterArray() {
  Register("Array.array", Array_array, 2);
  Register("Array.fromList", Array_fromList, 1);
  Register("Array.length", Array_length, 1);
  Register("Array.maxLen", Store::IntToWord(Array::maxLen));
  Register("Array.sub", Array_sub, 2);
  Register("Array.update", Array_update, 3);
}
