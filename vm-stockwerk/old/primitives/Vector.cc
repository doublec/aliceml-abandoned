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
  if (length > Vector::maxLen)
    RAISE(GlobalPrimitives::General_Size);
  Vector *vector = Vector::New(length);
  u_int i = 0;
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
  if (index < 0 || static_cast<u_int>(index) >= vector->GetLength())
    RAISE(GlobalPrimitives::General_Subscript);
  RETURN(vector->Sub(index));
} END

DEFINE2(Vector_tabulate) {
  DECLARE_INT(length, x0);
  DECLARE_CLOSURE(closure, x1);
  if (length == 0)
    RETURN(Vector::New(0)->ToWord());
  if (length < 0 || static_cast<u_int>(length) > Vector::maxLen)
    RAISE(GlobalPrimitives::General_Size);
  taskStack->PushFrame(3);
  taskStack->PutInt(0, 0); // start index
  taskStack->PutWord(1, Vector::New(length)->ToWord());
  taskStack->PutWord(2, closure->ToWord());
  taskStack->
    PushCall(Closure::FromWordDirect(GlobalPrimitives::Vector_tabulate_cont));
  taskStack->PushCall(closure);
  RETURN_INT(0); // applying the closure for the first time
} END

DEFINE1(Vector_tabulate_cont) {
  u_int index = Store::DirectWordToInt(taskStack->GetWord(0));
  Vector *vector = Vector::FromWordDirect(taskStack->GetWord(1));
  Closure *closure = Closure::FromWordDirect(taskStack->GetWord(2));
  vector->Replace(index, x0);
  index++;
  if (index == vector->GetLength())
    RETURN(vector->ToWord());
  taskStack->PushFrame(3);
  taskStack->PutInt(0, index);
  taskStack->PutWord(1, vector->ToWord());
  taskStack->PutWord(2, closure->ToWord());
  taskStack->
    PushCall(Closure::FromWordDirect(GlobalPrimitives::Vector_tabulate_cont));
  taskStack->PushCall(closure);
  RETURN_INT(index); // applying the closure
} END

void Primitive::RegisterVector() {
  Register("Vector.fromList", Vector_fromList, 1);
  Register("Vector.maxLen", Store::IntToWord(Vector::maxLen));
  Register("Vector.length", Vector_length, 1);
  Register("Vector.sub", Vector_sub, 2);
  Register("Vector.tabulate", Vector_tabulate, 2);
  Register("Vector.tabulate/cont", Vector_tabulate_cont, 1, 3);
}
