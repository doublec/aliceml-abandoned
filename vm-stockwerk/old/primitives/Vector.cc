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

#include "alice/primitives/Authoring.hh"

DEFINE1(Vector_fromList) {
  DECLARE_LIST(tagVal, length, x0);
  if (length > Vector::maxLen)
    RAISE(PrimitiveTable::General_Size);
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
    RAISE(PrimitiveTable::General_Subscript);
  RETURN(vector->Sub(index));
} END

DEFINE2(Vector_tabulate) { // NON-ABSTRACT TASK STACK USE
  DECLARE_INT(length, x0);
  DECLARE_CLOSURE(closure, x1);
  if (length == 0)
    RETURN(Vector::New(0)->ToWord());
  if (length < 0 || static_cast<u_int>(length) > Vector::maxLen)
    RAISE(PrimitiveTable::General_Size);
  taskStack->PushFrame(3 - 1); // overwrite the Interpreter slot
  taskStack->PutInt(0, 0); // start index
  taskStack->PutWord(1, Vector::New(length)->ToWord());
  taskStack->PutWord(2, closure->ToWord());
  taskStack->
    PushCall(Closure::FromWordDirect(PrimitiveTable::Vector_tabulate_cont));
  taskStack->PushCall(closure);
  taskStack->PushFrame(1); // RETURN_INT assumes a free slot
  RETURN_INT(0); // applying the closure for the first time
} END

DEFINE1(Vector_tabulate_cont) { // NON-ABSTRACT TASK STACK USE
  u_int index = Store::DirectWordToInt(taskStack->GetWord(1));
  Vector *vector = Vector::FromWordDirect(taskStack->GetWord(2));
  Closure *closure = Closure::FromWordDirect(taskStack->GetWord(3));
  vector->Replace(index, x0);
  index++;
  if (index == vector->GetLength()) {
    taskStack->PopFrame(3);
    RETURN(vector->ToWord());
  }
  taskStack->PopFrame(1); // pop the Interpreter
  taskStack->PutInt(0, index);
  taskStack->PutWord(1, vector->ToWord());
  taskStack->PutWord(2, closure->ToWord());
  taskStack->
    PushCall(Closure::FromWordDirect(PrimitiveTable::Vector_tabulate_cont));
  taskStack->PushCall(closure);
  taskStack->PushFrame(1); // RETURN_INT assumes a free slot
  RETURN_INT(index); // applying the closure
} END

void PrimitiveTable::RegisterVector() {
  Register("Vector.fromList", Vector_fromList, -1);
  Register("Vector.maxLen", Store::IntToWord(Vector::maxLen));
  Register("Vector.length", Vector_length, -1);
  Register("Vector.sub", Vector_sub, 2);
  Register("Vector.tabulate", Vector_tabulate, 2);
  Register("Vector.tabulate/cont", ::Vector_tabulate_cont, -1, 3);
}
