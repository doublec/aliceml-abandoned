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

#include "generic/Tuple.hh"
#include "generic/Closure.hh"
#include "alice/AliceConcreteCode.hh"
#include "alice/AliceLanguageLayer.hh"
#include "alice/Authoring.hh"

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

DEFINE1(Unsafe_getPrimitiveByName) {
  DECLARE_STRING(name, x0);
  RETURN(PrimitiveTable::LookupValue(static_cast<Chunk *>(name)));
} END

DEFINE1(Unsafe_makeConcreteCode) {
  DECLARE_TAGVAL(abstractCode, x0);
  RETURN(AliceLanguageLayer::concreteCodeConstructor(abstractCode));
} END

DEFINE2(Unsafe_makeClosure) {
  DECLARE_VECTOR(vector, x1);
  u_int nglobals = vector->GetLength();
  Closure *closure = Closure::New(x0, nglobals);
  for (u_int i = nglobals; i--; )
    closure->Init(i, vector->Sub(i));
  RETURN(closure->ToWord());
} END

DEFINE2(Unsafe_makeTaggedValue) {
  DECLARE_INT(tag, x0);
  DECLARE_VECTOR(vector, x1);
  u_int size = vector->GetLength();
  TagVal *tagVal = TagVal::New(tag, size);
  for (u_int i = size; i--; )
    tagVal->Init(i, vector->Sub(i));
  RETURN(tagVal->ToWord());
} END

DEFINE1(Unsafe_makeTuple) {
  DECLARE_VECTOR(vector, x0);
  u_int size = vector->GetLength();
  Tuple *tuple = Tuple::New(size);
  for (u_int i = size; i--; )
    tuple->Init(i, vector->Sub(i));
  RETURN(tuple->ToWord());
} END

DEFINE2(Unsafe_selRecord) {
  DECLARE_RECORD(record, x0);
  DECLARE_UNIQUE_STRING(label, x1);
  RETURN(record->PolySel(label));
} END

void PrimitiveTable::RegisterUnsafe() {
  Register("Unsafe.Array.sub", Unsafe_Array_sub, 2);
  Register("Unsafe.Array.update", Unsafe_Array_update, 3);
  Register("Unsafe.String.sub", Unsafe_String_sub, 2);
  Register("Unsafe.Vector.sub", Unsafe_Vector_sub, 2);
  Register("Unsafe.cast", Unsafe_cast, 1);
  Register("Unsafe.getPrimitiveByName", Unsafe_getPrimitiveByName, 1);
  Register("Unsafe.makeConcreteCode", Unsafe_makeConcreteCode, 1);
  Register("Unsafe.makeClosure", Unsafe_makeClosure, 2);
  Register("Unsafe.makeTaggedValue", Unsafe_makeTaggedValue, 2);
  Register("Unsafe.makeTuple", Unsafe_makeTuple, 1);
  Register("Unsafe.selRecord", Unsafe_selRecord, 2);
}
