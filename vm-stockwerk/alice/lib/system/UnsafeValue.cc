//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "generic/Tuple.hh"
#include "generic/ConcreteCode.hh"
#include "alice/Authoring.hh"

DEFINE1(UnsafeValue_cast) {
  RETURN(x0);
} END

DEFINE2(UnsafeValue_same) {
  RETURN_BOOL(PointerOp::Deref(x0) == PointerOp::Deref(x1));
} END

DEFINE3(UnsafeValue_proj) {
  DECLARE_TUPLE(record, x0);
  x1 = x1; // ignored
  DECLARE_INT(i, x2);
  RETURN(record->Sel(i));
} END

DEFINE2(UnsafeValue_tag) {
  TagVal *tagVal = TagVal::FromWord(x0);
  x1 = x1; // ignored
  if (tagVal == INVALID_POINTER) {
    s_int i = Store::WordToInt(x0);
    if (i == INVALID_INT) REQUEST(x0);
    RETURN_INT(i);
  } else {
    RETURN_INT(tagVal->GetTag());
  }
} END

DEFINE3(UnsafeValue_projTagged) {
  DECLARE_TAGVAL(tagVal, x0);
  x1 = x1; // ignored
  DECLARE_INT(i, x2);
  RETURN(tagVal->Sel(i));
} END

DEFINE1(UnsafeValue_con) {
  DECLARE_CONVAL(conVal, x0);
  Constructor *constructor =
    conVal->IsConVal()? conVal->GetConstructor():
    static_cast<Constructor *>(static_cast<Block *>(conVal));
  RETURN(constructor->ToWord());
} END

DEFINE3(UnsafeValue_projConstructed) {
  DECLARE_CONVAL(conVal, x0);
  x1 = x1; // ignored
  DECLARE_INT(i, x2);
  Assert(conVal->IsConVal());
  RETURN(conVal->Sel(i));
} END

DEFINE2(UnsafeValue_projPoly) {
  DECLARE_RECORD(record, x0);
  DECLARE_TAGVAL(tagVal, x1);
  switch (tagVal->GetTag()) {
  case Types::ALPHA:
    {
      word wLabel = tagVal->Sel(0);
      DECLARE_STRING(label, wLabel);
      RETURN(record->PolySel(UniqueString::New(label)));
    }
  case Types::NUM:
    Error("UnsafeValue.projPoly: numeric labels not supported");
  default:
    Error("UnsafeValue.projPoly: unknown tag");
  }
} END

DEFINE1(UnsafeValue_prod) {
  DECLARE_VECTOR(labelValueVec, x0);
  u_int length = labelValueVec->GetLength();
  if (length == 0) RETURN_UNIT;
  Tuple *tuple = Tuple::New(length);
  for (u_int i = length; i--; ) {
    Tuple *labelValuePair = Tuple::FromWord(labelValueVec->Sub(i));
    if (labelValuePair == INVALID_POINTER) REQUEST(labelValueVec->Sub(i));
    tuple->Init(i, labelValuePair->Sel(1));
  }
  RETURN(tuple->ToWord());
} END

DEFINE1(UnsafeValue_tuple) {
  DECLARE_VECTOR(values, x0);
  u_int length = values->GetLength();
  if (length == 0) RETURN_UNIT;
  Tuple *tuple = Tuple::New(length);
  for (u_int i = length; i--; )
    tuple->Init(i, values->Sub(i));
  RETURN(tuple->ToWord());
} END

DEFINE3(UnsafeValue_tagged) {
  x0 = x0; // ignored
  DECLARE_INT(tag, x1);
  DECLARE_VECTOR(labelValueVec, x2);
  u_int length = labelValueVec->GetLength();
  if (length == 0) RETURN_INT(tag);
  TagVal *tagVal = TagVal::New(tag, length);
  for (u_int i = length; i--; ) {
    Tuple *labelValuePair = Tuple::FromWord(labelValueVec->Sub(i));
    if (labelValuePair == INVALID_POINTER) REQUEST(labelValueVec->Sub(i));
    tagVal->Init(i, labelValuePair->Sel(1));
  }
  RETURN(tagVal->ToWord());
} END

DEFINE3(UnsafeValue_taggedTuple) {
  x0 = x0; // ignored
  DECLARE_INT(tag, x1);
  DECLARE_VECTOR(values, x2);
  u_int length = values->GetLength();
  if (length == 0) RETURN_INT(tag);
  TagVal *tagVal = TagVal::New(tag, length);
  for (u_int i = length; i--; )
    tagVal->Init(i, values->Sub(i));
  RETURN(tagVal->ToWord());
} END

DEFINE2(UnsafeValue_closure) {
  DECLARE_TAGVAL(abstractCode, x0);
  DECLARE_VECTOR(vector, x1);
  u_int nglobals = vector->GetLength();
  word wConcreteCode =
    AliceLanguageLayer::concreteCodeConstructor(abstractCode);
  Closure *closure = Closure::New(wConcreteCode, nglobals);
  for (u_int i = nglobals; i--; )
    closure->Init(i, vector->Sub(i));
  RETURN(closure->ToWord());
} END

DEFINE1(UnsafeValue_prim) {
  DECLARE_STRING(name, x0);
  RETURN(PrimitiveTable::LookupValue(static_cast<Chunk *>(name)));
} END

DEFINE1(UnsafeValue_conName) {
  DECLARE_CONSTRUCTOR(constructor, x0);
  String *name = constructor->GetName();
  TagVal *exId = TagVal::New(Types::ExId, 1);
  exId->Init(0, name->ToWord());
  RETURN(exId->ToWord());
} END

DEFINE2(UnsafeValue_inArity) {
  DECLARE_BOOL(eager, x1);
  Closure *closure = Closure::FromWord(x0);
  if (closure == INVALID_POINTER) {
    if (!eager) RETURN_INT(-2);
    REQUEST(x0);
  }
  word wConcreteCode = closure->GetConcreteCode();
  ConcreteCode *concreteCode = ConcreteCode::FromWord(wConcreteCode);
  if (concreteCode == INVALID_POINTER) {
    if (!eager) RETURN_INT(-2);
    REQUEST(wConcreteCode);
  }
  Interpreter *interpreter = concreteCode->GetInterpreter();
  u_int arity = interpreter->GetInArity(concreteCode);
  if (arity == static_cast<u_int>(INVALID_INT))
    RETURN_INT(-2);
  RETURN_INT(arity == Scheduler::ONE_ARG? -1: static_cast<s_int>(arity));
} END

DEFINE2(UnsafeValue_outArity) {
  x0 = x0; x1 = x1; // ignored
  RETURN_INT(-2); //--** try to do better
} END

word UnsafeValue() {
  Record *record = Record::New(20);
  INIT_STRUCTURE(record, "UnsafeValue", "cast",
		 UnsafeValue_cast, 1, true);
  INIT_STRUCTURE(record, "UnsafeValue", "same",
		 UnsafeValue_same, 2, true);
  INIT_STRUCTURE(record, "UnsafeValue", "proj",
		 UnsafeValue_proj, 3, true);
  INIT_STRUCTURE(record, "UnsafeValue", "projTuple",
		 UnsafeValue_proj, 3, true);
  INIT_STRUCTURE(record, "UnsafeValue", "tag",
		 UnsafeValue_tag, 2, true);
  INIT_STRUCTURE(record, "UnsafeValue", "projTagged",
		 UnsafeValue_projTagged, 3, true);
  INIT_STRUCTURE(record, "UnsafeValue", "projTaggedTuple",
		 UnsafeValue_projTagged, 3, true);
  INIT_STRUCTURE(record, "UnsafeValue", "con",
		 UnsafeValue_con, 1, true);
  INIT_STRUCTURE(record, "UnsafeValue", "projConstructed",
		 UnsafeValue_projConstructed, 3, true);
  INIT_STRUCTURE(record, "UnsafeValue", "projConstructedTuple",
		 UnsafeValue_projConstructed, 3, true);
  INIT_STRUCTURE(record, "UnsafeValue", "projPoly",
		 UnsafeValue_projPoly, 2, true);
  INIT_STRUCTURE(record, "UnsafeValue", "prod",
		 UnsafeValue_prod, 1, true);
  INIT_STRUCTURE(record, "UnsafeValue", "tuple",
		 UnsafeValue_tuple, 1, true);
  INIT_STRUCTURE(record, "UnsafeValue", "tagged",
		 UnsafeValue_tagged, 3, true);
  INIT_STRUCTURE(record, "UnsafeValue", "taggedTuple",
		 UnsafeValue_taggedTuple, 3, true);
  INIT_STRUCTURE(record, "UnsafeValue", "closure",
		 UnsafeValue_closure, 2, true);
  INIT_STRUCTURE(record, "UnsafeValue", "prim",
		 UnsafeValue_prim, 1, true);
  INIT_STRUCTURE(record, "UnsafeValue", "conName",
		 UnsafeValue_conName, 1, true);
  INIT_STRUCTURE(record, "UnsafeValue", "inArity",
		 UnsafeValue_inArity, 2, true);
  INIT_STRUCTURE(record, "UnsafeValue", "outArity",
		 UnsafeValue_outArity, 2, true);
  RETURN_STRUCTURE("UnsafeValue$", record);
}
