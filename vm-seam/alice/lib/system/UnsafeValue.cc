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
#include "alice/Authoring.hh"

DEFINE1(UnsafeValue_cast) {
  RETURN(x0);
} END

DEFINE2(UnsafeValue_same) {
  x0 = PointerOp::Deref(x0);
  x1 = PointerOp::Deref(x1);
  RETURN_BOOL(x0 == x1);
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
    conVal->IsConVal()? conVal->GetConstructor(): Constructor::FromWord(x0);
  RETURN(constructor->ToWord());
} END

DEFINE3(UnsafeValue_projConstructed) {
  DECLARE_CONVAL(conVal, x0);
  x1 = x1; // ignored
  DECLARE_INT(i, x2);
  Assert(conVal->IsConVal());
  RETURN(conVal->Sel(i));
} END

DEFINE1(UnsafeValue_conName) {
  DECLARE_CONSTRUCTOR(constructor, x0);
  String *name = constructor->GetName();
  TagVal *exId = TagVal::New(0, 1); // ExId ...
  exId->Init(0, name->ToWord());
  RETURN(exId->ToWord());
} END

DEFINE1(UnsafeValue_conType) {
  DECLARE_CONSTRUCTOR(constructor, x0);
  RETURN(Store::IntToWord(0)); //--** unimplemented
} END

word UnsafeValue() {
  Record *record = Record::New(11);
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
  INIT_STRUCTURE(record, "UnsafeValue", "conName",
		 UnsafeValue_conName, 1, true);
  INIT_STRUCTURE(record, "UnsafeValue", "conType",
		 UnsafeValue_conType, 1, true);
  RETURN_STRUCTURE("UnsafeValue$", record);
}
