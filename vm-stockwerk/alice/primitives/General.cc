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

DEFINE1(General_exnName) {
  DECLARE_BLOCK(conVal, x0);
 retry:
  switch (conVal->GetLabel()) {
  case UNIQUESTRING_LABEL:
    RETURN(UniqueString::FromWordDirect(conVal->ToWord())->
	   ToString()->ToWord());
  case Alice::ConVal:
    conVal = ConVal::FromWordDirect(conVal->ToWord())->GetConstructor();
    goto retry;
  default:
    RETURN(Constructor::FromWordDirect(conVal->ToWord())->GetName()->ToWord());
  }
} END

void PrimitiveTable::RegisterGeneral() {
  PrimitiveTable::General_Chr =
    UniqueConstructor::New(String::New("General.Chr"))->ToWord();
  PrimitiveTable::General_Div =
    UniqueConstructor::New(String::New("General.Div"))->ToWord();
  PrimitiveTable::General_Domain =
    UniqueConstructor::New(String::New("General.Domain"))->ToWord();
  PrimitiveTable::General_Overflow =
    UniqueConstructor::New(String::New("General.Overflow"))->ToWord();
  PrimitiveTable::General_Size =
    UniqueConstructor::New(String::New("General.Size"))->ToWord();
  PrimitiveTable::General_Subscript =
    UniqueConstructor::New(String::New("General.Subscript"))->ToWord();

  RegisterUniqueConstructor("General.Bind");
  Register("General.Chr", PrimitiveTable::General_Chr);
  Register("General.Div", PrimitiveTable::General_Div);
  Register("General.Domain", PrimitiveTable::General_Domain);
  RegisterUniqueConstructor("General.Fail");
  RegisterUniqueConstructor("General.Match");
  Register("General.Overflow", PrimitiveTable::General_Overflow);
  Register("General.Size", PrimitiveTable::General_Size);
  RegisterUniqueConstructor("General.Span");
  Register("General.Subscript", PrimitiveTable::General_Subscript);
  Register("General.exnName", General_exnName, 1);
}
