//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2000-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/Authoring.hh"

void PrimitiveTable::RegisterGeneral() {
  PrimitiveTable::General_Chr =
    UniqueConstructor::New("Chr", "General.Chr")->ToWord();
  PrimitiveTable::General_Div =
    UniqueConstructor::New("Div", "General.Div")->ToWord();
  PrimitiveTable::General_Domain =
    UniqueConstructor::New("Domain", "General.Domain")->ToWord();
  PrimitiveTable::General_Overflow =
    UniqueConstructor::New("Overflow", "General.Overflow")->ToWord();
  PrimitiveTable::General_Size =
    UniqueConstructor::New("Size", "General.Size")->ToWord();
  PrimitiveTable::General_Subscript =
    UniqueConstructor::New("Subscript", "General.Subscript")->ToWord();

  RegisterUniqueConstructor("Assert", "General.Assert");
  RegisterUniqueConstructor("Bind", "General.Bind");
  Register("General.Chr", PrimitiveTable::General_Chr);
  Register("General.Div", PrimitiveTable::General_Div);
  Register("General.Domain", PrimitiveTable::General_Domain);
  RegisterUniqueConstructor("Fail", "General.Fail");
  RegisterUniqueConstructor("Match", "General.Match");
  Register("General.Overflow", PrimitiveTable::General_Overflow);
  Register("General.Size", PrimitiveTable::General_Size);
  RegisterUniqueConstructor("Span", "General.Span");
  Register("General.Subscript", PrimitiveTable::General_Subscript);
}
