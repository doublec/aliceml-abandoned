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

#include "emulator/Authoring.hh"

DEFINE1(General_exnName) {
  RETURN(String::New("")->ToWord()); //--** to be determined
} END

void PrimitiveTable::RegisterGeneral() {
  RegisterUniqueConstructor("General.Bind");
  RegisterUniqueConstructor("General.Chr");
  RegisterUniqueConstructor("General.Div");
  RegisterUniqueConstructor("General.Domain");
  RegisterUniqueConstructor("General.Fail");
  RegisterUniqueConstructor("General.Match");
  RegisterUniqueConstructor("General.Overflow");
  RegisterUniqueConstructor("General.Size");
  RegisterUniqueConstructor("General.Span");
  RegisterUniqueConstructor("General.Subscript");
  Register("General.exnName", General_exnName, 1);
}
