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

DEFINE2(General_assign) {
  DECLARE_CELL(cell, x0);
  cell->Assign(x1);
  RETURN_UNIT;
} END

DEFINE2(General_exchange) {
  DECLARE_CELL(cell, x0);
  RETURN(cell->Exchange(x1));
} END

DEFINE1(General_exnName) {
  RETURN(String::New("")->ToWord()); //--** to be determined
} END

void Primitive::RegisterGeneral() {
  Register("General.:=", General_assign, 2);
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
  Register("General.exchange", General_exchange, 2);
  Register("General.exnName", General_exnName, 1);
}
