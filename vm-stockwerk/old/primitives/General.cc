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
  Register("General.Bind", Constructor::New()->ToWord()); //--** unique
  Register("General.Chr", Constructor::New()->ToWord()); //--** unique
  Register("General.Div", Constructor::New()->ToWord()); //--** unique
  Register("General.Domain", Constructor::New()->ToWord()); //--** unique
  Register("General.Fail", Constructor::New()->ToWord()); //--** unique
  Register("General.Match", Constructor::New()->ToWord()); //--** unique
  Register("General.Overflow", Constructor::New()->ToWord()); //--** unique
  Register("General.Size", Constructor::New()->ToWord()); //--** unique
  Register("General.Span", Constructor::New()->ToWord()); //--** unique
  Register("General.Subscript", Constructor::New()->ToWord()); //--** unique
  Register("General.exchange", General_exchange, 2);
  Register("General.exnName", General_exnName, 1);
};
