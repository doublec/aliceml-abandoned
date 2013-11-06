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

#include "alice/Authoring.hh"

DEFINE1(Byte_bytesToString) {
  DECLARE_STRING(s, x0);
  RETURN(s->ToWord());
} END

DEFINE1(Byte_stringToBytes) {
  DECLARE_STRING(s, x0);
  RETURN(s->ToWord());
} END

void PrimitiveTable::RegisterByte() {
  Register("Byte.bytesToString", Byte_bytesToString, 1);
  Register("Byte.stringToBytes", Byte_stringToBytes, 1);
}
