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

#include "java/Authoring.hh"

DEFINE2(doubleToLongBits) {
  DECLARE_DOUBLE(theDouble, x0); x1 = x1;
  DRETURN(JavaLong::New(theDouble->GetNetworkRepresentation())->ToWord());
} END

DEFINE2(longBitsToDouble) {
  DECLARE_LONG(theLong, x0); x1 = x1;
  Double *theDouble =
    Double::NewFromNetworkRepresentation(theLong->GetNetworkRepresentation());
  DRETURN(theDouble->ToWord());
} END

void NativeMethodTable::java_lang_Double(JavaString *className) {
  Register(className, "doubleToLongBits", "(D)J", doubleToLongBits, 2, false);
  Register(className, "longBitsToDouble", "(J)D", longBitsToDouble, 2, false);
}
