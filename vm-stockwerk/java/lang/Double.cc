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

#include "generic/Debug.hh"
#include "java/Authoring.hh"

DEFINE1(toLongBits) {
  DECLARE_DOUBLE(theDouble, x0);
  RETURN(JavaLong::New(theDouble->GetNetworkRepresentation())->ToWord());
} END

DEFINE1(longBitsToDouble) {
  DECLARE_LONG(theLong, x0);
  Double *theDouble =
    Double::NewFromNetworkRepresentation(theLong->GetNetworkRepresentation());
  RETURN(theDouble->ToWord());
} END

void NativeMethodTable::java_lang_Double(JavaString *className) {
  Register(className, "toLongBits", "(D)J", toLongBits, 1, false);
  Register(className, "longBitsToDouble", "(J)D", longBitsToDouble, 1, false);
}
