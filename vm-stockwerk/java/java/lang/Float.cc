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

DEFINE1(floatToIntBits) {
  DECLARE_FLOAT(theFloat, x0);
  union {
    float f;
    u_int x;
  } bytes;
  bytes.f = theFloat->GetValue();
  RETURN(JavaInt::ToWord(bytes.x));
} END

DEFINE1(intBitsToFloat) {
  DECLARE_INT(i, x0);
  union {
    float f;
    u_int x;
  } bytes;
  bytes.x = i;
  RETURN(Float::New(bytes.f)->ToWord());
} END

void NativeMethodTable::java_lang_Float(JavaString *className) {
  Register(className, "floatToIntBits", "(F)I", floatToIntBits, 1, false);
  Register(className, "intBitsToFloat", "(I)F", intBitsToFloat, 1, false);
}
