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

DEFINE0(VMSupportsCS8) {
  RETURN_BOOL(false); //--**
} END

void NativeMethodTable::sun_misc_AtomicLong(JavaString *className) {
  Register(className, "VMSupportsCS8", "()Z", VMSupportsCS8, 0, false);
}
