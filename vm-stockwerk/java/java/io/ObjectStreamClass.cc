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

DEFINE0(initNative) {
  RETURN_VOID;
} END

void NativeMethodTable::java_io_ObjectStreamClass(JavaString *className) {
  Register(className, "initNative", "()V", initNative, 0, false);
}
