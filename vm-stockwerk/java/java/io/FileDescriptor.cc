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

DEFINE0(initIDs) {
  RETURN_VOID;
} END

void NativeMethodTable::java_io_FileDescriptor(JavaString *className) {
  //--** sync
  Register(className, "initIDs", "()V", initIDs, 0, false);
}
