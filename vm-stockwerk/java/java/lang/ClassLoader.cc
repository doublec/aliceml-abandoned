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

#include "java/ClassLoader.hh"
#include "java/Authoring.hh"

DEFINE0(registerNatives) {
  RETURN_VOID;
} END

void NativeMethodTable::java_lang_ClassLoader(JavaString *className) {
  Register(className, "registerNatives", "()V", registerNatives, 0, false);
}
