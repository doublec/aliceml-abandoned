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

DEFINE1(dump) {
  Debug::Dump(x0);
  RETURN0;
} END

void NativeMethodTable::java_lang_Object(JavaString *className) {
  Register(className, "dump", "()V", dump, 1, true);
}
