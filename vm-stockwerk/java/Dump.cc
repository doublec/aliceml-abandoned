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

DEFINE1(dumpInteger) {
  DECLARE_INT(i, x0);
  std::printf("%d\n", i);
  RETURN_VOID;
} END

DEFINE1(dumpObject) {
  Debug::Dump(x0);
  RETURN_VOID;
} END

DEFINE1(dumpString) {
  DECLARE_JAVA_STRING(string, x0);
  std::printf("%s\n", string->ExportC());
  RETURN_VOID;
} END

void NativeMethodTable::Dump(JavaString *className) {
  Register(className, "dump", "(I)V", dumpInteger, 1, false);
  Register(className, "dump", "(Ljava/lang/Object;)V", dumpObject, 1, false);
  Register(className, "dump", "(Ljava/lang/String;)V", dumpString, 1, false);
}
