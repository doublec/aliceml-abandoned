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

DEFINE0(registerNatives) {
  RETURN0;
} END

DEFINE1(dump_I) {
  DECLARE_INT(i, x0);
  std::printf("%d\n", i);
  RETURN0;
} END

DEFINE1(dump_Object) {
  Debug::Dump(x0);
  RETURN0;
} END

DEFINE1(dump_String) {
  DECLARE_JAVA_STRING(string, x0);
  std::printf("%s\n", string->ExportC());
  RETURN0;
} END

void NativeMethodTable::java_lang_Object(JavaString *className) {
  Register(className, "registerNatives", "()V", registerNatives, 0, false);
  Register(className, "dump", "(I)V", dump_I, 1, false);
  Register(className, "dump", "(Ljava/lang/Object;)V", dump_Object, 1, false);
  Register(className, "dump", "(Ljava/lang/String;)V", dump_String, 1, false);
}
