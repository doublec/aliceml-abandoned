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

DEFINE1(intern) {
  DECLARE_JAVA_STRING(string, x0);
  RETURN(string->Intern()->ToWord());
} END

void NativeMethodTable::java_lang_String(JavaString *className) {
  Register(className, "intern", "()Ljava/lang/String;", intern, 1, true);
}
