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

DEFINE0(registerNatives) {
  RETURN0;
} END

DEFINE1(getPrimitiveClass) {
  RETURN(null); //--**
} END

void NativeMethodTable::java_lang_Class(JavaString *className) {
  Register(className, "registerNatives", "()V", registerNatives, 0, false);
  Register(className, "getPrimitiveClass",
	   "(Ljava/lang/String;)Ljava/lang/Class;",
	   getPrimitiveClass, 1, false);
}
