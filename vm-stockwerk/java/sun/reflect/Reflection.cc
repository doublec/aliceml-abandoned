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

DEFINE1(getCallerClass) {
  RETURN(null); //--**
} END

void NativeMethodTable::sun_reflect_Reflection(JavaString *className) {
  Register(className, "getCallerClass", "(I)Ljava/lang/Class;",
	   getCallerClass, 1, false);
}
