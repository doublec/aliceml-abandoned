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
  RETURN_VOID;
} END

DEFINE1(getClass) {
  DECLARE_OBJECT(_this, x0);
  Assert(_this != INVALID_POINTER);
  //--** return class object
  RETURN(_this->GetClass()->ToWord());
} END

DEFINE1(hashCode) {
  DECLARE_OBJECT(_this, x0);
  Assert(_this != INVALID_POINTER);
  RETURN_JINT(0); //--**
} END

void NativeMethodTable::java_lang_Object(JavaString *className) {
  Register(className, "registerNatives", "()V", registerNatives, 0, false);
  Register(className, "getClass", "()Ljava/lang/Class;", getClass, 1, true);
  Register(className, "hashCode", "()I", hashCode, 1, true);
  //--** clone
  //--** notify
  //--** notifyAll
  //--** wait
}
