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
  RETURN_VOID;
} END

DEFINE1(getClass) {
  Transient *transient = Store::WordToTransient(x0);
  if (transient != INVALID_POINTER) REQUEST(x0);
  Block *b = Store::WordToBlock(x0);
  switch (b->GetLabel()) {
  case JavaLabel::Object:
    RETURN(static_cast<Object *>(b)->GetClass()->GetClassObject()->ToWord());
  case JavaLabel::ObjectArray:
  case JavaLabel::BaseArray:
    Error("arrays not supported yet"); //--**
  default:
    Error("invalid object");
  }
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
