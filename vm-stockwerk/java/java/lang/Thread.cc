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

void NativeMethodTable::java_lang_Thread(JavaString *className) {
  Register(className, "registerNatives", "()V", registerNatives, 0, false);
  //--** currentThread
  //--** yield
  //--** sleep
  //--** start
  //--** isInterrupted
  //--** isAlive
  //--** countStackFrames
  //--** holdsLock
  //--** setPriority0
  //--** stop0
  //--** suspend0
  //--** resume0
  //--** interrupt0
}
