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

#if defined(__MINGW32__) || defined(_MSC_VER)
#include <windows.h>
#endif

#include "java/Authoring.hh"

DEFINE0(registerNatives) {
  RETURN0;
} END

DEFINE0(currentTimeMillis) {
#if defined(__MINGW32__) || defined(_MSC_VER)
  SYSTEMTIME st;
  GetSystemTime(&st);
  FILETIME ft;
  if (SystemTimeToFileTime(&st, &ft) == FALSE)
    Error("SystemTimeToFileTime failed"); //--**
  RETURN2(JavaLong::New(ft.dwHighDateTime, ft.dwLowDateTime)->ToWord(), null);
#else
  RETURN_INT(0); //--**
#endif
} END

void NativeMethodTable::java_lang_System(JavaString *className) {
  Register(className, "registerNatives", "()V", registerNatives, 0, false);
  Register(className, "currentTimeMillis", "()J", currentTimeMillis, 0, false);
}
