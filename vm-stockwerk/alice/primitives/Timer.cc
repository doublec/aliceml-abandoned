//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if 0
#include <windows.h>
#include "alice/Authoring.hh"

// to be done: proper platform support (non-windows)
typedef long long verylong;

static verylong fileTimeToMS(FILETIME *ft) {
  verylong x1 = ((verylong)(unsigned int)ft->dwHighDateTime)<<32;
  verylong x2 = x1 + (unsigned int)ft->dwLowDateTime;
  verylong ret = x2 / 10000;
  return ret;
}

static verylong GetTime() {
  SYSTEMTIME st;
  GetSystemTime(&st);
  FILETIME ft;
  SystemTimeToFileTime(&st,&ft);
  return fileTimeToMS(&ft);
}

static verylong startTime;

DEFINE0(Timer_startTimer) {
  startTime = GetTime();
  RETURN_UNIT;
} END

DEFINE0(Timer_checkTimer) {
  verylong curTime = GetTime();
  RETURN(Store::IntToWord((int) (curTime - startTime)));
} END

void PrimitiveTable::RegisterTimer() {
  Register("Timer.checkTimer", Timer_checkTimer, 0);
  Register("Timer.startTimer", Timer_startTimer, 0);
}
#endif
