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

#if defined(__MINGW32__) || defined(_MSC_VER)
#include <windows.h>
#else
#include <sys/times.h>
#endif

#include "Alice.hh"

// to be done: proper platform support (non-windows)
typedef long long verylong;

#if defined(__MINGW32__) || defined(_MSC_VER)
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
#else
static verylong GetTime() {
  struct tms tms;
  if (times(&tms) == (clock_t) -1)
    Error("could not get time");
  return tms.tms_utime + tms.tms_stime;
}
#endif

static verylong startTime;

DEFINE0(Timer_start) {
  startTime = GetTime();
  RETURN_UNIT;
} END

DEFINE0(Timer_check) {
  verylong curTime = GetTime();
  RETURN(Store::IntToWord((int) (curTime - startTime)));
} END

word InitComponent() {
  Record *record = Record::New(2);
  INIT_STRUCTURE(record, "Timer", "start",
		 Timer_start, 0, true);
  INIT_STRUCTURE(record, "Timer", "check",
		 Timer_check, 0, true);
  RETURN_STRUCTURE("Timer$", record);
}
