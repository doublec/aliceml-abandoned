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

#include "Alice.hh"

#if defined(__MINGW32__) || defined(_MSC_VER)
#include <windows.h>
#include <cmath>

static double shift;
static double precision;

static inline double LargeIntToDouble(LARGE_INTEGER *li) {
  double x1 = ((double)(unsigned int) li->HighPart) * shift;
  double x2 = ((double)(unsigned int) li->LowPart);
  return (x1 + x2);
}

static void InitTime() {
  LARGE_INTEGER buf;
  // buf = counts per second
  if (!QueryPerformanceFrequency(&buf)) {
    fprintf(stderr, "Profiler: unable to query performance count frequency\n");
    fflush(stderr);
    exit(0);
  }
  shift = std::pow((double) 2.0, (double) STORE_WORD_WIDTH);
  // We want microseconds
  precision = LargeIntToDouble(&buf) / (double) 1000000;
}

double SampleTime() {
  LARGE_INTEGER buf;
  if (!QueryPerformanceCounter(&buf)) {
    fprintf(stderr, "Profiler: unable to query performance counter\n");
    fflush(stderr);
    exit(0);
  }
  return (LargeIntToDouble(&buf) / precision);
}
#else
#include <sys/time.h>

static void InitTime() {
  return;
}

double SampleTime() {
  struct timeval tv;
  gettimeofday(&tv, 0);
  return ((double)tv.tv_sec*1000000.0+(double)tv.tv_usec);
}
#endif

static double startTime;

DEFINE0(Timer_start) {
  startTime = SampleTime();
  RETURN_UNIT;
} END

DEFINE0(Timer_check) {
  double curTime = SampleTime();
  double result = (curTime - startTime) / 1000.0;
  RETURN(Store::IntToWord(static_cast<int>(result)));
} END

word InitComponent() {
  Record *record = Record::New(2);
  InitTime();
  INIT_STRUCTURE(record, "AliceTimer", "start",
		 Timer_start, 0);
  INIT_STRUCTURE(record, "AliceTimer", "check",
		 Timer_check, 0);
  RETURN_STRUCTURE("AliceTimer$", record);
}
