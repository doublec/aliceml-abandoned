//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2004
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "generic/Time.hh"
#endif

#include "store/Store.hh"
#include "generic/Time.hh"

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(__CYGWIN__)
#include <windows.h>
#include <cmath>

static double shift;
static double precision;

static inline double LargeIntToDouble(LARGE_INTEGER *li) {
  double x1 = ((double)(unsigned int) li->HighPart) * shift;
  double x2 = ((double)(unsigned int) li->LowPart);
  return (x1 + x2);
}

void Time::Init() {
  LARGE_INTEGER buf;
  // buf = counts per second
  if (!QueryPerformanceFrequency(&buf)) {
    std::fprintf(stderr, "Time: unable to query performance count frequency\n");
    std::fflush(stderr);
    std::exit(0);
  }
  shift = std::pow((double) 2.0, (double) STORE_WORD_WIDTH);
  // We want microseconds
  precision = LargeIntToDouble(&buf) / (double) 1000000;
}

double Time::GetElapsedMicroseconds() {
  LARGE_INTEGER buf;
  if (!QueryPerformanceCounter(&buf)) {
    std::fprintf(stderr, "Time: unable to query performance counter\n");
    std::fflush(stderr);
    std::exit(0);
  }
  return (LargeIntToDouble(&buf) / precision);
}
#else
#include <sys/time.h>

void Time::Init() {}

double Time::GetElapsedMicroseconds() {
  struct timeval tv;
  gettimeofday(&tv, 0);
  return ((double)tv.tv_sec*1000000.0+(double)tv.tv_usec);
}
#endif
