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

#if defined(INTERFACE)
#pragma implementation "alice/Guid.hh"
#endif

#if defined(__MINGW32__) || defined(_MSC_VER)
#include <windows.h>
#include <process.h>
#else
#include <sys/times.h>
#endif

#include <unistd.h>
#include <cstdlib>
#include <time.h>

#include "generic/RootSet.hh"
#include "alice/Guid.hh"

word Guid::vmGuid;

void Guid::Init() {
  vmGuid = Guid::New()->ToWord();
  RootSet::Add(vmGuid);
  srand(0);
}

#if defined(__MINGW32__) || defined(_MSC_VER)

# ifdef __GNUC__
typedef long long verylong;
# else
typedef long verylong;
# endif

static verylong GetTimeStamp() {
  SYSTEMTIME st;
  GetSystemTime(&st);
  FILETIME ft;
  SystemTimeToFileTime(&st, &ft);
  verylong x1 = ((verylong) (u_int) ft.dwHighDateTime) << 32;
  verylong x2 = x1 + (u_int) ft.dwLowDateTime;
  return x2 / 10000;
}

#else

static int GetTimeStamp() {
  struct tms buffer;
  int t = times(&buffer);
  double t2 = t * 1000.0 / static_cast<double>(sysconf(_SC_CLK_TCK));
  return static_cast<int>(t2);
}

#endif

static inline word intToWord(int i) {
  //--** wild hack: make signed (n-1)-bit integer
  return Store::IntToWord((i + i) / 2);
}

Guid *Guid::New() {
  Tuple *tuple = Tuple::New(4);
  tuple->Init(0, intToWord(static_cast<int>(getpid())));
  tuple->Init(1, intToWord(static_cast<int>(time(0))));
  tuple->Init(2, intToWord(GetTimeStamp()));
  tuple->Init(3, intToWord(rand()));
  return static_cast<Guid *>(tuple);
}

int Guid::Compare(Guid *guid1, Guid *guid2) {
  for (int i = 0; i < 4; i++) {
    int a = Store::DirectWordToInt(guid1->Sel(0));
    int b = Store::DirectWordToInt(guid2->Sel(0));
    if (a > b) return 1;
    else if (a < b) return -1;
  }
  return 0;
}

u_int Guid::Hash() {
  return
    Store::DirectWordToInt(Sel(0)) ^
    Store::DirectWordToInt(Sel(1)) ^
    Store::DirectWordToInt(Sel(2)) ^
    Store::DirectWordToInt(Sel(3));
}
