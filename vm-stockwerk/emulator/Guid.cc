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
#pragma implementation "emulator/Guid.hh"
#endif

#ifdef __MINGW32__
#include <windows.h>
#endif
#include <unistd.h>
#include <cstdlib>
#include <time.h>
#include <sys/times.h>

#include "emulator/Guid.hh"
#include "emulator/RootSet.hh"

word Guid::vmGuid;

void Guid::Init() {
  vmGuid = Guid::New()->ToWord();
  RootSet::Add(vmGuid);
  srand(0);
}

Guid *Guid::New() {
  Tuple *tuple = Tuple::New(4);
  tuple->Init(0, Store::IntToWord(static_cast<int>(getpid())));
  tuple->Init(1, Store::IntToWord(static_cast<int>(time(0))));
#ifdef __MINGW32__
  SYSTEMTIME st;
  GetSystemTime(&st);
  FILETIME ft;
  SystemTimeToFileTime(&st,&ft);
  tuple->Init(2, fileTimeToMS(&ft));
#else
  struct tms buffer;
  int t = times(&buffer);
  double t2 = t * 1000.0 / static_cast<double>(sysconf(_SC_CLK_TCK));
  tuple->Init(2, Store::IntToWord(static_cast<int>(t2)));
#endif
  tuple->Init(3, Store::IntToWord(rand()));
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
