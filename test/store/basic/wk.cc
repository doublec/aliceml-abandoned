//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000-2001
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#include <cstdio>
#include "store/Store.hh"
#include "adt/Stack.hh"

#define COUNT_LIMIT 5

#if defined(STORE_DEBUG)
void AssertOutline(const char *file, int line, const char *message) {
  std::fprintf(stderr, "%s: line %d: %s\n", file, line, message);
}
#endif

int main(void) {
  u_int memLimits[STORE_GENERATION_NUM];
  WeakDictionary *dict;

  for (u_int i = 0; i < STORE_GENERATION_NUM; i++) {
    memLimits[i] = (i + 1) * STORE_MEMCHUNK_SIZE;
  }
  Store::InitStore(memLimits, 75, 20);
  
  std::printf("Allocating WD...\n");
  dict = WeakDictionary::New(10, INVALID_POINTER);
  std::printf("done\n");
  std::printf("Inserting Items...\n");
  for (u_int i = 0; i < 20; i++) {
    dict->InsertItem(i, Store::IntToWord(1000 * (i + 1)));
  }
  std::printf("done\n");
  std::printf("Accessing Items...\n");
  for (u_int i = 0; i < 20; i++) {
    printf("%d\n", Store::WordToInt(dict->GetItem(i)));
  }
  std::printf("done\n");

  std::printf("It Succeeded\n");
  return 0;
}
