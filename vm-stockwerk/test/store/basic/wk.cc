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
  Block *p = Store::AllocBlock(Store::MakeLabel(0), 2048);
  dict->InsertItem(20, p->ToWord());
  std::printf("done\n");
  std::printf("Accessing Items...\n");
  for (u_int i = 0; i < 20; i++) {
    printf("%d\n", Store::WordToInt(dict->GetItem(i)));
  }
  std::printf("done\n");
  std::printf("Creating root set...\n");
  Block *rb = Store::AllocBlock(Store::MakeLabel(0), 2);
  Block *q  = Store::AllocBlock(Store::MakeLabel(0), 2048);
  rb->InitArg(0, dict->ToWord());
  rb->InitArg(1, q->ToWord());
  word root = rb->ToWord();
  std::printf("done\n");
  Store::MemStat();
  std::printf("Forcing GC...\n");
  Store::ForceGC(root, 0);
  std::printf("done\n");
  Store::MemStat();
  rb = Store::WordToBlock(root);
  if (WeakDictionary::FromWord(rb->GetArg(0))->IsMember(20)) {
    std::printf("Finalization failed\n");
  }
  else {
    std::printf("Finalization succeeded\n");
  }
  std::printf("Forcing GC again...\n");
  Store::ForceGC(root, 1);
  std::printf("done\n");
  Store::MemStat();
  std::printf("It Succeeded\n");
  return 0;
}
