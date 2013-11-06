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

class MyFinalization : public Finalization {
public:
  MyFinalization() {}
  virtual ~MyFinalization() {}

  void Finalize(word value) {
    std::printf("MyFinalization::Finalize enter\n");
    Block *p = Store::WordToBlock(value);
    if (p->GetLabel() == WEAK_DICT_LABEL)
      std::printf("Got WK Dictionary\n");
    else
      std::printf("Got something different\n");
    std::printf("MyFinalization::Finalize leave\n");
    std::fflush(stdout);
  }
};

int main(void) {
  u_int memLimits[STORE_GENERATION_NUM];
  WeakDictionary *d1, *d2;

  for (u_int i = 0; i < STORE_GENERATION_NUM; i++) {
    memLimits[i] = (i + 1) * STORE_MEMCHUNK_SIZE;
  }
  Store::InitStore(memLimits, 75, 20);
  MyFinalization *f = new MyFinalization();
  std::printf("Allocating WDs...\n");
  d1 = WeakDictionary::New(10, f);
  d2 = WeakDictionary::New(10, f);
  std::printf("done\n");
  std::printf("Inserting Items...\n");
  for (u_int i = 0; i < 1; i++) {
    word p = Store::AllocBlock(Store::MakeLabel(0), 1024)->ToWord();

    d1->InsertItem(i, p);
    d2->InsertItem(i, p);
  }
  std::printf("done\n");
  std::printf("Creating root set...\n");
  Block *rb = Store::AllocBlock(Store::MakeLabel(0), 3);
  Block *q  = Store::AllocBlock(Store::MakeLabel(0), 2048);
  rb->InitArg(0, d1->ToWord());
  rb->InitArg(1, d2->ToWord());
  rb->InitArg(2, q->ToWord());
  word root = rb->ToWord();
  std::printf("done\n");
  Store::MemStat();
  std::printf("Forcing GC...\n");
  Store::ForceGC(root, 0);
  std::printf("done\n");
  Store::MemStat();
  rb = Store::WordToBlock(root);
  if ((WeakDictionary::FromWord(rb->GetArg(0))->IsMember(0)) ||
      (WeakDictionary::FromWord(rb->GetArg(1))->IsMember(0))) {
    std::printf("Finalization failed\n");
  }
  else {
    std::printf("Finalization succeeded\n");
  }
  std::printf("Forcing GC again...\n");
  Store::ForceGC(root, 0);
  std::printf("done\n");
  Store::MemStat();
  std::printf("It Succeeded\n");
  return 0;
}
