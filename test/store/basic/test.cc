//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#include <cstdio>
#include "store/Store.hh"
#include "adt/Stack.hh"

#define COUNT_LIMIT 5

static u_int StringToHexValue(char *s) {
  u_int l   = std::strlen(s) - 1;
  u_int val = 0;

  for (int i = l; i >= 0; i--) {
    if (s[l - i] == '1') {
      val += (1 << i);
    }
  }

  return val;
}

#if defined(STORE_DEBUG)
void AssertOutline(const char *file, int line, const char *message) {
  std::fprintf(stderr, "%s: line %d: %s\n", file, line, message);
}
#endif

int main(void) {
  u_int memLimits[STORE_GENERATION_NUM];
  Block *p;
  word root;

  for (u_int i = 0; i < STORE_GENERATION_NUM; i++) {
    memLimits[i] = (i + 1);
  }

  printf("SIZE_MASK is %x\n", StringToHexValue("00011111111111111110000000000000"));
  
  Store::InitStore(memLimits);
  
  std::printf("Allocating...\n");
  p = Store::AllocBlock(Store::MakeLabel(0), 1);
  std::printf("Initializing allocated block\n");
  p->InitArg(0, 667);
  std::printf("Checking Result...\n");
  printf("%d\n", Store::WordToInt(p->GetArg(0)));
  std::printf("Done\n");

  p->InitArg(0, Stack::New(2)->ToWord());
  //  p->InitArg(2, Store::AllocBlock((BlockLabel) 0, 1024)->ToWord());
  //  p->InitArg(3, Store::AllocBlock((BlockLabel) 0, 1024)->ToWord());

  root = p->ToWord();
#if defined(STORE_DEBUG)
  Store::MemStat();
  Store::ForceGCGen(0);
  std::printf("GCing gen 0...\n");
  Store::DoGC(root);
  Store::MemStat();
#else
  if (Store::NeedGC()) {
    std::printf("GCing..\n");
    Store::DoGC(root);
  }
#endif

  for (u_int i = 0; i <= COUNT_LIMIT; i++) {
    Stack *s = Stack::FromWord(Store::WordToBlock(root)->GetArg(0));

    std::printf("Pushing: %d\n", i);
    s->SlowPush(Store::IntToWord(i));
  }

#if defined(STORE_DEBUG)
  Store::MemStat();
  std::printf("GCing gen 0,1...\n");
  Store::ForceGCGen(1);
  Store::DoGC(root);
  Store::MemStat();
#else
  if (Store::NeedGC()) {
    std::printf("GCing..\n");
    Store::DoGC(root);
  }
#endif

  for (u_int i = 0; i <= COUNT_LIMIT; i++) {
    Stack *s = Stack::FromWord(Store::WordToBlock(root)->GetArg(0));
    int v    = Store::WordToInt(s->Pop());
    std::printf("Popped: %d\n", v);
  }

#if defined(STORE_DEBUG)
  Store::ForceGCGen(STORE_GEN_OLDEST); // formerly 0
  std::printf("GCing gen 0,1,2...\n");
  Store::DoGC(root);
  Store::MemStat();
#else
  if (Store::NeedGC()) {
    std::printf("GCing..\n");
    Store::DoGC(root);
  }
#endif

  if (Store::WordToBlock(root)->GetLabel() == (BlockLabel) 0) {
    std::printf("It Succeeded\n");
  }
  else {
    std::printf("It Failed\n");
  }

  return 0;
}
