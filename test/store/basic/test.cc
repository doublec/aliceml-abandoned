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

static unsigned int StringToHexValue(char *s) {
  unsigned int l   = std::strlen(s) - 1;
  unsigned int val = 0;

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
  unsigned int memLimits[STORE_GENERATION_NUM];
  Block *p;
  word root;

  for (unsigned int i = 0; i < STORE_GENERATION_NUM; i++) {
    memLimits[i] = (i + 1) * STORE_MEMCHUNK_SIZE;
  }

  printf("SIZE_MASK is %x\n", StringToHexValue("00011111111111111110000000000000"));
  
  Store::InitStore(memLimits, 75, 20);
  
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
//    if (Store::NeedGC()) {
//      std::printf("GCing..\n");
//      Store::DoGC(root);
//    }
  std::printf("GCing..\n");
  //  Store::ForceGC(root, 0);

  for (unsigned int i = 0; i <= COUNT_LIMIT; i++) {
    Stack *s = Stack::FromWord(Store::WordToBlock(root)->GetArg(0));

    std::printf("Pushing: %d\n", i);
    s->SlowPush(Store::IntToWord(i));
  }

//    if (Store::NeedGC()) {
//      std::printf("GCing..\n");
//      Store::DoGC(root);
//    }
  std::printf("GCing..\n");
  //Store::ForceGC(root, 0);

  for (unsigned int i = 0; i <= COUNT_LIMIT; i++) {
    Stack *s = Stack::FromWord(Store::WordToBlock(root)->GetArg(0));
    int v    = Store::WordToInt(s->Pop());
    std::printf("Popped: %d\n", v);
  }

//    if (Store::NeedGC()) {
//      std::printf("GCing..\n");
//      Store::DoGC(root);
//    }
  std::printf("GCing..\n");
  //Store::ForceGC(root, 0);

  if (Store::WordToBlock(root)->GetLabel() == (BlockLabel) 0) {
    std::printf("It Succeeded\n");
  }
  else {
    std::printf("It Failed\n");
  }

  if (Store::WordToInt((word) HeaderOp::EncodeHeader(REF_LABEL, 0, 0)) != INVALID_INT) {
    std::printf("Magic Header is Integer: %x\n", HeaderOp::EncodeHeader(REF_LABEL, 0, 0));
  }
  else {
    std::printf("Magic Header is no Integer!\n");
  }

  return 0;
}
