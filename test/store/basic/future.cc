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
#include "generic/Debug.hh"

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
#else
void ErrorOutline(const char *file, int line, const char *message) {
  std::fprintf(stderr, "%s: line %d: %s\n", file, line, message);
}
#endif

word Chain(Transient *t, u_int n) {
  Transient *c = t;
  while (n--) {
    Transient *nt = Store::AllocTransient(FUTURE_LABEL);
    c->Become(REF_LABEL, nt->ToWord());
    c = nt;
  }
  return t->ToWord();
}


int main(void) {
  u_int memLimits[STORE_GENERATION_NUM];
  word root;

  for (u_int i = 0; i < STORE_GENERATION_NUM; i++) {
    memLimits[i] = (i + 1) * STORE_MEMCHUNK_SIZE;
  }
  Store::InitStore(memLimits, 75, 20);
  
  std::printf("Allocating...\n");
  Block *p = Store::AllocBlock(MIN_DATA_LABEL, 1);
  Transient *t = Store::AllocTransient(FUTURE_LABEL);
  p->InitArg(0, Chain(t, 2));
  root = p->ToWord();
  std::printf("GCing...\n");
  //Store::DoGC(root);
  p = Store::WordToBlock(root);
  std::printf("Binding and Diumpingfuture..\n");
  t = Store::WordToTransient(p->GetArg(0));
  Debug::Dump(p->GetArg(0));
  std::printf("done\n");
  Block *a = Store::AllocBlock(MIN_DATA_LABEL, 1);
  t->Become(REF_LABEL, a->ToWord());
  //  t->Become(REF_LABEL, Store::IntToWord(0));
  //std::printf("Checking Result...\n");
  //printf("%d\n", Store::WordToInt(t->GetArg()));
  Block *ra = Store::WordToBlock(p->GetArg(0));
  Debug::Dump(p->GetArg(0));
  Debug::Dump(ra->ToWord());
  std::printf("Done\n");
  std::printf("GCing...\n");
  return 0;
}
