//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#include <cstdio>
#include "store/Store.hh"

#if defined(STORE_DEBUG)
void AssertOutline(const char *file, int line, const char *message) {
  std::fprintf(stderr, "%s: line %d: %s\n", file, line, message);
}
#endif

int main(void) {
  u_int memLimits[STORE_GENERATION_NUM];
  Block *b1, *b2, *rb;
  word root;

  for (u_int i = 0; i < STORE_GENERATION_NUM; i++)
    memLimits[i] = (i + 1) * STORE_MEMCHUNK_SIZE;
  Store::InitStore(memLimits, 75, 20);
  
  std::fprintf(stderr, "Allocating...");
  rb = Store::AllocBlock(Store::MakeLabel(0), 2);
  b1 = Store::AllocBlock(Store::MakeLabel(1), MAX_BLOCKSIZE + 1);
  u_int b1_req = (MAX_BLOCKSIZE + 1);
  u_int b1_got = b1->GetSize();
  std::fprintf(stderr, "%d --> %d\n", b1_req, b1_got);
   for (u_int i = b1->GetSize(); i--;)
    b1->InitArg(i, Store::IntToWord(i));
  b2 = Store::AllocBlock(Store::MakeLabel(2), 5);
  for (u_int i = b2->GetSize(); i--;)
    b2->InitArg(i, Store::IntToWord(i));
  rb->InitArg(0, b1->ToWord());
  rb->InitArg(1, b2->ToWord());
  std::fprintf(stderr, "Done\n");

  std::fprintf(stderr, "Verifying b1...\n");
  for (u_int i = b1->GetSize(); i--;) {
    u_int iv = Store::WordToInt(b1->GetArg(i));
    if (i != iv)
      std::fprintf(stderr, "Value %d = %d != %d broken\n", i, iv, i);
  }
  std::fprintf(stderr, "Verifying b2...\n");
  for (u_int i = b2->GetSize(); i--;) {
    u_int iv = Store::WordToInt(b2->GetArg(i));
    if (i != iv)
      std::fprintf(stderr, "Value %d = %d != %d broken\n", i, iv, i);
  }
  root = rb->ToWord();
  std::fprintf(stderr, "GCing..\n");
  Store::ForceGC(root, 0);
  rb = Store::DirectWordToBlock(root);
  b1 = Store::DirectWordToBlock(rb->GetArg(0));
  b2 = Store::DirectWordToBlock(rb->GetArg(1));
  std::fprintf(stderr, "Checking b1 size...");
  b1_req = b1->GetSize();
  if (b1_req != b1_got)
    std::fprintf(stderr, "Error: GC made %d --> %d\n", b1_got, b1_req);
  else
    std::fprintf(stderr, "passed\n");
  std::fprintf(stderr, "Verifying b1...\n");
  for (u_int i = b1->GetSize(); i--;) {
    u_int iv = Store::WordToInt(b1->GetArg(i));
    if (i != iv)
      std::fprintf(stderr, "Value %d = %d != %d broken\n", i, iv, i);
  }
  std::fprintf(stderr, "Verifying b2...\n");
  for (u_int i = b2->GetSize(); i--;) {
    u_int iv = Store::WordToInt(b2->GetArg(i));
    if (i != iv)
      std::fprintf(stderr, "Value %d = %d != %d broken\n", i, iv, i);
  }
  return 0;
}
