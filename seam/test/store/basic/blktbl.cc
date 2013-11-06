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

#define COUNT_LIMIT 5

#if defined(STORE_DEBUG)
void AssertOutline(const char *file, int line, const char *message) {
  std::fprintf(stderr, "%s: line %d: %s\n", file, line, message);
}
#endif

static Chunk *AllocChunk(char *s) {
  u_int len = strlen(s);
  Chunk *c = Store::AllocChunk(len);
  memcpy(c->GetBase(), s, len);
  return c;
}

static void ShowItems(word key, word) {
  fprintf(stderr, "Dict Key %x\n", key);
}

int main(void) {
  u_int memLimits[STORE_GENERATION_NUM];
  Chunk *k1, *k2;
  BlockHashTable *table;
  Block *r;

  for (u_int i = 0; i < STORE_GENERATION_NUM; i++) {
    memLimits[i] = (i + 1) * STORE_MEMCHUNK_SIZE;
  }
  Store::InitStore(memLimits, 75, 20);
  r = Store::AllocBlock(MIN_DATA_LABEL, 3);
  std::fprintf(stderr, "Allocating string keys...");
  k1 = AllocChunk("Key1");
  k2 = AllocChunk("Key2");
  std::fprintf(stderr, "done.\n");
  std::fprintf(stderr, "Filling entries...");
  r->InitArg(0, k1->ToWord());
  r->InitArg(1, k2->ToWord());
  table = BlockHashTable::New(10);
  r->InitArg(2, table->ToWord());
  table->InsertItem(k1->ToWord(), Store::IntToWord(4000));
  table->InsertItem(k2->ToWord(), Store::IntToWord(8000));
  std::fprintf(stderr, "done.\n");
  std::fprintf(stderr, "Looking up...\n");
  std::fprintf(stderr, "Elem k1 = %d\n",
	       Store::DirectWordToInt(table->GetItem(r->GetArg(0))));
  std::fprintf(stderr, "Elem k2 = %d\n",
	       Store::DirectWordToInt(table->GetItem(r->GetArg(1))));
  word root = r->ToWord();
  Store::MemStat();
  std::fprintf(stderr, "Forcing GC...\n");
  Store::ForceGC(root, 0);
  std::fprintf(stderr, "done\n");
  Store::MemStat();
  r = Store::DirectWordToBlock(root);
  table = BlockHashTable::FromWordDirect(r->GetArg(2));
  std::fprintf(stderr, "Key %p\n", r->GetArg(0));
  std::fprintf(stderr, "Key %p\n", r->GetArg(1));
  std::fprintf(stderr, "Dictionary Keys\n");
  table->Apply((item_apply) ShowItems);
  std::fprintf(stderr, "---\n");
  if (table->IsMember(r->GetArg(0)) && table->IsMember(r->GetArg(1)))
    std::fprintf(stderr, "BlockHashTable succeeded\n");
  else
    std::fprintf(stderr, "BlockHashTable failed\n");
  std::fprintf(stderr, "Elem k1 = %d\n",
	       Store::DirectWordToInt(table->GetItem(r->GetArg(0))));
  std::fprintf(stderr, "Elem k2 = %d\n",
	       Store::DirectWordToInt(table->GetItem(r->GetArg(1))));
  std::printf("Forcing GC again...\n");
  Store::ForceGC(root, 0);
  std::printf("done\n");
  Store::MemStat();
  std::printf("It Succeeded\n");
  return 0;
}
