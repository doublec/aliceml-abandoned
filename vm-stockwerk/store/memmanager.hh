#ifndef __memmanager_hh__
#define __memmanager_hh__

#include "memchunk.hh"

typedef struct {
  MemChunk *anchor;
  u_int used;
  u_int gen;
} MemChain;

#define MEMMAN_MAXGEN 4

class MemManager {
protected:
  static MemChain *roots[MEMMAN_MAXGEN];
  static u_int totalHeapSize;
  static u_int needGC;
  static u_int NeedGC();
  static MemChunk *Shrink(MemChunk *list, int threshold);
public:
  static void InitMemManager();
  static void CloseMemManager();
  static b_pointer Alloc(MemChain *chain, u_int size);
};

#endif
