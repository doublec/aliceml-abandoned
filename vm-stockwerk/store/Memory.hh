#ifndef __MEMCHUNK_HH__
#define __MEMCHUNK_HH__

#if defined(INTERFACE)
#pragma interface
#endif

#include <cstdlib>
#include <cstring>
#include "base.hh"

#define MEMCHUNK_SIZE (1024 * 128)

class MemChunk {
protected:
  MemChunk *prev, *next;
  char *block, *top, *max;
public:
  MemChunk(MemChunk *prv, MemChunk *nxt, u_int s) : prev(prv), next(nxt) {
    block = top = (char *) std::malloc(s); Assert(block != NULL);
    max   = (block + s);
  }
  ~MemChunk() {
    Assert(block != NULL); std::free(block);
  }

  int FitsInChunk(u_int s)      { return ((top + s) < max); }
  void MakeEmpty()              { top = block; }
  char *AllocChunkItem(u_int s) { char *oldtop = top; top += s; return oldtop; }
  char *GetTop()                { return (char *) top; }
  u_int GetSize()               { return (max - block); }
  char *GetBottom()             { return block; }
  MemChunk *GetNext()           { return next; }
  void SetNext(MemChunk *nxt)   { next = nxt; }
  MemChunk *GetPrev()           { return prev; }
  void SetPrev(MemChunk *prv)   { prev = prv; }
  void InitBlock(u_int size)    { std::memset(block, 1, size); }
};

#endif
