#ifndef __memchunk_hh__
#define __memchunk_hh__

#include <stdlib.h>
#include <string.h>
#include "base.hh"

#define MEMCHUNK_SIZE (1024 * 128)

class MemChunk {
protected:
  MemChunk *prev, *next;
  char *block, *top, *max;
public:
  MemChunk(MemChunk *prv, MemChunk *nxt, u_int s) : prev(prv),  next(nxt) {
    block = top = (char *) malloc(s);
    Assert(block != NULL);
    max   = (block + s);
  }
  ~MemChunk() {
    Assert(block != NULL);
    free(block);
  }

  inline int FitsInChunk(u_int s)      { return ((top + s) < max); }
  inline void MakeEmpty()              { top = block; }
  inline char *AllocChunkItem(u_int s) { char *oldtop = top;  top += s; return oldtop; }
  inline char *GetTop()                { return (char *) top; }
  inline u_int GetSize()               { return (u_int) (max - block); }
  inline char *GetBottom()             { return block; }
  inline MemChunk *GetNext()           { return next; }
  inline void SetNext(MemChunk *nxt)   { next = nxt; }
  inline MemChunk *GetPrev()           { return prev; }
  inline void SetPrev(MemChunk *prv)   { prev = prv; }
#ifdef DEBUG_CHECK
  inline void FillZero()               { memset(block, 0, (max - block)); }
#endif
};

#endif
