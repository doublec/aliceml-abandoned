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
#ifndef __STORE__MEMORY_HH__
#define __STORE__MEMORY_HH__

#if defined(INTERFACE)
#pragma interface "store/Memory.hh"
#endif

#include <cstdlib>
#include <cstring>

class MemChunk {
private:
#if defined(STORE_DEBUG)
  static u_int counter;
#endif
protected:
  MemChunk *prev, *next;
  char *block, *top, *max;
  int anchor;
public:
#if defined(STORE_DEBUG)
  u_int id;
#endif
  MemChunk(MemChunk *prv, MemChunk *nxt, u_int size) : prev(prv), next(nxt) {
    block = top = (char *) std::malloc(size); AssertStore(block != INVALID_POINTER);
    max = (block + size);
    anchor = 0;
    std::memset(block, 1, size);
#if defined(STORE_DEBUG)
    id = counter++;
#endif
  }
  MemChunk() : prev(INVALID_POINTER), next(INVALID_POINTER), block(INVALID_POINTER), anchor(1) {}
  ~MemChunk() {
    AssertStore(block != INVALID_POINTER);
    std::free(block);
  }

  void Clear()                { top = block; std::memset(block, 1, (max - block)); }
  char *GetTop()              { return top; }
  char **GetTopAddr()         { return &top; }
  void SetTop(char *top)      { MemChunk::top = top; } 
  char *GetMax()              { return max; }
  char *GetBottom()           { return block; }

  MemChunk *GetNext()         { return next; }
  void SetNext(MemChunk *nxt) { next = nxt; }
  MemChunk *GetPrev()         { return prev; }
  void SetPrev(MemChunk *prv) { prev = prv; }
  int IsAnchor()              { return anchor; }
};

#endif __STORE__MEMORY_HH__
