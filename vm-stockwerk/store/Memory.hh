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
  static u_int counter;
protected:
  MemChunk *prev, *next;
  char *block, *top, *max;
public:
  u_int id;
  MemChunk(MemChunk *prv, MemChunk *nxt, u_int s) : prev(prv), next(nxt) {
    block = top = (char *) std::malloc(s); Assert(block != NULL);
    max = (block + s);
    std::memset(block, 1, s);
    id = counter++;
  }
  ~MemChunk() {
    Assert(block != NULL); std::free(block);
  }

  void Clear()                  { top = block; std::memset(block, 1, (max - block)); }
  char *GetTop()                { return top; }
  void SetTop(char *top)        { MemChunk::top = top; } 
  char *GetMax()                { return max; }
  char *GetBottom()             { return block; }

  MemChunk *GetNext()           { return next; }
  void SetNext(MemChunk *nxt)   { next = nxt; }
  MemChunk *GetPrev()           { return prev; }
  void SetPrev(MemChunk *prv)   { prev = prv; }
};

#endif __STORE__MEMORY_HH__
