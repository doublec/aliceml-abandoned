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
#ifndef __STORE__MEMORY_HH__
#define __STORE__MEMORY_HH__

#if defined(INTERFACE)
#pragma interface "store/Memory.hh"
#endif

#include <cstring>

class MemChunk {
protected:
  MemChunk *prev, *next;
  char *block, *base, *top, *max;

  void SetPrev(MemChunk *prv) { prev = prv; }
  void SetNext(MemChunk *nxt) { next = nxt; }
  void Free(char *p);
  char *Alloc(u_int size);
public:
  MemChunk(MemChunk *root, u_int size) : prev(NULL) {
    block = base = MemChunk::Alloc(size + STORE_MEM_ALIGN);
    AssertStore(block != NULL);
    // Ensure Base Ptr Alignment
    base += (STORE_MEM_ALIGN - ((u_int) base & (STORE_MEM_ALIGN - 1)));
    max = (base + size);
    top = base;
    next = root;
    if (root != NULL) {
      root->SetPrev(this);
    }
    std::memset(base, 1, size);
  }
  ~MemChunk() {
    AssertStore(block != NULL);
    MemChunk::Free(block);
    if (prev != NULL) {
      prev->SetNext(next);
    }
    if (next != NULL) {
      next->SetPrev(prev);
    }
  }

  void Clear(){
    u_int size = (u_int) (max - base);
    std::memset(base, 1, size);
    top = base;
  }
  char *GetTop()         { return top; }
  void SetTop(char *top) { MemChunk::top = top; } 
  char *GetMax()         { return max; }
  char *GetBase()        { return base; }
  MemChunk *GetNext()    { return next; }
  MemChunk *GetPrev()    { return prev; }
};

#endif
