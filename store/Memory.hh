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

#include <cstdlib>
#include <cstring>

class MemChunk {
private:
#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
  static u_int counter;
#endif
protected:
  MemChunk *prev, *next;
  char *block, *max;
  s_int top;
  u_int anchor;
public:
#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
  u_int id;
#endif
  MemChunk(MemChunk *prv, MemChunk *nxt, u_int size) : prev(prv), next(nxt) {
    block = (char *) std::malloc(size);
    AssertStore(block != INVALID_POINTER);
    max    = (block + size);
    top    = (sizeof(u_int) - size);
    anchor = 0;
    u_int *b = (u_int *) block;
    b[0] = HeaderOp::EncodeHeader(REF_LABEL, 0, 0);
    //    for (u_int i = (size / sizeof(u_int)); i--;) {
    //  b[i] = HeaderOp::EncodeHeader(REF_LABEL, 0, 0);
    //}
#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
    id = counter++;
#endif
  }
  MemChunk() : prev(INVALID_POINTER), next(INVALID_POINTER), block(INVALID_POINTER), anchor(1) {}
  ~MemChunk() {
    AssertStore(block != INVALID_POINTER);
    std::free(block);
  }

  void Clear(){
    u_int size = (u_int) (max - block);

    top = (sizeof(u_int) - size);
    u_int *b = (u_int *) block;
    b[0] = HeaderOp::EncodeHeader(REF_LABEL, 0, 0);
    //    for (u_int i = (size / sizeof(u_int)); i--;) {
    //   b[i] = HeaderOp::EncodeHeader(REF_LABEL, 0, 0);
    //}
  }
  s_int GetTop()              { return top; }
  void SetTop(s_int top)      { MemChunk::top = top; } 
  char *GetMax()              { return max; }
  char *GetBottom()           { return block; }
  u_int GetSize()             { return (u_int) (max - block); }

  MemChunk *GetNext()         { return next; }
  void SetNext(MemChunk *nxt) { next = nxt; }
  MemChunk *GetPrev()         { return prev; }
  void SetPrev(MemChunk *prv) { prev = prv; }
  u_int IsAnchor()            { return anchor; }
};

#endif __STORE__MEMORY_HH__
