//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#ifndef __STORE__HEAP_HH__
#define __STORE__HEAP_HH__

#if defined(INTERFACE)
#pragma interface "store/Heap.hh"
#endif

class SeamDll HeapChunk {
protected:
  char *top, *max, *block, *base;
  HeapChunk *prev, *next;
  
  void Alloc(u_int size);
  void Free();
public:
  HeapChunk() {}
  HeapChunk(u_int size, HeapChunk *chain);
  ~HeapChunk();

  char *GetBase()            { return base; }
  char *GetTop()             { return top; }
  void SetTop(char *p)       { top = p; }
  char *GetMax()             { return max; }
  void SetMax(char *p)       { max = p; }
  HeapChunk *GetPrev()       { return prev; }
  void SetPrev(HeapChunk *p) { prev = p; }
  HeapChunk *GetNext()       { return next; }
  void SetNext(HeapChunk *p) { next = p; }
};

class SeamDll Heap {
protected:
  HeapChunk *chain;
  u_int size, limit;
  static u_int total;
public:
  Heap() {}
  Heap(const u_int chunkSize, u_int limit);
  ~Heap();

  void Enlarge();
  void Shrink();
  u_int GetExactSize();
  HeapChunk *GetChain()               { return chain; }
  u_int GetSize()                     { return size; }
  static u_int GetTotalSize()         { return total; }
  u_int GetLimit()                    { return limit; }
  void SetLimit(u_int l)              { limit = l; }
  void *operator new(size_t, void *p) { return p; }
};

#endif
