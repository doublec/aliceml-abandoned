#ifndef __dataset_hh__
#define __dataset_hh__

#include "types.hh"
#include "memchunk.hh"

class DataSet {
protected:
  u_int size, pos;
  u_int *mem;
public:
  DataSet() : size(MEMCHUNK_SIZE), pos(0) { mem = (u_int *) malloc(MEMCHUNK_SIZE); }
  ~DataSet()                              { Assert(mem != NULL); free(mem);}

  u_int GetSize()              { return pos; }
  word GetArg(u_int f)         { Assert(f < size); return (word) mem[f]; }
  void SetArg(u_int f, word p) { Assert(f < size); mem[f] = (u_int) p; }
  void Clear()                 { pos = 0; }
  void Pop()                   { Assert(pos > 0); pos--; }
  void Push(word v) {
    if (pos >= size) {
      size += MEMCHUNK_SIZE; mem = (u_int *) realloc(mem, size);
    }
    Assert(mem != NULL); mem[pos++] = (u_int) v;
  }
};

#endif
