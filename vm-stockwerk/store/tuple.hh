#ifndef __tuple_hh__
#define __tuple_hh__

#include "types.hh"

#define DYNAMIC_GROW_STEP 20

class DynamicTuple {
protected:
  u_int size, pos;
  u_int *mem;
public:
  DynamicTuple(u_int sz) : size(sz), pos(0) { mem = (u_int *) malloc(sz << 2); }
  ~DynamicTuple()                           { Assert(mem != NULL); free(mem);}

  inline t_size GetSize()     { return (t_size) pos; }
  inline word GetArg(u_int f) { Assert(f < size); return (word) mem[f]; }
  inline void Clear()         { pos = 0; }
  inline void SetArg(u_int f, word p) {
    while (f >= size) { size += DYNAMIC_GROW_STEP; mem = (u_int *) realloc(mem, size << 2); }
    mem[f] = (u_int) p;
  }
  inline void Add(word p) {
    while (pos >= size) { size += DYNAMIC_GROW_STEP; mem = (u_int *) realloc(mem, size << 2); }
    mem[pos++] = (u_int) p;
  }
};

#endif
