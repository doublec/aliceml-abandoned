#ifndef __store_hh__
#define __store_hh__

#include "base.hh"
#include "memchunk.hh"
#include "dataset.hh"
#include "headerop.hh"
#include "pointerop.hh"

class MemChain {
public:
  MemChunk *anchor;
  u_int total;      // total amount of mem within chain
  u_int used;       // used memory
  u_int gen;        // generation index
};

class MemConfig {
public:
  u_int max_gen;   // maximum number of generations
  u_int gc_active; // turn GC on/off
  virtual int NeedGC(MemChain **roots, u_int reqm, int gen) = 0;
};

class Store {
protected:
  static MemConfig *config;
  static MemChain **roots;
  static u_int totalHeapSize;
  static DataSet *root_set;
  static DataSet *intgen_set;
  static Block *WithGCAlloc(MemChain *chain, u_int size);
  static Block *NoGCAlloc(MemChain *chain, u_int size);
  static void Shrink(MemChain *chain, int threshold);
  static Block *CopyBlockToDst(Block *p, MemChain *dst);
  static void ScanChunks(MemChain *dst, u_int match_gen, MemChunk *anchor, char *scan);
  static void DoGC(u_int gen);
  static Block *Alloc(MemChain *chain, u_int size) {
    if (config->gc_active) {
      return Store::WithGCAlloc(chain, size);
    }
    else {
      return Store::NoGCAlloc(chain, size);
    }
  }
public:
  static void InitStore(MemConfig *cfg);
  static void CloseStore();
  // Type Access Functions
  static t_label MakeLabel(int l) {
    Assert(l> TAG0); Assert(l <= MAX_LSIZE); return (t_label) l;
  }
  // Allocation Functions
  static Block *AllocBlock(t_label l, u_int s) {
    Assert(s > INVALID_TSIZE);
    if (s < HeaderDef::MAX_HBSIZE) {
      Block *t = Store::Alloc(roots[0], (u_int) s + 1);

      HeaderOp::EncodeHeader(t, l, s, 0);
      return t;
    }
    else {
      char *t = (((char *) Store::Alloc(roots[0], (u_int) s + 1)) + 4);
      
      HeaderOp::EncodeHeader((Block *) t, l, HeaderDef::MAX_HBSIZE, 0);
      ((u_int *) t)[-1] = (u_int) s;
      
      return (Block *) t;
    }
  }
  static Block *AllocChunk(u_int s) {
    return Store::AllocBlock(CHUNK, s);
  }
  static Transient *AllocTransient(t_label l) {
    return (Transient *) Store::AllocBlock(l, 2);
  }
  // Conversion Functions
  static word IntToWord(int v) {
    return PointerOp::EncodeInt(v);
  }
  static int WordToInt(word v) {
    return PointerOp::DecodeInt(PointerOp::Deref(v));
  }
  static Block *WordToBlock(word v) {
    return PointerOp::DecodeBlock(PointerOp::Deref(v));
  }
  static Transient *WordToTransient(word v) {
    return PointerOp::DecodeTransient(PointerOp::Deref(v));
  }
  // GC Related Functions
  static DataSet *GetRootSet() {
    return root_set;
  }
  static void AddToIntgenSet(word v) {
    intgen_set->Push(v);
  }
#ifdef DEBUG_CHECK
  static void MemStat();
#endif
};

// Defined Store Values Classes
#include "value.hh"

#endif
