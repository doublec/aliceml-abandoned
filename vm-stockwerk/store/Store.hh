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
#ifndef __STORE_HH__
#define __STORE_HH__

#if defined(INTERFACE)
#pragma interface
#endif

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

class StoreConfig {
public:
  u_int word_width;       // word datatype width (e.g. 32 or 64)
  u_int tag_width;        // block tag width
  u_int size_width;       // block size field width
  u_int generation_width; // block generations field width (max num below must fit in)
  u_int max_gen;          // maximum number of generations
  u_int *gen_limits;      // limit for each memory section (in Bytes)
};

class Store {
protected:
  static StoreConfig *config;
  static MemChain **roots;
  static u_int totalHeapSize;
  static DataSet *intgen_set;
  static u_int needGC;
  static void Shrink(MemChain *chain, int threshold);
  static Block *CopyBlockToDst(Block *p, MemChain *dst);
  static void ScanChunks(MemChain *dst, u_int match_gen, MemChunk *anchor, char *scan);
  static Block *Alloc(MemChain *chain, u_int size);
  static Block *InternalAllocBlock(t_label l, u_int s) {
    Assert(s > INVALID_TSIZE);
    if (s < HeaderDef::MAX_HBSIZE) {
      Block *t = Store::Alloc(roots[0], (u_int) s + 1);
      
      Assert(t != NULL);
      HeaderOp::EncodeHeader(t, l, s, 0);
      return t;
    }
    else {
      char *t = (((char *) Store::Alloc(roots[0], (u_int) s + 1)) + 4);
      
      Assert(t != NULL);
      HeaderOp::EncodeHeader((Block *) t, l, HeaderDef::MAX_HBSIZE, 0);
      ((u_int *) t)[-1] = (u_int) s;
      
      return (Block *) t;
    }
  }
public:
  static void InitStore(StoreConfig *cfg);
  static void CloseStore();
  // Type Access Functions
  static t_label MakeLabel(int l) {
    Assert(l>= BlockLabel::MIN_LSIZE); Assert(l <= BlockLabel::MAX_LSIZE); return (t_label) l;
  }
  // Allocation Functions
  static Block *AllocBlock(t_label l, u_int s) {
    Assert(l >= BlockLabel::MIN_LSIZE);
    Assert(l <= BlockLabel::MAX_LSIZE);
    return InternalAllocBlock(l, s);
  }
  static Block *AllocChunk(u_int s) {
    return Store::InternalAllocBlock(BlockLabel::CHUNK, s);
  }
  static Transient *AllocTransient(t_label l) {
    Assert((l == BlockLabel::PROMISE) || (l == BlockLabel::FUTURE) || (l == BlockLabel::BYNEED));
    return (Transient *) Store::InternalAllocBlock(l, 2);
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
  static void DoGC(DataSet *root_set, u_int gen);
  static void AddToIntgenSet(word v) {
    intgen_set->Push(v);
  }
  static int NeedGC() {
    return needGC;
  }
#ifdef DEBUG_CHECK
  static void MemStat();
#endif
};

// Defined Store Values Classes
#include "value.hh"

#endif
