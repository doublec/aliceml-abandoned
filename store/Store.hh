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
  u_int max_gen;     // maximum number of generations
  u_int *gen_limits; // gc limit for each memory section (in Bytes)
  u_int intgen_size; // initial intgen pointer threshold
};

class Store {
protected:
  static StoreConfig *config;
  static MemChain **roots;
  static u_int totalHeapSize;
  static word intgen_set;
  static u_int needGC;

  static void Shrink(MemChain *chain, int threshold);
  static Block *CopyBlockToDst(Block *p, MemChain *dst);
  static void ScanChunks(MemChain *dst, u_int match_gen, MemChunk *anchor, char *scan);
  static Block *Alloc(MemChain *chain, u_int size);
  static Block *InternalAllocBlock(BlockLabel l, u_int s) {
    Assert(s > INVALID_TSIZE);
    if (s < (MAX_HBSIZE - 1)) {
      Block *t = Store::Alloc(roots[0], (u_int) s + 1);
      
      Assert(t != NULL);
      HeaderOp::EncodeHeader(t, l, s);
      return t;
    }
    else {
      char *t = (((char *) Store::Alloc(roots[0], (u_int) s + 1)) + 4);
      
      Assert(t != NULL);
      HeaderOp::EncodeHeader((Block *) t, l, MAX_HBSIZE);
      ((u_int *) t)[-1] = (u_int) s;
      
      return (Block *) t;
    }
  }
public:
  // Init Functions
  static void InitStore(StoreConfig *cfg);
  static void CloseStore();

  // GC Related Functions
  static word DoGC(word root, u_int gen);
  static void AddToIntgenSet(Block *v);
  static int NeedGC() {
    return needGC;
  }
  // DataLabel Function
  static BlockLabel MakeLabel(u_int l) {
    Assert(l <= MAX_HELPERLABELSIZE);
    return (BlockLabel) l;
  }
  // Allocation Functions
  static Block *AllocBlock(BlockLabel l, u_int s) {
    Assert(l >= MIN_DATALABELSIZE);
    Assert(l <= MAX_HELPERLABELSIZE);
    return InternalAllocBlock(l, s);
  }
  static Block *AllocChunk(u_int s) {
    return Store::InternalAllocBlock(CHUNK, s);
  }
  static Block *AllocStack(u_int s) {
    return Store::InternalAllocBlock(STACK, s);
  }
  static Transient *AllocTransient(BlockLabel l) {
    Assert(l >= MIN_TRANSIENT && l <= MAX_TRANSIENT);
    return (Transient *) Store::InternalAllocBlock(l, 1);
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
  static word UnmanagedPointerToWord(void *v) {
    return PointerOp::EncodeUnmanagedPointer(v);
  }
  static void *WordToUnmanagedPointer(word x) {
    return PointerOp::DecodeUnmanagedPointer(x);
  }
#ifdef DEBUG_CHECK
  static void MemStat();
#endif
};

// Defined Store Values Classes
#include "value.hh"

#endif
