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
#ifndef __STORE__STORE_HH__
#define __STORE__STORE_HH__

#if defined(INTERFACE)
#pragma interface "store/Store.hh"
#endif

#include "store/Base.hh"
#include "store/HeaderOp.hh"
#include "store/PointerOp.hh"

class MemChunk;

extern char *storeChunkTop;
extern char *storeChunkMax;
extern MemChunk *storeCurChunk;

class Store {
private:
  static MemChunk *roots[STORE_GENERATION_NUM];
  static u_int memUsage[STORE_GENERATION_NUM];
  static u_int memLimits[STORE_GENERATION_NUM];
  static word intgenSet;
  static u_int needGC;
  static u_int gcGen;

  static void Shrink(MemChunk *list, int threshold);
  static Block *CopyBlockToDst(Block *p, u_int dst_gen, u_int cpy_gen);
  static word ForwardBlock(word p, u_int dst_gen, u_int cpy_gen, u_int match_gen);
  static void ScanChunks(u_int dst_gen, u_int cpy_gen, u_int match_gen,
			 MemChunk *anchor, char *scan);
  static char *GCAlloc(u_int s, u_int gen);
  static void SwitchToNewChunk(MemChunk *chunk);
  static void AllocNewMemChunk(u_int size, u_int gen);
  
  static char *FastAlloc(u_int size) {
  retry:
    char *top = storeChunkTop;

    storeChunkTop += size;
    if (storeChunkTop > storeChunkMax) {
      AllocNewMemChunk(size, 0);
      goto retry;
    }

    return top;
  }
  static Block *InternalAllocBlock(BlockLabel l, u_int s) {
    Assert(s > INVALID_BLOCKSIZE);
    Assert(s <= MAX_BLOCKSIZE);

    Block *t = (Block *) Store::FastAlloc((s + 1) << 2);
    Assert(t != NULL);
    HeaderOp::EncodeHeader(t, l, s);

    return t;
  }
public:
  // Init Functions
  static void InitStore(u_int mem_limits[STORE_GENERATION_NUM]);
  static void CloseStore();

  // GC Related Functions
  static word DoGC(word root);
  static void AddToIntgenSet(Block *v);
  static int NeedGC() {
    return needGC;
  }

  // DataLabel Function
  static BlockLabel MakeLabel(u_int l) {
    Assert(l <= MAX_HELPER_LABEL);
    return (BlockLabel) l;
  }
  // Allocation Functions
  static Block *AllocBlock(BlockLabel l, u_int s) {
    Assert(l >= MIN_DATA_LABEL);
    Assert(l <= MAX_HELPER_LABEL);
    return InternalAllocBlock(l, s);
  }
  static Block *AllocChunk(u_int s) {
    return Store::InternalAllocBlock(CHUNK_LABEL, s);
  }
  static Transient *AllocTransient(BlockLabel l) {
    Assert((l >= MIN_TRANSIENT_LABEL) && (l <= MAX_TRANSIENT_LABEL));
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
    return PointerOp::DecodeUnmanagedPointer(PointerOp::Deref(x));
  }
  static int DirectWordToInt(word x) {
    Assert(((u_int) x & INTTAG) == INTTAG);
    return PointerOp::DecodeInt(x);
  }
  static Block *DirectWordToBlock(word x) {
    Assert(((u_int) x & TAGMASK) == BLKTAG);
    return PointerOp::DecodeBlock(x);
  }
  static void *DirectWordToUnmanagedPointer(word x) {
    Assert(((u_int) x & TAGMASK) == BLKTAG);
    return PointerOp::DecodeUnmanagedPointer(x);
  }
#if defined(DEBUG_CHECK)
  static void MemStat();
  static void ForceGCGen(u_int gen);
#endif
};

// Defined Store Values Classes
#include "store/Value.hh"

#endif __STORE__STORE_HH__
