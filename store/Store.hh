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
#include "store/Types.hh"
#include "store/HeaderOp.hh"
#include "store/PointerOp.hh"
#include "store/Handler.hh"

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
    AssertStore(s > INVALID_BLOCKSIZE);
    AssertStore(s <= MAX_BLOCKSIZE);

    Block *t = (Block *) Store::FastAlloc((s + 1) << 2);
    AssertStore(t != INVALID_POINTER);
    HeaderOp::EncodeHeader(t, l, s);

    return t;
  }
public:
  // Init Functions
  static void InitStore(u_int mem_limits[STORE_GENERATION_NUM]);
  static void CloseStore();

  // GC Related Functions
  static word ResolveForwardPtr(word v);
  static word DoGC(word root);
  static void AddToIntgenSet(Block *v);
  static int NeedGC() {
    return needGC;
  }

  // DataLabel Function
  static BlockLabel MakeLabel(u_int l) {
    AssertStore(l <= MAX_HELPER_LABEL);
    return (BlockLabel) l;
  }
  // Allocation Functions
  static Block *AllocBlock(BlockLabel l, u_int s) {
    AssertStore(l >= MIN_DATA_LABEL);
    AssertStore(l <= MAX_HELPER_LABEL);
    return InternalAllocBlock(l, s);
  }
  static Block *AllocChunk(u_int s) {
    return Store::InternalAllocBlock(CHUNK_LABEL, s);
  }
  static Transient *AllocTransient(BlockLabel l) {
    AssertStore((l >= MIN_TRANSIENT_LABEL) && (l <= MAX_TRANSIENT_LABEL));
    return (Transient *) Store::InternalAllocBlock(l, 1);
  }
  static Block *AllocBlockWithHandler(u_int s, Handler *h) {
    Block *t = Store::InternalAllocBlock(HANDLER_BLOCK_LABEL, (s + 1));
    AssertStore(t != INVALID_POINTER);
    // ugly hack to avoid regrouping the items
    ((word *) t)[1] = PointerOp::EncodeUnmanagedPointer((void *) h);
    return t;
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
    AssertStore(((u_int) x & INTTAG) == INTTAG);
    return PointerOp::DecodeInt(x);
  }
  static Block *DirectWordToBlock(word x) {
    AssertStore(((u_int) x & TAGMASK) == BLKTAG);
    return PointerOp::DecodeBlock(x);
  }
  static void *DirectWordToUnmanagedPointer(word x) {
    AssertStore(((u_int) x & TAGMASK) == BLKTAG);
    return PointerOp::DecodeUnmanagedPointer(x);
  }
#if defined(STORE_DEBUG)
  static void MemStat();
  static void ForceGCGen(u_int gen);
#endif
};

// Defined Store Values Classes
#include "store/Value.hh"

#endif __STORE__STORE_HH__
