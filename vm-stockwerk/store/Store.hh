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
#include "store/Handler.hh"
#include "store/PointerOp.hh"

class MemChunk;
class Set;

extern char *storeChunkTop;
extern char *storeChunkMax;
extern MemChunk *storeCurChunk;

class Store {
private:
  static MemChunk *roots[STORE_GENERATION_NUM];
  static u_int memUsage[STORE_GENERATION_NUM];
  static u_int memLimits[STORE_GENERATION_NUM];
  static word intgenSet;
  static word wkDictSet;
  static u_int needGC;
  static u_int gcGen;

  static void Shrink(MemChunk *list, int threshold);
  static Block *CopyBlockToDst(Block *p, u_int dst_gen, u_int cpy_gen);
  static word ForwardBlock(word p, u_int dst_gen, u_int cpy_gen, u_int match_gen);
  static void ScanChunks(u_int dst_gen, u_int cpy_gen, u_int match_gen,
			 MemChunk *anchor, char *scan);
  static void HandleInterGenerationalPointers(Set *intgen_set, Set *new_intgen_set,
					      u_int gcGen, u_int dst_gen, u_int cpy_gen);
  static Block *HandleWeakDictionaries(Set *wkdict_set, Set *new_wkdict_set,
				       u_int match_gen, u_int dst_gen, u_int cpy_gen);
  static char *GCAlloc(u_int s, u_int gen);
  static Block *AllocFinSet(u_int size, u_int dst_gen, u_int cpy_gen);
  static Block *PushToFinSet(Block *p, Handler *h, word value, u_int dst_gen, u_int cpy_gen);
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

    Block *t = (Block *) Store::FastAlloc(((s + 1) * sizeof(u_int)));
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
  static void DoGC(word &root);
  // To be determined
  static void AddToIntgenSet(Block *v);
  static void RegisterWeakDict(WeakDictionary *v);
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
  static Chunk *AllocChunk(u_int s) {
    u_int ws = (1 + (((s + sizeof(u_int)) - 1) / sizeof(u_int)));
    Block *p = Store::InternalAllocBlock(CHUNK_LABEL, ws);

    ((word *) p)[1] = PointerOp::EncodeInt(s);
    return (Chunk *) p;
  }
  static Transient *AllocTransient(BlockLabel l) {
    AssertStore((l >= MIN_TRANSIENT_LABEL) && (l <= MAX_TRANSIENT_LABEL));
    return (Transient *) Store::InternalAllocBlock(l, 1);
  }
  static Block *AllocBlockWithHandler(BlockLabel l, u_int s, Handler *h) {
    Block *t = Store::AllocBlock(l, (s + 1));
    HeaderOp::SetHandlerMark(t);
    PointerOp::EncodeHandler(t, h);
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
  static Chunk *WordToChunk(word v) {
    return PointerOp::DecodeChunk(PointerOp::Deref(v));
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
  static Chunk *DirectWordToChunk(word x) {
    return PointerOp::DecodeChunk(x);
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

// Defined Store Value Classes
#include "store/Value.hh"
#include "store/WeakDictionary.hh"

#endif __STORE__STORE_HH__
