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
#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
struct timeval;
#endif

class Finalization {
public:
  virtual void Finalize(word value) = 0;
};

class Store {
private:
  static MemChunk *roots[STORE_GENERATION_NUM];
  static u_int memMax[STORE_GENERATION_NUM];
  static u_int memFree;
  static u_int memTolerance;
  static char *curChunkMax;
  static s_int curChunkTop;
  static MemChunk *curChunk;
  static u_int hdrGen;
  static u_int dstGen;
  static Set *intgenSet;
  static Set *wkDictSet;
  static u_int needGC;
  static Finalization *handler;
#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
  static struct timeval *sum_t;
#endif

  static u_int BlockMemSize(u_int args) {
    return (u_int) ((args + 1) * sizeof(u_int));
  }

  static void FreeMemChunks(MemChunk *chunk, const u_int threshold);
  static Block *CloneBlock(Block *p);
  static word ForwardWord(word p);
  static Block *ForwardSet(Block *p);
  static s_int CanFinalize(Block *p);
  static void CheneyScan(MemChunk *chunk, Block *scan);
  static void HandleInterGenerationalPointers(u_int gen);
  static Block *HandleWeakDictionaries();
  static u_int GetMemUsage(MemChunk *chunk);
  static char *GCAlloc(u_int size, u_int header);
  static char *GCAlloc(u_int size);
  static Block *AddToFinSet(Block *p, word value);
  static void SwitchToChunk(MemChunk *chunk);
  static void AllocNewMemChunk();
  static void AllocNewMemChunk(u_int size, const u_int gen);
  
  static char *Alloc(u_int size, u_int header) {
    for (;;) {
      char *p      = (curChunkMax + curChunkTop);
      s_int newtop = (curChunkTop + size);

      ((u_int *) p)[-1] = header;
      if (newtop >= 0) {
	AllocNewMemChunk();
	continue;
      }
      curChunkTop = newtop;
      return p;
    }
  }
  static Block *InternalAllocBlock(BlockLabel l, u_int s) {
    AssertStore(s >= MIN_BLOCKSIZE);
    AssertStore(s <= MAX_BIGBLOCKSIZE);
    return (Block *) Store::Alloc(BlockMemSize(s), HeaderOp::EncodeHeader(l, s, 0));
  }
  static void DoGC(word &root, const u_int gen);
public:
#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
  static u_int totalMem;
  static u_int gcLiveMem;
#endif
  // Init Functions
  static void InitStore(u_int mem_max[STORE_GENERATION_NUM], u_int mem_free, u_int mem_tolerance);
  static void CloseStore();

  // GC Related Functions
  static word ResolveForwardPtr(word v);
  static void DoGC(word &root);
  static void SetGCParams(u_int mem_free, u_int mem_tolerance);
  static void AddToIntgenSet(Block *v);
  static void RegisterWeakDict(WeakDictionary *v);
  static void RegisterFinalizer(Finalization *handler) {
    Store::handler = handler;
  }
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

    ((word *) p)[0] = PointerOp::EncodeInt(s);
    return (Chunk *) p;
  }
  static Transient *AllocTransient(BlockLabel l) {
    AssertStore((l >= MIN_TRANSIENT_LABEL) && (l <= MAX_TRANSIENT_LABEL));
    return (Transient *) Store::InternalAllocBlock(l, 1);
  }
  static Block *AllocBlockWithHandler(u_int s, Handler *h) {
    Block *t = Store::InternalAllocBlock(HANDLERBLOCK_LABEL, s);
    AssertStore(s >= 1);
    ((word *) t)[0] = PointerOp::EncodeUnmanagedPointer((void *) h);
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
  // Calculate Block Size according to given size (used only for assertions)
  static u_int SizeToBlockSize(u_int size) {
    return HeaderOp::TranslateSize(size);
  }
  // Calculate Block Byte Size according to given byte size (used only for assertions)
  static u_int SizeToChunkSize(u_int size) {
    u_int ws = (1 + (((size + sizeof(u_int)) - 1) / sizeof(u_int)));
    return (HeaderOp::TranslateSize(ws) * sizeof(u_int));
  }
#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
  static void MemStat();
  static void ResetTime();
  static struct timeval *ReadTime();
#endif
#if defined(STORE_DEBUG)
  static void ForceGC(word &root, const u_int gen);
#endif
};

// Defined Store Value Classes
#include "store/Value.hh"
#include "store/WeakDictionary.hh"

#endif
