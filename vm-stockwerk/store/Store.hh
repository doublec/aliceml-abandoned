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
#include "store/PointerOp.hh"
#include "store/StatusWord.hh"

#if defined(STORE_PROFILE)
struct timeval;
#endif

class WeakMap;
class JITStore;
class Profiler;
class MemChunk;

#define STORE_NEED_GC_STATUS 0

class DllExport Store : public StatusWord {
protected:
  friend class Map;
  friend class JITStore;
  friend class Profiler;
  static MemChunk *roots[STORE_GENERATION_NUM];
  static u_int memMax[STORE_GENERATION_NUM];
  static u_int memFree;
  static u_int memTolerance;
  static MemChunk *curChunk;
  static char *chunkTop;
  static char *chunkMax;
  static u_int hdrGen;
  static u_int dstGen;
  static u_int nbBlkTables;
#if defined(STORE_PROFILE)
  static struct timeval *sum_t;
#endif
  static u_int BlockMemSize(u_int args) {
    return (u_int) ((args + 1) * sizeof(u_int));
  }
  static void FreeMemChunks(MemChunk *chunk);
  static Block *CloneBlock(Block *p);
  static word ForwardWord(word p);
  static Block *ForwardSet(Block *p);
  static s_int CanFinalize(Block *p);
  static void CheneyScan(MemChunk *chunk, char *scan);
  static void FinalizeCheneyScan(MemChunk *chunk, char *scan);
  static void HandleInterGenerationalPointers(u_int gen);
  static Block *HandleWeakDictionaries();
  static u_int GetMemUsage(MemChunk *chunk);
  static char *GCAlloc(u_int size, u_int header);
  static char *GCAlloc(u_int size);
  static Block *AddToFinSet(Block *p, word value);
  static void SwitchToChunk(MemChunk *chunk);
  static void AllocNewMemChunkStd();
  static void AllocNewMemChunk(u_int size, const u_int gen);
//    static char *Alloc(u_int size, u_int header) {
//      for (;;) {
//        char *p      = curChunk->GetTop();
//        char *newtop = p + size;
//        if (newtop >= curChunk->GetMax()) {
//  	AllocNewMemChunkStd(size);
//  	continue;
//        }
//        curChunk->SetTop(newtop);
//        ((u_int *) p)[0] = header;
//        return p;
//      }
//    }
  static char *Alloc(u_int size, u_int header) {
    for (;;) {
      char *p = chunkTop;
      ((u_int *) p)[0] = header;
      char *newtop = p + size;
      if (newtop >= chunkMax) {
	AllocNewMemChunkStd();
	continue;
      }
      chunkTop = newtop;
      return p;
    }
  }
  static Block *InternalAllocBlock(BlockLabel l, u_int s) {
    AssertStore(s <= MAX_BIGBLOCKSIZE);
    s = HeaderOp::TranslateSize(s);
    return (Block *) Store::Alloc(BlockMemSize(s),
				  HeaderOp::EncodeHeader(l, s, 0));
  }
  static void NextGCLimits();
  static void DoGC(word &root, const u_int gen);
public:
  static u_int allowGC;
  static u_int forceGC;
#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
  static u_int totalMem;
  static u_int gcLiveMem;
#endif
  // Init Functions
  static void InitStore(u_int mem_max[STORE_GENERATION_NUM],
			u_int mem_free, u_int mem_tolerance);
  static void CloseStore();

  // GC Related Functions
  static void DoGC(word &root);
  static void SetGCParams(u_int mem_free, u_int mem_tolerance);
  static void AddToIntgenSet(Block *v);
  static void RegisterWeakDict(WeakMap *v);
  static u_int GCStatus() {
    return (1 << STORE_NEED_GC_STATUS);
  }
  static u_int NeedGC() {
    return StatusWord::GetStatus(GCStatus());
  }
  // DataLabel Function
  static BlockLabel MakeLabel(u_int l) {
    AssertStore(l <= MAX_HELPER_LABEL);
    return (BlockLabel) l;
  }
  // Allocation Functions
  static Block *AllocBlock(BlockLabel l, u_int s) {
    AssertStore(l >= MIN_DATA_LABEL);
    AssertStore(l <= MAX_STORE_LABEL); // to be done
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
  // Conversion Functions
  static word IntToWord(s_int v) {
    return PointerOp::EncodeInt(v);
  }
  static s_int WordToInt(word v) {
    return PointerOp::DecodeInt(PointerOp::Deref(v));
  }
  static Block *WordToBlock(word v) {
    Block *p = PointerOp::DecodeBlock(PointerOp::Deref(v));
    return p;
  }
  static Transient *DirectWordToTransient(word v) {
    AssertStore(((u_int) v & TAGMASK) == TRTAG);
    Transient *p = PointerOp::DirectDecodeTransient(PointerOp::Deref(v));
    return p;
  }
  static Transient *WordToTransient(word v) {
    Transient *p = PointerOp::DecodeTransient(PointerOp::Deref(v));
    return p;
  }
  static Chunk *WordToChunk(word v) {
    Chunk *p = PointerOp::DecodeChunk(PointerOp::Deref(v));
    return p;
  }
  static word UnmanagedPointerToWord(void *v) {
    return PointerOp::EncodeUnmanagedPointer(v);
  }
  static void *WordToUnmanagedPointer(word x) {
    return PointerOp::DecodeUnmanagedPointer(PointerOp::Deref(x));
  }
  static s_int DirectWordToInt(word x) {
    AssertStore(((u_int) x & INTMASK) == INTTAG);
    return PointerOp::DirectDecodeInt(x);
  }
  static Block *DirectWordToBlock(word x) {
    AssertStore(((u_int) x & TAGMASK) == BLKTAG);
    Block *p = PointerOp::DirectDecodeBlock(x);
    return p;
  }
  static Chunk *DirectWordToChunk(word x) {
    Chunk *p = PointerOp::DirectDecodeChunk(x);
    return p;
  }
  static void *DirectWordToUnmanagedPointer(word x) {
    AssertStore(((u_int) x & INTMASK) == INTTAG);
    return PointerOp::DirectDecodeUnmanagedPointer(x);
  }
  // Calculate Block Size according to given size (used only for assertions)
  static u_int SizeToBlockSize(u_int size) {
    return HeaderOp::TranslateSize(size);
  }
  static void JITReplaceArg(u_int i, Block *p, word v);
  static void MemStat();
#if defined(STORE_GC_DEBUG)
  static void VerifyGC(word root);
#endif
#if defined(STORE_DEBUG)
  static void ForceGC(word &root, const u_int gen);
#endif
#if defined(STORE_PROFILE)
  static void ResetTime();
  static struct timeval *ReadTime();
#endif
};

// Defined Store Value Classes
#include "store/Value.hh"

#endif
