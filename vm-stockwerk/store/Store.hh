#ifndef __store_hh__
#define __store_hh__

#include "base.hh"
#include "memmanager.hh"
#include "tuple.hh"
#include "headerop.hh"
#include "pointerop.hh"

class Store : public MemManager {
protected:
  static DynamicTuple *intgen_set;
  static Tuple *CopyBlockToDst(Tuple *p, MemChain *dst);
  static void Store::ScanChunks(MemChain *dst, u_int match_gen, MemChunk *anchor, char *scan);
public:
  static void InitStore();
  // Type Access Functions
  static t_label GenLabel(int l) {
    Assert((l > BYNEED) && (l <= MAX_LSIZE)); return (t_label) l;
  }
  static t_size GenTSize(int s) {
    Assert((s > INVALID_TSIZE)); return (t_size) s;
  }
  static t_field GenTField(int f) {
    Assert(f > INVALID_FIELD); return (t_field) f;
  }
  // Allocation Functions
  static Tuple *AllocTuple(t_label l, t_size s) {
    Assert(s > INVALID_TSIZE);
    if (s < HeaderDef::MAX_HBSIZE) {
      Tuple *t = MemManager::Alloc(roots[0], (u_int) s + 1);
      
      HeaderOp::EncodeHeader(t, l, s, 0);
      return t;
    }
    else {
      char *t = (((char *) MemManager::Alloc(roots[0], (u_int) s + 1)) + 4);
      
      HeaderOp::EncodeHeader((Tuple *) t, l, (t_size) HeaderDef::MAX_HBSIZE, 0);
      ((u_int *) t)[-1] = (u_int) s;
      
      return (Tuple *) t;
    }
  }
  static Tuple *AllocChunk(t_size s) {
    return Store::AllocTuple(CHUNK, s);
  }
  static Transient *AllocTransient(t_label l) {
    return (Transient *) Store::AllocTuple(l, TRANS_SIZE);
  }
  // Conversion Functions
  static word IntToWord(int v) {
    return PointerOp::EncodeInt(v);
  }
  static int WordToInt(word v) {
    return PointerOp::DecodeInt(PointerOp::Deref(v));
  }
  static Tuple *WordToTuple(word v) {
    return PointerOp::DecodeTuple(PointerOp::Deref(v));
  }
  static Transient *WordToTransient(word v) {
    return PointerOp::DecodeTransient(PointerOp::Deref(v));
  }
  static void DoGC(DynamicTuple *root_set, u_int gen);
#ifdef DEBUG_CHECK
  static void MemStat();
#endif
};

// Defined Store Values Classes
#include "value.hh"

#endif
