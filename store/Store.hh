#ifndef __store_hh__
#define __store_hh__

#include "base.hh"
#include "memmanager.hh"
#include "tuple.hh"

class Store : public MemManager {
protected:
  // class Helper;
#include "helper.hh"
  static DynamicTuple *intgen_set;
  static void Store::ScanChunks(MemChain *dst, u_int match_gen, MemChunk *anchor, char *scan);
public:
  inline static void InitStore() {
    MemManager::InitMemManager();
    intgen_set = new DynamicTuple(64);
  }
  inline static t_label GenLabel(int l) {
    Assert((l > BYNEED) && (l <= MAX_LSIZE)); return (t_label) l;
  }
  inline static t_size GenTSize(int s) {
    Assert((s > INVALID_TSIZE)); return (t_size) s;
  }
  inline static t_field GenTField(int f) {
    Assert(f > INVALID_FIELD); return (t_field) f;
  }
  inline static b_pointer AllocTuple(t_label l, t_size s) {
    Assert(s > INVALID_TSIZE);
    if (s < HeaderDef::MAX_HBSIZE) {
      b_pointer t = MemManager::Alloc(roots[0], (u_int) s + 1);
      
      Helper::EncodeHeader(t, l, s, 0);
      return t;
    }
    else {
      char *t = (((char *) MemManager::Alloc(roots[0], (u_int) s + 1)) + 4);
      
      Helper::EncodeHeader((b_pointer) t, l, (t_size) HeaderDef::MAX_HBSIZE, 0);
      ((u_int *) t)[-1] = (u_int) s;
      
      return (b_pointer) t;
    }
  }
  inline static b_pointer AllocChunk(t_size s) {
    return Store::AllocTuple(CHUNK, s);
  }
  inline static t_pointer AllocTransient(t_label l) {
    return (t_pointer) Store::AllocTuple(l, TRANS_SIZE);
  }
  inline static word IntToWord(int v) {
    return Helper::EncodeInt(v);
  }
  inline static t_label GetLabel(b_pointer v) {
    Assert(v != INVALID_BPOINTER); return Helper::DecodeLabel(v);
  }
  inline static t_size GetSize(b_pointer v) {
    Assert(v != INVALID_BPOINTER); return Helper::DecodeSize(v);
  }
  inline static word GetArg(b_pointer t, t_field f) {
    Assert(t != INVALID_BPOINTER);
    Assert((f > (t_field) INVALID_TSIZE));
    Assert(f <= (t_field) Helper::DecodeSize(t));
    return (word) ((u_int *) t)[(u_int) f];
  }
  inline static void SetArg(b_pointer t, t_field f, word v) {
    Assert(t != INVALID_BPOINTER);
    Assert(f > (t_field) INVALID_TSIZE);
    Assert(f <= (t_field) Helper::DecodeSize(t));
    ((word *) t)[(u_int) f] = v;
  }
  inline static void ReplaceArg(b_pointer t, t_field f, word v) {
    Assert(t != INVALID_BPOINTER);
    Assert(f > (t_field) INVALID_TSIZE);
    Assert(f <= (t_field) Helper::DecodeSize(t));
    
    if (!Helper::IsInt(v) && (Helper::GetHeader(Helper::RemoveTag(v)) < Helper::GetHeader(t))) {
      intgen_set->Add(BPointerToWord(t));
    }
    ((word *) t)[(u_int) f] = v;
  }
  inline static t_label GetLabel(t_pointer v) {
    return GetLabel(Helper::TPointerToBPointer(v));
  }
  inline static word GetArg(t_pointer v, t_field f) {
    return GetArg(Helper::TPointerToBPointer(v), f);
  }
  inline static void SetArg(t_pointer t, t_field f, word v) {
    SetArg(Helper::TPointerToBPointer(t), f, v);
  }
  inline static void Bind(t_pointer t, word v) {
    Helper::ShareBind(t, v, REF);
  }
  inline static void Cancelled(t_pointer t, word ex) {
    Helper::ShareBind(t, ex, CANCELLED);
  }
  inline static word BPointerToWord(b_pointer p) {
    return Helper::EncodeBPointer(p);
  }
  inline static word TPointerToWord(t_pointer p) {
    return Helper::EncodeTPointer(p);
  }
  inline static b_pointer WordToBPointer(word v) {
    return Helper::DecodeBPointer(Helper::Deref(v));
  }
  inline static t_pointer WordToTPointer(word v) {
    return Helper::DecodeTPointer(Helper::Deref(v));
  }
  inline static int WordToInt(word v) {
    return Helper::DecodeInt(Helper::Deref(v));
  }
  static void DoGC(DynamicTuple *root_set, u_int gen);
#ifdef DEBUG_CHECK
  static void MemStat();
#endif
};

#endif
