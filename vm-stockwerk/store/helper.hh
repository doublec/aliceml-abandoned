#ifndef __helper_hh_
#define __helper_hh_

#include "base.hh"

class Helper {
public:
  inline static void EncodeHeader(b_pointer t, t_label l, t_size s, u_int g) {
    ((u_int *) t)[0] =  0x00 | (((u_int) s) << HeaderDef::SIZE_SHIFT) |
      (((u_int) l) << HeaderDef::TAG_SHIFT) | (g << HeaderDef::GEN_SHIFT);
  }
  inline static t_label DecodeLabel(b_pointer p) {
    return (t_label) ((((u_int *) p)[0] & HeaderDef::TAG_MASK) >> HeaderDef::TAG_SHIFT);
  }
  inline static void SetLabel(t_pointer p, t_label l) {
    ((u_int *) p)[0] = ((((u_int *) p)[0] & ~HeaderDef::TAG_MASK) |
			(((u_int) l) << HeaderDef::TAG_SHIFT));
  }
  inline static t_size BlankDecodeSize(b_pointer p) {
    return (t_size) ((((u_int *) p)[0] & HeaderDef::SIZE_MASK) >> HeaderDef::SIZE_SHIFT);
  }
  inline static t_size DecodeSize(b_pointer p) {
    t_size s = Helper::BlankDecodeSize(p);

    return (t_size) ((s < HeaderDef::MAX_HBSIZE) ? s : Helper::DecodeInt((word) ((char *) p - 4)));
  }
  inline static word EncodeInt(int v) {
    Assert(v > -(1 << 30));
    Assert(v < (1 << 30));
    return (word) ((((u_int) v) << 1) | (u_int) INTTAG);
  }
  inline static int IsInt(word v) {
    return ((u_int) v & (u_int) INTMASK);
  }
  inline static u_int DecodeTag(word p) {
    return ((u_int) p & (u_int) TAGMASK);
  }
  inline static b_pointer RemoveTag(word p) {
    return (b_pointer) ((u_int) p & ~((u_int) TAGMASK));
  }
  inline static word EncodeTag(b_pointer p, u_int tag) {
    return (word) ((u_int) p | tag);
  }
  inline static void EncodeGen(b_pointer p, u_int gen) {
    *((u_int *) p) = (*((u_int *) p) & ~HeaderDef::GEN_MASK) | (gen << HeaderDef::GEN_SHIFT);
  }
  inline static u_int DecodeGen(b_pointer p) {
    return ((((u_int *) p)[0] & HeaderDef::GEN_MASK) >> HeaderDef::GEN_SHIFT);
  }
  inline static void MarkMoved(b_pointer op, word np) {
    ((u_int *) op)[1]  = (u_int) np;
    ((u_int *) op)[0] |= (1 << HeaderDef::GC_SHIFT);
  }
  inline static u_int AlreadyMoved(b_pointer op) {
    return ((((u_int *) op)[0] & HeaderDef::GC_MASK) >> HeaderDef::GC_SHIFT);
  }
  inline static word GetForwardPtr(b_pointer op) {
    return (word) ((u_int *) op)[1];
  }
  inline static int DecodeInt(word v) {
    Assert((((u_int) v) & INTMASK) == 1);
    return (int) ((u_int) v >> 1);
  }
  inline static word EncodeBPointer(b_pointer p) {
    Assert(((u_int) p & (u_int) TAGMASK) == 0);
    return (word) p;
  }
  inline static b_pointer DecodeBPointer(word p) {
    return (b_pointer) ((((u_int) p & (u_int) TAGMASK) == 0) ? p : INVALID_BPOINTER);
  }
  inline static word EncodeTPointer(t_pointer p) {
    Assert(((u_int) p & (u_int) TAGMASK) == 0);
    return (word) ((u_int) p | (u_int) TRTAG);
  }
  inline static t_pointer DecodeTPointer(word p) {
    return (t_pointer) ((((u_int) p & (u_int) TAGMASK) == 2) ? ((char *) p-2) : INVALID_BPOINTER);
  }
  inline static b_pointer TPointerToBPointer(t_pointer v) {
    return (b_pointer) v;
  }
  inline static word Deref(word v) {
    while (true) {
      u_int vi = (u_int) v;

      Assert(v != NULL);
      if ((vi & TAGMASK) <= 1) {
	return v;
      }
      else {
	vi -= 2;
	if (Helper::DecodeLabel((b_pointer) vi) == REF) {
	  v = (word) ((u_int *) vi)[1];
	}
	else {
	  return v;
	}
      }
    }
  }
  inline static void ShareBind(t_pointer t, word v, t_label l) {
    switch (Helper::DecodeLabel(Helper::TPointerToBPointer(t))) {
    case PROMISE:
    case FUTURE:
      Store::ReplaceArg(Helper::TPointerToBPointer(t), Store::GenTField(1), v);
      Helper::SetLabel(t, REF);
      break;
    default:
      Assert(0);
    }
  }
  inline static u_int GetHeader(b_pointer p) {
    return *((u_int *) p);
  }
  inline static b_pointer CopyBlockToDst(b_pointer p, MemChain *dst) {
    t_size s = Helper::BlankDecodeSize(p);
 
    if (s == HeaderDef::MAX_HBSIZE) {
      b_pointer newp, realnp;

      p      = (b_pointer) ((char *) p - 4);
      s      = *((t_size *) p);
      newp   = MemManager::Alloc(dst, (s + 2));
      realnp = (b_pointer) ((char *) newp + 4);

      memcpy(newp, p, (s + 2) << 2);
      Helper::EncodeGen(realnp, dst->gen);
      return realnp;
    }
    else {
      b_pointer newp = MemManager::Alloc(dst, (s + 1));
	
      memcpy(newp, p, (s + 1) << 2);
      Helper::EncodeGen(newp, dst->gen);
      return newp;
    }
  }
};

#endif
