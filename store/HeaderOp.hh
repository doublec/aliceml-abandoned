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
#ifndef __STORE__HEADEROP_HH__
#define __STORE__HEADEROP_HH__

#if defined(INTERFACE)
#pragma interface "store/HeaderOp.hh"
#endif

class SeamDll HeaderOp {
public:
  // Header Creation and Access
  static u_int EncodeHeader(BlockLabel l, u_int s, u_int gen,
			    u_int mutableFlag=0) {
    AssertStore((u_int) l <= MAX_STORE_LABEL);
    AssertStore(gen < STORE_GENERATION_NUM);
    AssertStore(mutableFlag == 0 || mutableFlag == 1);
    if (s > MAX_BLOCKSIZE) {
      Assert(s <= MAX_BIGBLOCKSIZE);
      return (((gen + 1) << GEN_GC_SHIFT) |
	      (1 << SIZESHIFT_SHIFT) |
	      ((s >> SIZESHIFT_MASK) << SIZE_SHIFT) |
	      (((u_int) l) << TAG_SHIFT |
	       mutableFlag << MUTABLE_SHIFT));
    }
    else {
      return (((gen + 1) << GEN_GC_SHIFT) | (s << SIZE_SHIFT) |
	      (((u_int) l) << TAG_SHIFT | mutableFlag << MUTABLE_SHIFT));
    }
  }
  static u_int GetHeader(Block *p) {
    AssertStore(p != INVALID_POINTER);
    return ((u_int *) p)[0];
  }
  // Label Creation and Access
  static void EncodeLabel(Transient *p, BlockLabel l) {
    AssertStore(p != INVALID_POINTER);
    AssertStore((u_int) l <= MAX_STORE_LABEL);
    ((u_int *) p)[0] =
      ((((u_int *) p)[0] & ~TAG_MASK) | (((u_int) l) << TAG_SHIFT));
  }
  static BlockLabel DecodeLabel(Block *p) {
    AssertStore(p != INVALID_POINTER);
    return (BlockLabel) ((((u_int *) p)[0] & TAG_MASK) >> TAG_SHIFT);
  }
  static u_int DecodeMutableFlag(Block *p) {
    AssertStore(p != INVALID_POINTER);
    return (u_int) ((((u_int *) p)[0] & MUTABLE_MASK) >> MUTABLE_SHIFT);
  }
  static void EncodeMutableFlag(Block *p, u_int f) {
    AssertStore(p != INVALID_POINTER);
    AssertStore(f<=1);
    ((u_int *) p)[0] =
      ((((u_int *) p)[0] & ~MUTABLE_MASK) | (f << MUTABLE_SHIFT));
  }
  // Size Creation and Access
  static void EncodeSize(Block *p, u_int s) {
    AssertStore(p != INVALID_POINTER);
    AssertStore(s <= MAX_BLOCKSIZE);
    ((u_int *) p)[0] = ((((u_int *) p)[0] & ~SIZE_MASK) | (s << SIZE_SHIFT));
  }
  static u_int DecodeSize(Block *p) {
    AssertStore(p != INVALID_POINTER);
    u_int h = ((u_int *) p)[0];
    return (u_int) (((h & SIZE_MASK) >> SIZE_SHIFT) << (h & SIZESHIFT_MASK));
  }
  // Generation Access
  static u_int DecodeGeneration(Block *p) {
    AssertStore(p != INVALID_POINTER);
    return (((((u_int *) p)[0] & GEN_GC_MASK) >> GEN_GC_SHIFT) - 1);
  }
  // Intgen Mark Access
  static void SetChildishFlag(Block *p) {
    AssertStore(p != INVALID_POINTER);
    ((u_int *) p)[0] |= (1u << CHILDISH_SHIFT);
  }
  static void ClearChildishFlag(Block *p) {
    AssertStore(p != INVALID_POINTER);
    ((u_int *) p)[0] &= ~(1 << CHILDISH_SHIFT);
  }
  static u_int IsChildish(Block *p) {
    AssertStore(p != INVALID_POINTER);
    return (((u_int *) p)[0] & CHILDISH_MASK);
  }
  static u_int TranslateSize(u_int size) {
    if (size <= MAX_BLOCKSIZE) {
      return size;
    }
    return ((((size + BIGSIZE_MIN) >> SIZESHIFT_MASK) << SIZESHIFT_MASK));
  }
};

#endif
