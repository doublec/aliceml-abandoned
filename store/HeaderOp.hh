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

class HeaderOp {
public:
  // Header Creation and Access
  static u_int EncodeHeader(BlockLabel l, u_int s, u_int gen) {
    return (((gen + 1) << GEN_GC_SHIFT) | (s << SIZE_SHIFT) | (((u_int) l) << TAG_SHIFT));
  }
  static u_int GetHeader(Block *p) {
    AssertStore(p != INVALID_POINTER);
    return ((u_int *) p)[-1];
  }
  // Label Creation and Access
  static void EncodeLabel(Transient *p, BlockLabel l) {
    AssertStore(p != INVALID_POINTER);
    ((u_int *) p)[-1] = ((((u_int *) p)[-1] & ~TAG_MASK) | (((u_int) l) << TAG_SHIFT));
  }
  static BlockLabel DecodeLabel(Block *p) {
    AssertStore(p != INVALID_POINTER);
    return (BlockLabel) ((((u_int *) p)[-1] & TAG_MASK) >> TAG_SHIFT);
  }
  // Size Creation and Access
  static void EncodeSize(Block *p, u_int s) {
    AssertStore(p != INVALID_POINTER);
    ((u_int *) p)[-1] = ((((u_int *) p)[-1] & ~SIZE_MASK) | (s << SIZE_SHIFT));
  }
  static u_int DecodeSize(Block *p) {
    AssertStore(p != INVALID_POINTER);
    return (u_int) ((((u_int *) p)[-1] & SIZE_MASK) >> SIZE_SHIFT);
  }
  // Generation Access
  static u_int DecodeGeneration(Block *p) {
    AssertStore(p != INVALID_POINTER);
    return (((((u_int *) p)[-1] & GEN_GC_MASK) >> GEN_GC_SHIFT) - 1);
  }
  // Intgen Mark Access
  static void SetChildishFlag(Block *p) {
    AssertStore(p != INVALID_POINTER);
    ((u_int *) p)[-1] |= (1 << CHILDISH_SHIFT);
  }
  static void ClearChildishFlag(Block *p) {
    AssertStore(p != INVALID_POINTER);
    ((u_int *) p)[-1] &= ~(1 << CHILDISH_SHIFT);
  }
  static u_int IsChildish(Block *p) {
    AssertStore(p != INVALID_POINTER);
    return (((u_int *) p)[-1] & CHILDISH_MASK);
  }
};

#endif __STORE__HEADEROP_HH__
