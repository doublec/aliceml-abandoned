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
  static void EncodeHeader(Block *t, BlockLabel l, u_int s) {
    AssertStore(t != INVALID_POINTER);
    ((u_int *) t)[0] = (s << SIZE_SHIFT) | (((u_int) l) << TAG_SHIFT);
  }
  static u_int GetHeader(Block *p) {
    AssertStore(p != INVALID_POINTER);
    return *((u_int *) p);
  }
  // Label Creation and Access
  static void EncodeLabel(Transient *p, BlockLabel l) {
    AssertStore(p != INVALID_POINTER);
    ((u_int *) p)[0] = ((((u_int *) p)[0] & ~TAG_MASK) | (((u_int) l) << TAG_SHIFT));
  }
  static BlockLabel DecodeLabel(Block *p) {
    AssertStore(p != INVALID_POINTER);
    return (BlockLabel) ((((u_int *) p)[0] & TAG_MASK) >> TAG_SHIFT);
  }
  // Size Creation and Access
  static void EncodeSize(Block *p, u_int s) {
    AssertStore(p != INVALID_POINTER);
    ((u_int *) p)[0] = ((((u_int *) p)[0] & ~SIZE_MASK) | (s << SIZE_SHIFT));
  }
  static u_int DecodeSize(Block *p) {
    AssertStore(p != INVALID_POINTER);
    return (u_int) ((((u_int *) p)[0] & SIZE_MASK) >> SIZE_SHIFT);
  }
  // Generation Access
  static u_int DecodeGeneration(Block *p) {
    AssertStore(p != INVALID_POINTER);
    return ((*((u_int *) p)) >> GEN_SHIFT);
  }
  // Intgen Mark Access
  static void SetIntgenMark(Block *p) {
    AssertStore(p != INVALID_POINTER);
    ((u_int *) p)[0] |= (1 << INTGEN_SHIFT);
  }
  static void ClearIntgenMark(Block *p) {
    AssertStore(p != INVALID_POINTER);
    ((u_int *) p)[0] &= ~(1 << INTGEN_SHIFT);
  }
  static u_int HasIntgenMark(Block *p) {
    AssertStore(p != INVALID_POINTER);
    return (((u_int *) p)[0] & INTGEN_MASK);
  }
  // Handler Flag Access
  static void SetHandlerMark(Block *p) {
    AssertStore(p != INVALID_POINTER);
    ((u_int *) p)[0] |= (1 << HANDLER_SHIFT);
  }
  static u_int HasHandlerMark(Block *p) {
    AssertStore(p != INVALID_POINTER);
    return (((u_int *) p)[0] & HANDLER_MASK);
  }
};

#endif __STORE__HEADEROP_HH__
