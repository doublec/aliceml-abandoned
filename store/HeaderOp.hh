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
#ifndef __HEADEROP_HH__
#define __HEADEROP_HH__

#if defined(INTERFACE)
#pragma interface
#endif

#include "base.hh"
#include "headerdef.hh"

class HeaderOp {
public:
  // Header Creation and Acess
  static void EncodeHeader(Block *t, BlockLabel l, u_int s, u_int g) {
    Assert(t != NULL);
    ((u_int *) t)[0] =  (s << SIZE_SHIFT) | (((u_int) l) << TAG_SHIFT) | (g << GEN_SHIFT);
  }
  static u_int GetHeader(Block *p) {
    Assert(p != NULL); return *((u_int *) p);
  }
  // Label Access
  static void EncodeLabel(Transient *p, BlockLabel l) {
    Assert(p != NULL);
    ((u_int *) p)[0] = ((((u_int *) p)[0] & ~TAG_MASK) | (((u_int) l) << TAG_SHIFT));
  }
  static BlockLabel DecodeLabel(Block *p) {
    Assert(p != NULL);
    return (BlockLabel) ((((u_int *) p)[0] & TAG_MASK) >> TAG_SHIFT);
  }
  // Size Access
  static u_int BlankDecodeSize(Block *p) {
    Assert (p != NULL);
    return (u_int) ((((u_int *) p)[0] & SIZE_MASK) >> SIZE_SHIFT);
  }
  static u_int DecodeSize(Block *p) {
    u_int s = BlankDecodeSize(p);
    return (u_int) ((s < MAX_HBSIZE) ? s : (*((u_int *) ((char *) p - 4)) >> 1));
  }
  static void EncodeSize(Block *p, u_int s) {
    if (HeaderOp::BlankDecodeSize(p) == MAX_HBSIZE) {
      *((u_int *) ((char *) p - 4)) = s;
    }
    else {
      ((u_int *) p)[0] = ((((u_int *) p)[0] & ~SIZE_MASK) | (s << SIZE_SHIFT));
    }
  }
  // Generation Access
  static u_int DecodeGeneration(Block *p) {
    Assert(p != NULL); return ((*((u_int *) p)) >> GEN_SHIFT);
  }
  // Intgen Mark Access
  static void SetIntgenMark(Block *p) {
    Assert(p != NULL);
    ((u_int *) p)[0] |= (1 << INTGEN_SHIFT);
  }
  static void ClearIntgenMark(Block *p) {
    Assert(p != NULL);
    ((u_int *) p)[0] &= ~(1 << INTGEN_SHIFT);
  }
  static u_int HasIntgenMark(Block *p) {
    Assert(p != NULL);
    return (((u_int *) p)[0] & INTGEN_MASK);
  }

};

#endif
