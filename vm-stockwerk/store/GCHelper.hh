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
#ifndef __STORE__GCHELPER_HH__
#define __STORE__GCHELPER_HH__

#if defined(INTERFACE)
#pragma interface "store/GCHelper.hh"
#endif

class GCHelper {
public:
  // Moving Helper
  static u_int AlreadyMoved(Block *op) {
    return ((((u_int *) op)[0] & GC_MASK) >> GC_SHIFT);
  }
  static word GetForwardPtr(Block *op) {
    return ((word *) op)[1];
  }
  static void MarkMoved(Block *op, word np) {
    ((u_int *) op)[1]  = (u_int) np;
    ((u_int *) op)[0] |= (1 << GC_SHIFT);
  }
  // Generation Encoding
  static void EncodeGen(Block *p, u_int gen) {
    *((u_int *) p) = (*((u_int *) p) & ~GEN_MASK) | (gen << GEN_SHIFT);
  }
  static u_int DecodeGen(Block *p) {
    return ((((u_int *) p)[0] & GEN_MASK) >> GEN_SHIFT);
  }
};

#endif __STORE__GCHELPER_HH__
