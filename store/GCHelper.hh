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
#ifndef __STORE__GCHELPER_HH__
#define __STORE__GCHELPER_HH__

#if defined(INTERFACE)
#pragma interface "store/GCHelper.hh"
#endif

class GCHelper {
public:
  // Moving Helper
  static u_int AlreadyMoved(Block *p) {
    return (!((((u_int *) p)[0] & GEN_GC_MASK) >> GEN_GC_SHIFT));
  }
  static Block *GetForwardPtr(Block *p) {
    return ((Block **) p)[0];
  }
  static void MarkMoved(Block *p, Block *np) {
    ((Block **) p)[0] = np; // test for 8 byte Alignment to be determined
  }
  // Generation Encoding
  static void EncodeGen(Block *p, u_int gen) {
    ((u_int *) p)[0] = (((u_int *) p)[0] & ~GEN_GC_MASK) | ((gen + 1) << GEN_GC_SHIFT);
  }
  static u_int DecodeGen(Block *p) {
    return ((((u_int *) p)[0] & GEN_GC_MASK) >> GEN_GC_SHIFT);
  }
  // like PointerOp::Deref, but be careful about forwards
  static word Deref(word v) {
  loop:
    u_int vi = (u_int) v;

    if (!((vi ^ TRTAG) & TAGMASK)) {
      vi ^= (u_int) TRTAG;
      
      if (!AlreadyMoved((Block *) vi) &&
	  HeaderOp::DecodeLabel((Block *) vi) == REF_LABEL) {
	v = ((word *) vi)[1];
	goto loop;
      }
    }

    return v;
  }
};

#endif
