#ifndef __GCHELPER_HH__
#define __GCHELPER_HH__

#if defined(INTERFACE)
#pragma interface
#endif

#include "base.hh"
#include "headerdef.hh"

class GCHelper {
public:
  // Moving Helper
  static u_int AlreadyMoved(Block *op) {
    return ((((u_int *) op)[0] & HeaderDef::GC_MASK) >> HeaderDef::GC_SHIFT);
  }
  static word GetForwardPtr(Block *op) {
    return ((word *) op)[1];
  }
  static void MarkMoved(Block *op, word np) {
    ((u_int *) op)[1]  = (u_int) np;
    ((u_int *) op)[0] |= (1 << HeaderDef::GC_SHIFT);
  }
  // Generation Encoding
  static void EncodeGen(Block *p, u_int gen) {
    *((u_int *) p) = (*((u_int *) p) & ~HeaderDef::GEN_MASK) | (gen << HeaderDef::GEN_SHIFT);
  }
  static u_int DecodeGen(Block *p) {
    return ((((u_int *) p)[0] & HeaderDef::GEN_MASK) >> HeaderDef::GEN_SHIFT);
  }
};

#endif
