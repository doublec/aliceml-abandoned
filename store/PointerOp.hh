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
#ifndef __POINTEROP_HH__
#define __POINTEROP_HH__

#if defined(INTERFACE)
#pragma interface
#endif

#include "base.hh"

class PointerOp {
public:
  // Core Tag Operations
  static word EncodeTag(Block *p, u_int tag) {
    return (word) ((u_int) p | tag);
  }
  static u_int DecodeTag(word p) {
    return ((u_int) p & (u_int) TAGMASK);
  }
  static Block *RemoveTag(word p) {
    return (Block *) ((u_int) p & ~((u_int) TAGMASK));
  }
  // Block<->Word Conversion
  static word EncodeBlock(Block *p) {
    return (word) p;
  }
  static Block *DecodeBlock(word p) {
    return (Block *) ((((u_int) p & (u_int) TAGMASK) == (u_int) BLKTAG) ? p : INVALID_POINTER);
  }
  // Transient<->Word Conversion
  static word EncodeTransient(Transient *p) {
    Assert(((u_int) p & (u_int) TAGMASK) == (u_int) BLKTAG);
    return (word) ((u_int) p | (u_int) TRTAG);
  }
  static Transient *DecodeTransient(word p) {
    return (Transient *) ((((u_int) p & (u_int) TAGMASK) == (u_int) TRTAG) ?
			  ((char *) p - (u_int) TRTAG) : INVALID_POINTER);
  }
  // int Test
  static int IsInt(word v) {
    return ((u_int) v & (u_int) INTMASK);
  }
  // int<->Word Conversion
  static word EncodeInt(int v) {
    Assert(v >= -(1 << 30));
    Assert(v <= (1 << 30));
    return (word) ((((u_int) v) << 1) | (u_int) INTTAG);
  }
  static int DecodeInt(word v) {
    if ((((u_int) v) & INTMASK) != INTTAG) {
      return (int) INVALID_INT;
    }
    else {
      return (int) (((u_int) v & (1 << 31)) ? ((1 << 31) | ((u_int) v >> 1)) : ((u_int) v >> 1));
    }
  }
  static word EncodeUnmanagedPointer(void *v) {
    Assert(((u_int) v & (1 << 31)) == 0);
    return (word) (((u_int) v << 1) | (u_int) INTTAG); 
  }
  static void *DecodeUnmanagedPointer(word v) {
    Assert((u_int) v & INTMASK);
    return (void *) ((u_int) v >> 1);
  }
  // Deref Function
  static word Deref(word v) {
    while (true) {
      u_int vi = (u_int) v;
 
      Assert(v != NULL);
      if (vi & INTMASK) {
	return v;
      }
      if ((vi & TAGMASK) == (u_int) TRTAG) {
	vi -= (u_int) TRTAG;
      }
      if (HeaderOp::DecodeLabel((Block *) vi) == REF) {
	v = ((word *) vi)[1];
      }
      else {
	return v;
      }
    }
  }
};

#endif
