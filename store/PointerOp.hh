#ifndef __pointerop_hh__
#define __pointerop_hh__

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
    Assert(v > -(1 << 30));
    Assert(v < (1 << 30));
    return (word) ((((u_int) v) << 1) | (u_int) INTTAG);
  }
  static int DecodeInt(word v) {
    Assert((((u_int) v) & INTMASK) == INTTAG);
    return (int) ((u_int) v >> 1);
  }
  // Deref Function
  static word Deref(word v) {
    while (true) {
      u_int vi = (u_int) v;
 
      Assert(v != NULL);
      if ((vi & TAGMASK) == (u_int) TRTAG) {
	vi -= (u_int) TRTAG;
	if (HeaderOp::DecodeLabel((Block *) vi) == REF) {
	  v = ((word *) vi)[1];
	}
	else {
	  return v;
	}
      }
      else {
	return v;
      }
    }
  }
};

#endif
