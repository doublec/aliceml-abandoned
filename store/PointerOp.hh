#ifndef __pointerop_hh__
#define __pointerop_hh__

#include "base.hh"

class PointerOp {
public:
  // Core Tag Operations
  static word EncodeTag(Tuple *p, u_int tag) {
    return (word) ((u_int) p | tag);
  }
  static u_int DecodeTag(word p) {
    return ((u_int) p & (u_int) TAGMASK);
  }
  static Tuple *RemoveTag(word p) {
    return (Tuple *) ((u_int) p & ~((u_int) TAGMASK));
  }
  // Tuple<->Word Conversion
  static word EncodeTuple(Tuple *p) {
    return (word) p;
  }
  static Tuple *DecodeTuple(word p) {
    return (Tuple *) ((((u_int) p & (u_int) TAGMASK) == 0) ? p : INVALID_POINTER);
  }
  // Transient<->Word Conversion
  static word EncodeTransient(Transient *p) {
    Assert(((u_int) p & (u_int) TAGMASK) == 0);
    return (word) ((u_int) p | (u_int) TRTAG);
  }
  static Transient *DecodeTransient(word p) {
    return (Transient *) ((((u_int) p & (u_int) TAGMASK) == 2) ? ((char *) p-2) : INVALID_POINTER);
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
    Assert((((u_int) v) & INTMASK) == 1);
    return (int) ((u_int) v >> 1);
  }
  // Deref Function
  static word Deref(word v) {
    while (true) {
      u_int vi  = (u_int) v;
 
      Assert(v != NULL);
      if ((vi & TAGMASK) == 2) {
	vi -= 2;
	if (HeaderOp::DecodeLabel((Tuple *) vi) == REF) {
	  v = (word) ((u_int *) vi)[1];
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
