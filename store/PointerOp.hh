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
#ifndef __STORE__POINTEROP_HH__
#define __STORE__POINTEROP_HH__

#if defined(INTERFACE)
#pragma interface "store/PointerOp.hh"
#endif

class SeamMemberDll PointerOp {
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
    AssertStore(((u_int) p & (u_int) TAGMASK) == (u_int) EMPTYTAG);
    return (word) ((u_int) p | (u_int) BLKTAG);
  }
  static Block *DirectDecodeBlock(word p) {
    return (Block *) ((char *) p - (u_int) BLKTAG);
  }
  static Block *DecodeBlock(word p) {
    if (((u_int) p & (u_int) TAGMASK) == (u_int) BLKTAG)
      return DirectDecodeBlock(p);
    else
      return (Block *) INVALID_POINTER;
  }
  static Chunk *DirectDecodeChunk(word p) {
    return (Chunk *) ((char *) p - (u_int) BLKTAG);
  }
  static Chunk *DecodeChunk(word p) {
    if (((u_int) p & (u_int) TAGMASK) == (u_int) BLKTAG)
      return DirectDecodeChunk(p);
    else
      return (Chunk *) INVALID_POINTER;
  }
  // Transient<->Word Conversion
  static word EncodeTransient(Transient *p) {
    AssertStore(((u_int) p & (u_int) TAGMASK) == (u_int) EMPTYTAG);
    return (word) ((u_int) p | (u_int) TRTAG);
  }
  static Transient *DirectDecodeTransient(word p) {
    return (Transient *) ((char *) p - (u_int) TRTAG);
  }
  static Transient *DecodeTransient(word p) {
    if (((u_int) p & (u_int) TAGMASK) == (u_int) TRTAG)
      return DirectDecodeTransient(p);
    else
      return (Transient *) INVALID_POINTER;
  }
  // int Test
  static bool IsInt(word v) {
    return ((u_int) v & (u_int) INTMASK);
  }
  static bool IsTransient(word v) {
    return (((u_int) v & TAGMASK) == TRTAG);
  }
  // int<->Word Conversion
  static word EncodeInt(s_int v) {
    AssertStore(v >= MIN_VALID_INT);
    AssertStore(v <= MAX_VALID_INT);
    return (word) ((((u_int) v) << 1) | (u_int) INTTAG);
  }
  static s_int DirectDecodeInt(word v) {
    return (s_int) (((u_int) v & (1u << (STORE_WORD_WIDTH - 1))) ?
		    ((1u << (STORE_WORD_WIDTH - 1)) | ((u_int) v >> 1)) :
		    ((u_int) v >> 1));
  }
  static s_int DecodeInt(word v) {
    if ((((u_int) v) & INTMASK) == INTTAG)
      return DirectDecodeInt(v);
    else
      return INVALID_INT;
  }
  static word EncodeUnmanagedPointer(void *v) {
    AssertStore(((u_int) v & INTMASK) == 0); // Require at least word alignment
    return (word) ((u_int) v | (u_int) INTTAG); 
  }
  static void *DirectDecodeUnmanagedPointer(word v) {
    return (void *) ((u_int) v ^ INTTAG);
  }
  static void *DecodeUnmanagedPointer(word v) {
    AssertStore((u_int) v & INTMASK);
    return DirectDecodeUnmanagedPointer(v);
  }
  // Deref Function
  static word Deref(word v) {
  loop:
    u_int vi = (u_int) v;

    if (!((vi ^ TRTAG) & TAGMASK)) {
      vi ^= (u_int) TRTAG;
      
      if (HeaderOp::DecodeLabel((Block *) vi) == REF_LABEL) {
	v = ((word *) vi)[1];
	goto loop;
      }
    }

    return v;
  }
};

#endif
