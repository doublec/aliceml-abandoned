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
    return reinterpret_cast<word>(reinterpret_cast<u_int>(p) | tag);
  }
  static u_int DecodeTag(word p) {
    return reinterpret_cast<u_int>(p) & static_cast<u_int>(TAGMASK);
  }
  static Block *RemoveTag(word p) {
    return reinterpret_cast<Block *>(reinterpret_cast<u_int>(p) & ~static_cast<u_int>(TAGMASK));
  }
  // Block<->Word Conversion
  static word EncodeBlock(Block *p) {
    AssertStore((reinterpret_cast<u_int>(p) & static_cast<u_int>(TAGMASK)) == static_cast<u_int>(EMPTYTAG));
    return reinterpret_cast<word>(reinterpret_cast<u_int>(p) | static_cast<u_int>(BLKTAG));
  }
  static Block *DirectDecodeBlock(word p) {
    AssertStore((reinterpret_cast<u_int>(p) & static_cast<u_int>(TAGMASK)) == static_cast<u_int>(BLKTAG));
    return reinterpret_cast<Block *>(reinterpret_cast<char *>(p) - static_cast<u_int>(BLKTAG));
  }
  static Block *DecodeBlock(word p) {
    if ((reinterpret_cast<u_int>(p) & static_cast<u_int>(TAGMASK)) == static_cast<u_int>(BLKTAG))
      return DirectDecodeBlock(p);
    else
      return static_cast<Block *>(INVALID_POINTER);
  }
  static Chunk *DirectDecodeChunk(word p) {
    AssertStore((reinterpret_cast<u_int>(p) & static_cast<u_int>(TAGMASK)) == static_cast<u_int>(BLKTAG));
    return reinterpret_cast<Chunk *>(reinterpret_cast<char *>(p) - static_cast<u_int>(BLKTAG));
  }
  static Chunk *DecodeChunk(word p) {
    if ((reinterpret_cast<u_int>(p) & static_cast<u_int>(TAGMASK)) == static_cast<u_int>(BLKTAG))
      return DirectDecodeChunk(p);
    else
      return static_cast<Chunk *>(INVALID_POINTER);
  }
  // Transient<->Word Conversion
  static word EncodeTransient(Transient *p) {
    AssertStore((reinterpret_cast<u_int>(p) & static_cast<u_int>(TAGMASK)) == static_cast<u_int>(EMPTYTAG));
    return reinterpret_cast<word>(reinterpret_cast<u_int>(p) | static_cast<u_int>(TRTAG));
  }
  static Transient *DirectDecodeTransient(word p) {
    AssertStore((reinterpret_cast<u_int>(p) & static_cast<u_int>(TAGMASK)) == static_cast<u_int>(TRTAG));
    return reinterpret_cast<Transient *>(reinterpret_cast<char *>(p) - static_cast<u_int>(TRTAG));
  }
  static Transient *DecodeTransient(word p) {
    if ((reinterpret_cast<u_int>(p) & static_cast<u_int>(TAGMASK)) == static_cast<u_int>(TRTAG))
      return DirectDecodeTransient(p);
    else
      return static_cast<Transient *>(INVALID_POINTER);
  }
  // int Test
  static bool IsInt(word v) {
    return reinterpret_cast<u_int>(v) & static_cast<u_int>(INTMASK);
  }
  static bool IsTransient(word v) {
    return (reinterpret_cast<u_int>(v) & TAGMASK) == TRTAG;
  }
  // int<->Word Conversion
  static word EncodeInt(s_int v) {
    AssertStore(v >= MIN_VALID_INT);
    AssertStore(v <= MAX_VALID_INT);
    return reinterpret_cast<word>((static_cast<u_int>(v) << 1) | static_cast<u_int>(INTTAG));
  }
  static s_int DirectDecodeInt(word v) {
    AssertStore((reinterpret_cast<u_int>(v) & INTMASK) == INTTAG);
    s_int i = static_cast<s_int>
    		((reinterpret_cast<u_int>(v) & (static_cast<u_int>(1) << (STORE_WORD_WIDTH - 1))) ?
		    ((static_cast<u_int>(1) << (STORE_WORD_WIDTH - 1)) | (reinterpret_cast<u_int>(v) >> 1)) :
		    (reinterpret_cast<u_int>(v) >> 1));
    AssertStore(i >= MIN_VALID_INT && i <= MAX_VALID_INT);
    return i;
  }
  static s_int DecodeInt(word v) {
    if ((reinterpret_cast<u_int>(v) & INTMASK) == INTTAG)
      return DirectDecodeInt(v);
    else
      return INVALID_INT;
  }
  static word EncodeUnmanagedPointer(void *v) {
    AssertStore((reinterpret_cast<u_int>(v) & INTMASK) == 0); // Require at least word alignment
    return reinterpret_cast<word>(reinterpret_cast<u_int>(v) | static_cast<u_int>(INTTAG));
  }
  static void *DirectDecodeUnmanagedPointer(word v) {
    AssertStore(reinterpret_cast<u_int>(v) & INTMASK);
    return reinterpret_cast<void *>(reinterpret_cast<u_int>(v) ^ INTTAG);
  }
  static void *DecodeUnmanagedPointer(word v) {
    return DirectDecodeUnmanagedPointer(v);
  }
  // Deref Function
  static word Deref(word v) {
  loop:
    u_int vi = reinterpret_cast<u_int>(v);

    if (!((vi ^ TRTAG) & TAGMASK)) {
      vi ^= static_cast<u_int>(TRTAG);
      
      if (HeaderOp::DecodeLabel(reinterpret_cast<Block *>(vi)) == REF_LABEL) {
        v = reinterpret_cast<word *>(vi)[1];
        goto loop;
      }
    }

    return v;
  }
};

#endif
