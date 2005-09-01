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
#ifndef __STORE__TYPES_HH__
#define __STORE__TYPES_HH__

#include "store/StoreConfig.hh"

#define INVALID_POINTER 0
#define INVALID_INT     STATIC_CAST(s_int, STATIC_CAST(u_int, 1) << (STORE_WORD_WIDTH - 1))
// We must define INT_PRECISION to be 31 such that pickles remain
// platform independent
//#define INT_PRECISION   (STORE_WORD_WIDTH - 1)
#define INT_PRECISION   31
#define MIN_VALID_INT   STATIC_CAST(s_int, -(STATIC_CAST(u_int, 1) << (INT_PRECISION - 1)))
#define MAX_VALID_INT   STATIC_CAST(s_int, (STATIC_CAST(u_int, 1) << (INT_PRECISION - 1)) - STATIC_CAST(u_int, 1))
#define MIN_BLOCKSIZE   0

#define SIZEOF_BLOCK(s) \
  (u_int) ((s + 1) * sizeof(u_int))

// Word Datatype
typedef struct {
  s_int *dummy_entry;
} word_s;
typedef word_s * word;

// Pointer Tags
typedef enum {
  EMPTYTAG = 0,
  BLKTAG   = 0,
  INTTAG   = 1,
  TRTAG    = 2
} PointerTag;

// Pointer Masks
typedef enum {
  INTMASK = 1,
  TAGMASK = 3
} PointerMask;

class Block;
class Transient;
class Chunk;
class WeakDictionary;
class DynamicBlock;

#endif
