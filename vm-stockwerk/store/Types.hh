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

#define INVALID_POINTER   0
#define INVALID_INT       (1 << (STORE_WORD_WIDTH - 1))
#define MIN_VALID_INT     (-(1 << (STORE_WORD_WIDTH - 2)))
#define MAX_VALID_INT     ((1 << (STORE_WORD_WIDTH - 2)) - 1)
#define INVALID_BLOCKSIZE 0
#define INVALID_FIELD     0

// Word Datatype
typedef struct {
  unsigned int *dummy_entry;
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
class WeakDictionary;

#endif __STORE__TYPES_HH__
