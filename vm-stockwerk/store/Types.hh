#ifndef __types_hh__
#define __types_hh__

#include "headerdef.hh"

#define INVALID_POINTER NULL
#define INVALID_TSIZE   0
#define INVALID_FIELD   0

typedef unsigned int u_int;

// Word Datatype
typedef struct {
  unsigned int *dummy_entry;
} word_s;
typedef word_s * word;

// Tuple Labels (Block Tags)
typedef enum {
  TAG0      = 0,
  TAG1      = 1,
  TAG2      = 2,
  TAG3      = 3,
  TAG4      = 4,
  STACK     = HeaderDef::MAX_TAGSIZE - 6,
  MAX_LSIZE = HeaderDef::MAX_TAGSIZE - 6,
  CHUNK     = HeaderDef::MAX_TAGSIZE - 5,
  PROMISE   = HeaderDef::MAX_TAGSIZE - 4,
  FUTURE    = HeaderDef::MAX_TAGSIZE - 3,
  REF       = HeaderDef::MAX_TAGSIZE - 2,
  CANCELLED = HeaderDef::MAX_TAGSIZE - 1,
  BYNEED    = HeaderDef::MAX_TAGSIZE
} t_label;

// Pointer Tags
typedef enum {
  BLKTAG = 0,
  INTTAG = 1,
  TRTAG  = 2
} p_tags;

// Pointer Masks
typedef enum {
  INTMASK = 1,
  TAGMASK = 3
} p_mask;

class Block;
class Transient;
class Stack;

#endif
