#ifndef __types_hh__
#define __types_hh__

#include "headerdef.hh"

#define INVALID_POINTER NULL

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
  CHUNK     = 5,
  PROMISE   = 6,
  FUTURE    = 7,
  REF       = 8,
  CANCELLED = 9,
  BYNEED    = 10,
  MAX_LSIZE = HeaderDef::MAX_TAGSIZE
} t_label;

// Tuple Field Datatype
typedef enum {
  INVALID_FIELD = 0
} t_field;

// Tuple Size Datatype
typedef enum {
  INVALID_TSIZE = 0,
  TRANS_SIZE    = 1,
} t_size;

// Ptr Tags
typedef enum {
  TUPTAG = 0,
  INTTAG = 1,
  TRTAG  = 2
} tags;

// Ptr Masks
typedef enum {
  INTMASK = 1,
  TAGMASK = 3
} mask;

class Tuple;
class Transient;

#endif
