#ifndef __TYPES_HH__
#define __TYPES_HH__

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
