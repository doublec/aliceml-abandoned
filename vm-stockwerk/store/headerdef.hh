//
// This File is generated. Please do not edit.
//
#ifndef __HEADERDEF_HH__
#define __HEADERDEF_HH__

typedef enum {
  GC_SHIFT     = 0x0,
  TAG_SHIFT    = 0x1,
  SIZE_SHIFT   = 0x11,
  MAXOLD_SHIFT = 0x1c,
  GEN_SHIFT    = 0x1e,
  MAX_TAGSIZE  = 0xffff,
  MAX_HBSIZE   = 0x7ff,
  GC_MASK      = 0x1,
  TAG_MASK     = 0x1fffe,
  SIZE_MASK    = 0xffe0000,
  MAXOLD_MASK  = 0x30000000,
  GEN_MASK     = 0xc0000000
} HeaderHef;

typedef enum {
  MIN_LSIZE = 0x0,
  STACK     = 0xfff9,
  MAX_LSIZE = 0xfff9,
  CHUNK     = 0xfffa,
  PROMISE   = 0xfffb,
  FUTURE    = 0xfffc,
  REF       = 0xfffd,
  CANCELLED = 0xfffe,
  BYNEED    = 0xffff
} BlockLabel;

#define PLACEGENERATIONLIMIT static u_int gen_limits[] = { 0xfffffff, 0x4fffffff, 0x8fffffff }

#endif
