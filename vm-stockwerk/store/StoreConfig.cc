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
#include <iostream.h>
#include <cstdio>
#include <cstdlib>

#if defined(INTERFACE)
#pragma implementation "headerdef.hh"
#endif
#include "headerdef.hh"

#include "store.hh"
//
// Helper Functions
//
static unsigned int ComputeMask(int max, int pos, int width) {
  unsigned int mask = 0;

  for (int i = 0; i < width; i++) {
    mask += (1 << (pos + i));
  }

  return mask;
}

static unsigned int *CreateGenerationLimits(int pos, int generations) {
  unsigned int *ar = (unsigned int *) std::malloc(sizeof(unsigned int) * generations);

  for (int i = 0; i < generations; i++) {
    unsigned int mask = (i << pos);

    for (int k = 0; k < (pos - 2); k++) {
      mask += (1 << k);
    }

    ar[i] = mask;
  }

  return ar;
}
//
// Class Fields
//
t_label BlockLabel::MIN_LSIZE = 0;
t_label BlockLabel::STACK     = 0;
t_label BlockLabel::MAX_LSIZE = 0;
t_label BlockLabel::CHUNK     = 0;
t_label BlockLabel::PROMISE   = 0;
t_label BlockLabel::FUTURE    = 0;
t_label BlockLabel::REF       = 0;
t_label BlockLabel::CANCELLED = 0;
t_label BlockLabel::BYNEED    = 0;

unsigned int HeaderDef::GC_SHIFT    = 0;
unsigned int HeaderDef::TAG_SHIFT   = 0;
unsigned int HeaderDef::SIZE_SHIFT  = 0;
unsigned int HeaderDef::GEN_SHIFT   = 0;

unsigned int HeaderDef::MAX_TAGSIZE = 0;
unsigned int HeaderDef::MAX_HBSIZE  = 0;

unsigned int *HeaderDef::GEN_LIMIT  = NULL;

unsigned int HeaderDef::GC_MASK   = 0;
unsigned int HeaderDef::TAG_MASK  = 0;
unsigned int HeaderDef::SIZE_MASK = 0;
unsigned int HeaderDef::GEN_MASK  = 0;
//
// Method Implementations
//
void BlockLabel::CreateLabel(unsigned int size) {
  MIN_LSIZE = 0;
  STACK     = (size - 6);
  MAX_LSIZE = (size - 6);
  CHUNK     = (size - 5);
  PROMISE   = (size - 4);
  FUTURE    = (size - 3);
  REF       = (size - 2);
  CANCELLED = (size - 1);
  BYNEED    = size;
}

void HeaderDef::CreateHeader(int width, int tag, int size, int generations) {
  Assert((tag + size + generations) <= width);

  GC_SHIFT    = 0;
  TAG_SHIFT   = 1;
  SIZE_SHIFT  = (TAG_SHIFT + tag);
  GEN_SHIFT   = (SIZE_SHIFT + size);
  MAX_TAGSIZE = ((1 << tag) - 1);
  MAX_HBSIZE  = ((1 << size) - 1);

  GEN_LIMIT   = CreateGenerationLimits(GEN_SHIFT, ((1 << generations) - 1));
  
  GC_MASK     = ComputeMask(width, 0, 1);
  TAG_MASK    = ComputeMask(width, TAG_SHIFT, tag);
  SIZE_MASK   = ComputeMask(width, SIZE_SHIFT, size);
  GEN_MASK    = ComputeMask(width, GEN_SHIFT, generations);

  BlockLabel::CreateLabel(MAX_TAGSIZE);
}
