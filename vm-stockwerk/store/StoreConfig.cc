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
#include <cstdio>
#include <cstdlib>

#include "Parameter.hh"

typedef unsigned int u_int;

//
// Helper Functions
//

static unsigned int ComputeMask(int pos, int width) {
  unsigned int mask = 0;

  for (int i = 0; i < width; i++) {
    mask += (1 << (pos + i));
  }

  return mask;
}

static void CreateHeader(FILE *f, u_int tag, u_int size, u_int generations,
			 u_int *MAX_TAGSIZE_PTR, u_int *GEN_SHIFT_PTR) {
  u_int GC_SHIFT      = 0;
  u_int TAG_SHIFT     = 1;
  u_int SIZE_SHIFT    = (TAG_SHIFT + tag);
  u_int INTGEN_SHIFT  = (SIZE_SHIFT + size);
  u_int GEN_SHIFT     = (INTGEN_SHIFT + 1);
  u_int MAX_TAGSIZE   = ((1 << tag) - 1);
  u_int MAX_BLOCKSIZE = ((1 << size) - 1);
  u_int GC_MASK       = ComputeMask(0, 1);
  u_int TAG_MASK      = ComputeMask(TAG_SHIFT, tag);
  u_int SIZE_MASK     = ComputeMask(SIZE_SHIFT, size);
  u_int INTGEN_MASK   = ComputeMask(INTGEN_SHIFT, 1);
  u_int GEN_MASK      = ComputeMask(GEN_SHIFT, generations);

  *MAX_TAGSIZE_PTR = MAX_TAGSIZE;
  *GEN_SHIFT_PTR   = GEN_SHIFT;

  std::fprintf(f, "typedef enum {\n");

  std::fprintf(f, "  GC_SHIFT      = 0x%x,\n", GC_SHIFT);
  std::fprintf(f, "  TAG_SHIFT     = 0x%x,\n", TAG_SHIFT);
  std::fprintf(f, "  SIZE_SHIFT    = 0x%x,\n", SIZE_SHIFT);
  std::fprintf(f, "  INTGEN_SHIFT  = 0x%x,\n", INTGEN_SHIFT);
  std::fprintf(f, "  GEN_SHIFT     = 0x%x,\n", GEN_SHIFT);
  std::fprintf(f, "  MAX_TAGSIZE   = 0x%x,\n", MAX_TAGSIZE);
  std::fprintf(f, "  MAX_BLOCKSIZE = 0x%x,\n", MAX_BLOCKSIZE);

  std::fprintf(f, "  GC_MASK       = 0x%x,\n", GC_MASK);
  std::fprintf(f, "  TAG_MASK      = 0x%x,\n", TAG_MASK);
  std::fprintf(f, "  SIZE_MASK     = 0x%x,\n", SIZE_MASK);
  std::fprintf(f, "  INTGEN_MASK   = 0x%x,\n", INTGEN_MASK);
  std::fprintf(f, "  GEN_MASK      = 0x%x\n", GEN_MASK);

  std::fprintf(f, "} HeaderWord;\n\n");
}

static void CreateLabel(FILE *f, int size) {
  STORE_HELPER_LABEL_ARRAY;
  u_int arr_size = (sizeof(helper_arr) / sizeof(char *));

  std::fprintf(f, "typedef enum {\n");

  std::fprintf(f, "  MIN_DATA_LABEL      = 0x%x,\n", 0);
  std::fprintf(f, "  MAX_DATA_LABEL      = 0x%x,\n", (size - 6 - arr_size));
  std::fprintf(f, "  MIN_HELPER_LABEL    = 0x%x,\n\n", (size - 5 - arr_size));

  for (u_int i = 0; i < arr_size; i++) {
    std::fprintf(f, "  %s_LABEL = 0x%x,\n", helper_arr[i], (size - 5 - (arr_size  - i)));
  }

  std::fprintf(f, "\n  MAX_HELPER_LABEL    = 0x%x,\n", (size - 6));
  std::fprintf(f, "  MIN_STORE_LABEL     = 0x%x,\n", (size - 5));
  std::fprintf(f, "  CHUNK_LABEL         = 0x%x,\n", (size - 5));
  std::fprintf(f, "  HOLE_LABEL          = 0x%x,\n", (size - 4));
  std::fprintf(f, "  MIN_TRANSIENT_LABEL = 0x%x,\n", (size - 4)); 
  std::fprintf(f, "  FUTURE_LABEL        = 0x%x,\n", (size - 3));
  std::fprintf(f, "  REF_LABEL           = 0x%x,\n", (size - 2));
  std::fprintf(f, "  CANCELLED_LABEL     = 0x%x,\n", (size - 1));
  std::fprintf(f, "  BYNEED_LABEL        = 0x%x,\n", size);
  std::fprintf(f, "  MAX_TRANSIENT_LABEL = 0x%x,\n", size);
  std::fprintf(f, "  MAX_STORE_LABEL     = 0x%x\n", size);

  std::fprintf(f, "} BlockLabel;\n\n");
}

static void CreateGenerationLimits(FILE *f, int pos, int generations) {
  std::fprintf(f, "#define PLACEGENERATIONLIMIT \\\n  static u_int gen_limits[] = {");

  for (int i = 0; i < generations; i++) {
    unsigned int mask = (i << pos);
    
    for (int k = 0; k < pos; k++) {
      mask += (1 << k);
    }
    
    std::fprintf(f, " 0x%x", mask);
    if ((i + 1) <= generations) {
      std::fprintf(f, ",");
    }
  }

  std::fprintf(f, " }\n\n");
}

int main(int argc, char **argv) {
  u_int MAX_TAGSIZE, GEN_SHIFT;
  FILE *f;

  argc = argc;

  if ((f = fopen("StoreConfig.hh", "w")) == NULL) {
    std::fprintf(stderr, "%s: unable to open file storeconfig.hh\n", argv[0]);
    exit(1);
  }
  
  std::fprintf(f, "//\n// This File is generated. Please do not edit.\n//\n");
  std::fprintf(f, "#ifndef __STORECONFIG_HH__\n");
  std::fprintf(f, "#define __STORECONFIG_HH__\n\n");

  if (HEADER_FULL_WIDTH != (HEADER_GC_MARK_WIDTH +
			    HEADER_TAG_WIDTH +
			    HEADER_SIZE_WIDTH +
			    HEADER_INTGEN_MARK_WIDTH +
			    HEADER_GENERATION_WIDTH)) {
    std::fprintf(stderr, "%s: illegal HEADER parameter selection. Field-Sum must match full width\n",
	    argv[0]);
    exit(1);
  }

  if (STORE_GENERATION_NUM > ((1 << HEADER_GENERATION_WIDTH) - 1)) {
    std::fprintf(stderr, "%s: STORE_GENERATION_NUM must fit in HEADER_GENERATION_WIDTH starting at one.\n", argv[0]);
    exit(1);
  }

  CreateHeader(f,
	       HEADER_TAG_WIDTH,
	       HEADER_SIZE_WIDTH,
	       HEADER_GENERATION_WIDTH,
	       &MAX_TAGSIZE,
	       &GEN_SHIFT);
  CreateLabel(f, MAX_TAGSIZE);
  CreateGenerationLimits(f, GEN_SHIFT, STORE_GENERATION_NUM);

  std::fprintf(f, "#define STORE_GENERATION_NUM %d\n", STORE_GENERATION_NUM);
  std::fprintf(f, "#define STORE_MEMCHUNK_SIZE  %d\n", STORE_MEMCHUNK_SIZE);
  std::fprintf(f, "#define STORE_INTGEN_SIZE    %d\n", STORE_INITIAL_INTGEN);
  std::fprintf(f, "#define STORE_WORD_WIDTH     %d\n\n", HEADER_FULL_WIDTH);

  if (STORE_CHUNKTOP_IN_REG) {
    std::fprintf(f, "#define STORE_CHUNKTOP_IN_REG 1\n\n");
  }

  std::fprintf(f, "#endif\n");
  fclose(f);

  return 0;
}
