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

//
// Helper Functions
//

static unsigned long ComputeMask(int pos, int width) {
  unsigned long mask = 0;

  for (long i = 0; i < width; i++) {
    mask += (1 << (pos + i));
  }

  return mask;
}

static void CreateHeader(FILE *f, unsigned int tag, unsigned int size, unsigned int generations,
			 unsigned long *MAX_TAGSIZE_PTR, unsigned long *GEN_SHIFT_PTR) {
  unsigned long GC_SHIFT      = 0;
  unsigned long HANDLER_SHIFT = 1;
  unsigned long TAG_SHIFT     = (HANDLER_SHIFT + 1);
  unsigned long SIZE_SHIFT    = (TAG_SHIFT + tag);
  unsigned long INTGEN_SHIFT  = (SIZE_SHIFT + size);
  unsigned long GEN_SHIFT     = (INTGEN_SHIFT + 1);
  unsigned long MAX_TAGSIZE   = ((1 << tag) - 1);
  unsigned long MAX_BLOCKSIZE = ((1 << size) - 1);
  unsigned long GC_MASK       = ComputeMask(GC_SHIFT, 1);
  unsigned long HANDLER_MASK  = ComputeMask(HANDLER_SHIFT, 1);
  unsigned long TAG_MASK      = ComputeMask(TAG_SHIFT, tag);
  unsigned long SIZE_MASK     = ComputeMask(SIZE_SHIFT, size);
  unsigned long INTGEN_MASK   = ComputeMask(INTGEN_SHIFT, 1);
  unsigned long GEN_MASK      = ComputeMask(GEN_SHIFT, generations);

  *MAX_TAGSIZE_PTR = MAX_TAGSIZE;
  *GEN_SHIFT_PTR   = GEN_SHIFT;

  std::fprintf(f, "typedef enum {\n");

  std::fprintf(f, "  GC_SHIFT      = 0x%lx,\n", GC_SHIFT);
  std::fprintf(f, "  HANDLER_SHIFT = 0x%lx,\n", HANDLER_SHIFT);
  std::fprintf(f, "  TAG_SHIFT     = 0x%lx,\n", TAG_SHIFT);
  std::fprintf(f, "  SIZE_SHIFT    = 0x%lx,\n", SIZE_SHIFT);
  std::fprintf(f, "  INTGEN_SHIFT  = 0x%lx,\n", INTGEN_SHIFT);
  std::fprintf(f, "  GEN_SHIFT     = 0x%lx,\n", GEN_SHIFT);
  std::fprintf(f, "  MAX_TAGSIZE   = 0x%lx,\n", MAX_TAGSIZE);
  std::fprintf(f, "  MAX_BLOCKSIZE = 0x%lx,\n", MAX_BLOCKSIZE);

  std::fprintf(f, "  GC_MASK       = 0x%lx,\n", GC_MASK);
  std::fprintf(f, "  HANDLER_MASK  = 0x%lx,\n", HANDLER_MASK);
  std::fprintf(f, "  TAG_MASK      = 0x%lx,\n", TAG_MASK);
  std::fprintf(f, "  SIZE_MASK     = 0x%lx,\n", SIZE_MASK);
  std::fprintf(f, "  INTGEN_MASK   = 0x%lx,\n", INTGEN_MASK);
  std::fprintf(f, "  GEN_MASK      = 0x%lx\n", GEN_MASK);

  std::fprintf(f, "} HeaderWord;\n\n");
}

static void CreateLabel(FILE *f, unsigned long size) {
  STORE_HELPER_LABEL_ARRAY;
  unsigned int arr_size = (sizeof(helper_arr) / sizeof(char *));

  std::fprintf(f, "typedef enum {\n");

  std::fprintf(f, "  MIN_DATA_LABEL      = 0x%lx,\n", (unsigned long) 0);
  std::fprintf(f, "  MAX_DATA_LABEL      = 0x%lx,\n", (size - 7 - arr_size));
  std::fprintf(f, "  MIN_HELPER_LABEL    = 0x%lx,\n\n", (size - 6 - arr_size));

  for (unsigned long i = 0; i < arr_size; i++) {
    std::fprintf(f, "  %s_LABEL = 0x%lx,\n", helper_arr[i], (size - 6 - (arr_size  - i)));
  }

  std::fprintf(f, "\n  MAX_HELPER_LABEL    = 0x%lx,\n", (size - 7));
  std::fprintf(f, "  MIN_STORE_LABEL     = 0x%lx,\n", (size - 6));
  std::fprintf(f, "  WEAK_DICT_LABEL     = 0x%lx,\n", (size - 6));
  std::fprintf(f, "  CHUNK_LABEL         = 0x%lx,\n", (size - 5));
  std::fprintf(f, "  HOLE_LABEL          = 0x%lx,\n", (size - 4));
  std::fprintf(f, "  MIN_TRANSIENT_LABEL = 0x%lx,\n", (size - 4)); 
  std::fprintf(f, "  FUTURE_LABEL        = 0x%lx,\n", (size - 3));
  std::fprintf(f, "  REF_LABEL           = 0x%lx,\n", (size - 2));
  std::fprintf(f, "  CANCELLED_LABEL     = 0x%lx,\n", (size - 1));
  std::fprintf(f, "  BYNEED_LABEL        = 0x%lx,\n", size);
  std::fprintf(f, "  MAX_TRANSIENT_LABEL = 0x%lx,\n", size);
  std::fprintf(f, "  MAX_STORE_LABEL     = 0x%lx\n", size);

  std::fprintf(f, "} BlockLabel;\n\n");
}

static void CreateGenerationLimits(FILE *f, unsigned long pos, unsigned long generations) {
  std::fprintf(f, "#define PLACEGENERATIONLIMIT \\\n  static u_int gen_limits[] = {");

  for (unsigned long i = 0; i < generations; i++) {
    unsigned long mask = ((i + 1) << pos);

    for (unsigned long k = 0; k < pos; k++) {
      mask += (1 << k);
    }
    
    std::fprintf(f, " 0x%lx", mask);
    if ((i + 1) < generations) {
      std::fprintf(f, ",");
    }
  }

  std::fprintf(f, " }\n\n");
}

int main(int argc, char **argv) {
  unsigned long MAX_TAGSIZE, GEN_SHIFT;
  const char *int_val;
  FILE *f;

  if (((sizeof(unsigned long) * 8) < HEADER_FULL_WIDTH)) {
    std::fprintf(stderr,
		 "%s: unable to operate: (sizeof(unsigned long) * 8) must fit in %d bits\n",
		 argv[0], HEADER_FULL_WIDTH);
    exit(1);
  }

  argc = argc; // Hack to remove unused argc warning

  if ((f = std::fopen("StoreConfig.hh", "w")) == NULL) {
    std::fprintf(stderr, "%s: unable to open file storeconfig.hh\n", argv[0]);
    exit(1);
  }
  
  std::fprintf(f, "//\n// This File is generated. Please do not edit.\n//\n");
  std::fprintf(f, "#ifndef __STORECONFIG_HH__\n");
  std::fprintf(f, "#define __STORECONFIG_HH__\n\n");

  if (HEADER_FULL_WIDTH != (HEADER_GC_MARK_WIDTH +
			    HEADER_HANDLER_MARK_WIDTH +
			    HEADER_TAG_WIDTH +
			    HEADER_SIZE_WIDTH +
			    HEADER_INTGEN_MARK_WIDTH +
			    HEADER_GENERATION_WIDTH)) {
    std::fprintf(stderr, "%s: illegal HEADER parameter selection. Field-Sum must match full width\n",
	    argv[0]);
    exit(1);
  }

  if (STORE_GENERATION_NUM > ((1 << HEADER_GENERATION_WIDTH) - 1)) {
    std::fprintf(stderr,
		 "%s: STORE_GENERATION_NUM must fit in HEADER_GENERATION_WIDTH starting at 1.\n",
		 argv[0]);
    exit(1);
  }

  unsigned int MIN_WIDTH = (HEADER_FULL_WIDTH >> 3);
  unsigned int PTR_WIDTH = sizeof(void *);

  if (PTR_WIDTH > MIN_WIDTH) {
    MIN_WIDTH = PTR_WIDTH;
  }

  if (MIN_WIDTH <= sizeof(short)) {
    MIN_WIDTH = sizeof(short);
    int_val   = "short";
  }
  else if (MIN_WIDTH >= sizeof(int)) {
    MIN_WIDTH = sizeof(int);
    int_val = "int";
  }
  else if (MIN_WIDTH >= sizeof(long)) {
    MIN_WIDTH = sizeof(long);
    int_val = "long";
  }
  // #if defined(__GNUC__)
  // else if (MIN_WIDTH >= sizeof(long long)) {
  //   MIN_WIDTH = sizeof(long long);
  //   int_val   = "long long";
  // }
  // #endif
  else {
    std::fprintf(stderr,
		 "%s: unsupported platform: unable to find appropriate int type\n", argv[0]);

    exit(1);
  }

  CreateHeader(f,
	       HEADER_TAG_WIDTH,
	       HEADER_SIZE_WIDTH + ((MIN_WIDTH * 8) - HEADER_FULL_WIDTH),
	       HEADER_GENERATION_WIDTH,
	       &MAX_TAGSIZE,
	       &GEN_SHIFT);
  CreateLabel(f, MAX_TAGSIZE);
  CreateGenerationLimits(f, GEN_SHIFT, STORE_GENERATION_NUM);

  std::fprintf(f, "#define STORE_GENERATION_NUM %d\n", STORE_GENERATION_NUM);
  std::fprintf(f, "#define STORE_MEMCHUNK_SIZE  %d\n", STORE_MEMCHUNK_SIZE);
  std::fprintf(f, "#define STORE_INTGENSET_SIZE %d\n", STORE_INITIAL_INTGEN);
  std::fprintf(f, "#define STORE_WKDICTSET_SIZE %d\n", STORE_INITIAL_WKDICT);
  std::fprintf(f, "#define STORE_WORD_WIDTH     %d\n\n", (MIN_WIDTH * 8));

  if (STORE_CHUNKTOP_IN_REG) {
    std::fprintf(f, "#define STORE_CHUNKTOP_IN_REG 1\n\n");
  }

  std::fprintf(f, "typedef %s s_int;\n", int_val);
  std::fprintf(f, "typedef unsigned %s u_int;\n\n", int_val);

  std::fprintf(f, "#endif\n");
  fclose(f);

  return 0;
}
