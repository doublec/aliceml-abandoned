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

typedef unsigned int u_int;

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

static int StringToInt(char *v) {
  int i;

  sscanf(v, "%u", &i);
  return i;
}

static void CreateHeader(FILE *f, int width, int tag, int size, int generations,
			 u_int *MAX_TAGSIZE_PTR, u_int *GEN_SHIFT_PTR) {
  u_int GC_SHIFT     = 0;
  u_int TAG_SHIFT    = 1;
  u_int SIZE_SHIFT   = (TAG_SHIFT + tag);
  u_int INTGEN_SHIFT = (SIZE_SHIFT + size);
  u_int GEN_SHIFT    = (INTGEN_SHIFT + 1);
  u_int MAX_TAGSIZE  = ((1 << tag) - 1);
  u_int MAX_HBSIZE   = ((1 << size) - 1);
  u_int GC_MASK      = ComputeMask(width, 0, 1);
  u_int TAG_MASK     = ComputeMask(width, TAG_SHIFT, tag);
  u_int SIZE_MASK    = ComputeMask(width, SIZE_SHIFT, size);
  u_int INTGEN_MASK  = ComputeMask(width, INTGEN_SHIFT, 1);
  u_int GEN_MASK     = ComputeMask(width, GEN_SHIFT, generations);

  *MAX_TAGSIZE_PTR = MAX_TAGSIZE;
  *GEN_SHIFT_PTR   = GEN_SHIFT;

  fprintf(f, "typedef enum {\n");

  fprintf(f, "  GC_SHIFT     = 0x%x,\n", GC_SHIFT);
  fprintf(f, "  TAG_SHIFT    = 0x%x,\n", TAG_SHIFT);
  fprintf(f, "  SIZE_SHIFT   = 0x%x,\n", SIZE_SHIFT);
  fprintf(f, "  INTGEN_SHIFT = 0x%x,\n", INTGEN_SHIFT);
  fprintf(f, "  GEN_SHIFT    = 0x%x,\n", GEN_SHIFT);
  fprintf(f, "  MAX_TAGSIZE  = 0x%x,\n", MAX_TAGSIZE);
  fprintf(f, "  MAX_HBSIZE   = 0x%x,\n", MAX_HBSIZE);

  fprintf(f, "  GC_MASK      = 0x%x,\n", GC_MASK);
  fprintf(f, "  TAG_MASK     = 0x%x,\n", TAG_MASK);
  fprintf(f, "  SIZE_MASK    = 0x%x,\n", SIZE_MASK);
  fprintf(f, "  INTGEN_MASK  = 0x%x,\n", INTGEN_MASK);
  fprintf(f, "  GEN_MASK     = 0x%x\n", GEN_MASK);

  fprintf(f, "} HeaderHef;\n\n");
}

static void CreateLabel(FILE *f, int size) {
  u_int MIN_LSIZE = 0;
  u_int STACK     = (size - 6);
  u_int MAX_LSIZE = (size - 6);
  u_int CHUNK     = (size - 5);
  u_int HOLE      = (size - 4);
  u_int FUTURE    = (size - 3);
  u_int REF       = (size- 2);
  u_int CANCELLED = (size - 1);
  u_int BYNEED    = size;

  fprintf(f, "typedef enum {\n");

  fprintf(f, "  MIN_LSIZE = 0x%x,\n", MIN_LSIZE);
  fprintf(f, "  STACK     = 0x%x,\n", STACK);
  fprintf(f, "  MAX_LSIZE = 0x%x,\n", MAX_LSIZE);
  fprintf(f, "  CHUNK     = 0x%x,\n", CHUNK);
  fprintf(f, "  MIN_TRANSIENT = 0x%x,\n", HOLE);
  fprintf(f, "  HOLE      = 0x%x,\n", HOLE);
  fprintf(f, "  FUTURE    = 0x%x,\n", FUTURE);
  fprintf(f, "  REF       = 0x%x,\n", REF);
  fprintf(f, "  CANCELLED = 0x%x,\n", CANCELLED);
  fprintf(f, "  BYNEED    = 0x%x,\n", BYNEED);
  fprintf(f, "  MAX_TRANSIENT = 0x%x\n", BYNEED);

  fprintf(f, "} BlockLabel;\n\n");
}

static void CreateGenerationLimits(FILE *f, int pos, int generations) {
  fprintf(f, "#define PLACEGENERATIONLIMIT static u_int gen_limits[] = {");

  for (int i = 0; i < generations; i++) {
    unsigned int mask = (i << pos);
    
    for (int k = 0; k < (pos - 2); k++) {
      mask += (1 << k);
    }
    
    fprintf(f, " 0x%x", mask);
    if ((i + 1) < generations) {
      fprintf(f, ",");
    }
  }

  fprintf(f, " }\n\n");
}

int main(int argc, char **argv) {
  int checksum = 1; // INTGEN TAG
  int val[4]; // width, tag, size, generations
  u_int MAX_TAGSIZE, GEN_SHIFT;
  FILE *f;

  if (argc < 5) {
    fprintf(stderr, "usage: %s width tag size generations\n", argv[0]);
    exit(0);
  }

  for (int i = 0; i < 4; i++) {
    val[i] = StringToInt(argv[i + 1]);
  }

  for (int i = 1; i < 4; i++) {
    checksum += val[i];
  }

  if (checksum != val[0]) {
    fprintf(stderr, "%s: error: illegal width checksum. Aborting generation.\n", argv[0]);
    exit(0);
  }

  if ((f = fopen("headerdef.hh", "w")) == NULL) {
    fprintf(stderr, "%s: unable to open file headerdef.hh\n", argv[0]);
    exit(1);
  }
  
  fprintf(f, "//\n// This File is generated. Please do not edit.\n//\n");
  fprintf(f, "#ifndef __HEADERDEF_HH__\n");
  fprintf(f, "#define __HEADERDEF_HH__\n\n");

  CreateHeader(f, val[0], val[1], val[2], val[3], &MAX_TAGSIZE, &GEN_SHIFT);
  CreateLabel(f, MAX_TAGSIZE);
  CreateGenerationLimits(f, GEN_SHIFT, ((1 << val[3]) - 1));

  fprintf(f, "#endif\n");
  fclose(f);

  return 0;
}
