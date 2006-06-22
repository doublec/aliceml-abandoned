//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Contributor:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000-2001
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "Parameter.hh"

union Endianness {
  unsigned char c[sizeof(double)];
  float f;
  double d;
};

enum floatEndianness { littleEndian, bigEndian, armEndian,
		       badSize, nonIEC };

static floatEndianness CheckFloatEndianness() {
  static unsigned char littleOne[4] =
    { 0x00, 0x00, 0x80, 0x3F };
  static unsigned char bigOne[4] =
    { 0x3F, 0x80, 0x00, 0x00 };
  Endianness x;
  x.f = 1.0;
  if (sizeof(float) != 4)
    return badSize;
  if (!std::memcmp(littleOne, x.c, 4))
    return littleEndian;
  if (!std::memcmp(bigOne, x.c, 4))
    return bigEndian;
  return nonIEC;
}

static floatEndianness CheckDoubleEndianness() {
  static unsigned char littleOne[8] =
    { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF0, 0x3F };
  static unsigned char bigOne[8] =
    { 0x3F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };
  static unsigned char armOne[8] = 
    { 0x00, 0x00, 0xF0, 0x3F, 0x00, 0x00, 0x00, 0x00 };
  Endianness x;
  x.d = 1.0;
  if (sizeof(double) != 8)
    return badSize;
  if (!std::memcmp(littleOne, x.c, 8))
    return littleEndian;
  if (!std::memcmp(bigOne, x.c, 8))
    return bigEndian;
  if (!std::memcmp(armOne, x.c, 8))
    return armEndian;
  return nonIEC;
}

//
// Helper Functions
//

static unsigned long ComputeMask(int pos, int width) {
  unsigned long mask = 0;

  for (unsigned long i = 0; i < width; i++) {
    mask += (1lu << (pos + i));
  }

  return mask;
}

static void CreateHeader(std::FILE *f, unsigned int header_size_width) {
  unsigned long GEN_GC_SHIFT     = 0;
  unsigned long SIZESHIFT_SHIFT  = (HEADER_GEN_GC_MARK_WIDTH);
  unsigned long SIZE_SHIFT       = (SIZESHIFT_SHIFT + HEADER_SIZESHIFT_WIDTH);
  unsigned long TAG_SHIFT        = (SIZE_SHIFT + header_size_width);
  unsigned long MUTABLE_SHIFT    = (TAG_SHIFT + HEADER_TAG_WIDTH);
  unsigned long CHILDISH_SHIFT   = (MUTABLE_SHIFT + HEADER_MUTABLE_WIDTH);
  unsigned long MAX_TAGSIZE      = ((1 << HEADER_TAG_WIDTH) - 1);
  unsigned long MAX_BLOCKSIZE    = ((1 << HEADER_SIZE_WIDTH) - 1); // to be determined
  unsigned long GEN_GC_MASK      = ComputeMask(GEN_GC_SHIFT, HEADER_GEN_GC_MARK_WIDTH);
  unsigned long SIZESHIFT_MASK   = ComputeMask(SIZESHIFT_SHIFT, HEADER_SIZESHIFT_WIDTH);
  unsigned long SIZE_MASK        = ComputeMask(SIZE_SHIFT, header_size_width);
  unsigned long TAG_MASK         = ComputeMask(TAG_SHIFT, HEADER_TAG_WIDTH);
  unsigned long MUTABLE_MASK     = ComputeMask(MUTABLE_SHIFT, HEADER_MUTABLE_WIDTH);
  unsigned long CHILDISH_MASK    = ComputeMask(CHILDISH_SHIFT, HEADER_CHILDISH_WIDTH);
  unsigned long BIGSIZE_MIN      = (1 << SIZESHIFT_MASK);
  unsigned long MAX_BIGBLOCKSIZE = ((MAX_BLOCKSIZE << SIZESHIFT_MASK)-1);

  std::fprintf(f, "typedef enum {\n");

  std::fprintf(f, "  GEN_GC_SHIFT     = 0x%lx,\n", GEN_GC_SHIFT);
  std::fprintf(f, "  SIZESHIFT_SHIFT  = 0x%lx,\n", SIZESHIFT_SHIFT);
  std::fprintf(f, "  SIZE_SHIFT       = 0x%lx,\n", SIZE_SHIFT);
  std::fprintf(f, "  TAG_SHIFT        = 0x%lx,\n", TAG_SHIFT);
  std::fprintf(f, "  MUTABLE_SHIFT    = 0x%lx,\n", MUTABLE_SHIFT);
  std::fprintf(f, "  CHILDISH_SHIFT   = 0x%lx,\n", CHILDISH_SHIFT);

  std::fprintf(f, "  MAX_TAGSIZE      = 0x%lx,\n", MAX_TAGSIZE);
  std::fprintf(f, "  MAX_BLOCKSIZE    = 0x%lx,\n", MAX_BLOCKSIZE);
  std::fprintf(f, "  MAX_BIGBLOCKSIZE = 0x%lx,\n", MAX_BIGBLOCKSIZE);
  std::fprintf(f, "  BIGSIZE_MIN      = 0x%lx,\n", BIGSIZE_MIN);

  std::fprintf(f, "  GEN_GC_MASK      = 0x%lx,\n", GEN_GC_MASK);
  std::fprintf(f, "  SIZESHIFT_MASK   = 0x%lx,\n", SIZESHIFT_MASK);
  std::fprintf(f, "  SIZE_MASK        = 0x%lx,\n", SIZE_MASK);
  std::fprintf(f, "  TAG_MASK         = 0x%lx,\n", TAG_MASK);
  std::fprintf(f, "  MUTABLE_MASK     = 0x%lx,\n", MUTABLE_MASK);
  std::fprintf(f, "  CHILDISH_MASK    = 0x%lx,\n", CHILDISH_MASK);

  std::fprintf(f, "} HeaderWord;\n\n");
}

static void CreateLabel(std::FILE *f, unsigned long size) {
  STORE_HELPER_LABEL_ARRAY;
  unsigned int arr_size = (sizeof(helper_arr) / sizeof(char *));

  std::fprintf(f, "typedef enum {\n");

  std::fprintf(f, "  MIN_DATA_LABEL      = 0x%lx,\n", (unsigned long) 0);
  std::fprintf(f, "  MAX_DATA_LABEL      = 0x%lx,\n", (size - 8 - arr_size));
  std::fprintf(f, "  MIN_HELPER_LABEL    = 0x%lx,\n\n", (size - 7 - arr_size));

  for (unsigned long i = 0; i < arr_size; i++) {
    std::fprintf(f, "  %s_LABEL = 0x%lx,\n", helper_arr[i], (size - 7 - (arr_size  - i)));
  }

  std::fprintf(f, "\n  MAX_HELPER_LABEL    = 0x%lx,\n", (size - 8));
  std::fprintf(f, "  MIN_STORE_LABEL     = 0x%lx,\n", (size - 7));
  std::fprintf(f, "  MIN_TRANSIENT_LABEL = 0x%lx,\n", (size - 7)); 
  std::fprintf(f, "  HOLE_LABEL          = 0x%lx,\n", (size - 7));
  std::fprintf(f, "  FUTURE_LABEL        = 0x%lx,\n", (size - 6));
  std::fprintf(f, "  REF_LABEL           = 0x%lx,\n", (size - 5));
  std::fprintf(f, "  CANCELLED_LABEL     = 0x%lx,\n", (size - 4));
  std::fprintf(f, "  BYNEED_LABEL        = 0x%lx,\n", (size - 3));
  std::fprintf(f, "  MAX_TRANSIENT_LABEL = 0x%lx,\n", (size - 3));
  std::fprintf(f, "  CHUNK_LABEL         = 0x%lx,\n", (size - 2));
  std::fprintf(f, "  WEAK_MAP_LABEL      = 0x%lx,\n", (size - 1));
  std::fprintf(f, "  DYNAMIC_LABEL       = 0x%lx,\n", size);
  std::fprintf(f, "  MAX_STORE_LABEL     = 0x%lx\n", size);

  std::fprintf(f, "} BlockLabel;\n\n");
}

int main(int argc, char **argv) {
  const char *int_val = NULL;
  std::FILE *f;

  if (((sizeof(unsigned long) * 8) < HEADER_FULL_WIDTH)) {
    std::fprintf(stderr,
		 "%s: unable to operate: (sizeof(unsigned long) * 8) must fit in %d bits\n",
		 argv[0], HEADER_FULL_WIDTH);
    std::exit(1);
  }

  argc = argc; // Hack to remove unused argc warning

  if ((f = std::fopen("StoreConfig.hh", "w")) == NULL) {
    std::fprintf(stderr, "%s: unable to open file StoreConfig.hh\n", argv[0]);
    std::exit(1);
  }
  
  std::fprintf(f, "//\n// This File is generated. Please do not edit.\n//\n");
  std::fprintf(f, "#ifndef __STORECONFIG_HH__\n");
  std::fprintf(f, "#define __STORECONFIG_HH__\n\n");

  if (HEADER_FULL_WIDTH != (HEADER_GEN_GC_MARK_WIDTH +
			    HEADER_SIZESHIFT_WIDTH +
			    HEADER_SIZE_WIDTH +
			    HEADER_TAG_WIDTH +
			    HEADER_MUTABLE_WIDTH +
			    HEADER_CHILDISH_WIDTH)) {
    std::fprintf(stderr, "%s: illegal HEADER parameter selection. Field-Sum must match full width\n",
	    argv[0]);
    std::exit(1);
  }

  if (STORE_GENERATION_NUM > ((1 << HEADER_GEN_GC_MARK_WIDTH) - 1)) {
    std::fprintf(stderr,
		 "%s: STORE_GENERATION_NUM must fit in HEADER_GENERATION_WIDTH starting at 1.\n",
		 argv[0]);
    std::exit(1);
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
  else if (MIN_WIDTH <= sizeof(int)) {
    MIN_WIDTH = sizeof(int);
    int_val = "int";
  }
  else if (MIN_WIDTH <= sizeof(long)) {
    MIN_WIDTH = sizeof(long);
    int_val = "long";
  }
  // #if defined(__GNUC__)
  // else if (MIN_WIDTH <= sizeof(long long)) {
  //   MIN_WIDTH = sizeof(long long);
  //   int_val   = "long long";
  // }
  // #endif
  else {
    std::fprintf(stderr,
		 "%s: unsupported platform: unable to find appropriate int type\n", argv[0]);

    std::exit(1);
  }

  if (STORE_MEMCHUNK_SIZE % MIN_WIDTH != 0) {
    std::fprintf(stderr,
		 "%s: STORE_MEMCHUNK_SIZE must be multiple of MIN_WIDTH=%d\n", argv[0], MIN_WIDTH);
    std::exit(1);
  }

  CreateHeader(f, HEADER_SIZE_WIDTH + ((MIN_WIDTH * 8) - HEADER_FULL_WIDTH));
  CreateLabel(f, ((1 << HEADER_TAG_WIDTH) - 1));
  
  // add shadow generation
  std::fprintf(f, "#define STORE_GENERATION_NUM %d\n", (STORE_GENERATION_NUM + 1));
  std::fprintf(f, "#define STORE_GEN_YOUNGEST   %d\n", 0);
  if (STORE_GENERATION_NUM > 1) {
    std::fprintf(f, "#define STORE_GEN_OLDEST     %d\n", (STORE_GENERATION_NUM - 1));
  }
  std::fprintf(f, "#define STORE_MEM_ALIGN      %d\n", MIN_WIDTH);
  std::fprintf(f, "#define STORE_MEMCHUNK_SIZE  %d\n", STORE_MEMCHUNK_SIZE);
  std::fprintf(f, "#define STORE_INTGENSET_SIZE %d\n", STORE_INITIAL_INTGEN);
  std::fprintf(f, "#define STORE_WKDICTSET_SIZE %d\n", STORE_INITIAL_WKDICT);
  std::fprintf(f, "#define STORE_WORD_WIDTH     %d\n\n", (MIN_WIDTH * 8));

  std::fprintf(f, "typedef %s s_int;\n", int_val);
  std::fprintf(f, "#ifndef HAVE_U_INT\n");
  std::fprintf(f, "#define u_int unsigned %s\n", int_val);
  std::fprintf(f, "#define u_char unsigned char\n\n");
  std::fprintf(f, "#else\n");
  std::fprintf(f, "#include <sys/types.h>\n");
  std::fprintf(f, "#endif\n");

  switch (CheckFloatEndianness()) {
  case bigEndian:
    std::fprintf(f, "#define FLOAT_BIG_ENDIAN 1\n");
    std::fprintf(f, "#define FLOAT_LITTLE_ENDIAN 0\n\n");
    break;
  case littleEndian:
    std::fprintf(f, "#define FLOAT_BIG_ENDIAN 0\n");
    std::fprintf(f, "#define FLOAT_LITTLE_ENDIAN 1\n\n");
    break;
  case armEndian:
    // cannot occur
    break;
  case badSize:
    std::fprintf(stderr, "%s: `float' type is not 4 bytes\n", argv[0]);
    std::exit(1);
  case nonIEC:
    std::fprintf(stderr, "%s: `float' type is not IEC 60559 conformant\n",
		 argv[0]);
    std::exit(1);
  }

  switch (CheckDoubleEndianness()) {
  case bigEndian:
    std::fprintf(f, "#define DOUBLE_BIG_ENDIAN 1\n");
    std::fprintf(f, "#define DOUBLE_LITTLE_ENDIAN 0\n\n");
    std::fprintf(f, "#define DOUBLE_ARM_ENDIAN 0\n\n");
    break;
  case littleEndian:
    std::fprintf(f, "#define DOUBLE_BIG_ENDIAN 0\n");
    std::fprintf(f, "#define DOUBLE_LITTLE_ENDIAN 1\n\n");
    std::fprintf(f, "#define DOUBLE_ARM_ENDIAN 0\n\n");
    break;
  case armEndian:
    std::fprintf(f, "#define DOUBLE_BIG_ENDIAN 0\n");
    std::fprintf(f, "#define DOUBLE_LITTLE_ENDIAN 0\n\n");
    std::fprintf(f, "#define DOUBLE_ARM_ENDIAN 1\n\n");
    break;
  case badSize:
    std::fprintf(stderr, "%s: `double' type is not 8 bytes\n", argv[0]);
    std::exit(1);
  case nonIEC:
    std::fprintf(stderr, "%s: `double' type is not IEC 60559 conformant\n",
		 argv[0]);
    std::exit(1);
  }

  std::fprintf(f, "#endif\n");
  std::fclose(f);

  return 0;
}
