#ifndef __headerdef_hh__
#define __headerdef_hh__

class HeaderDef {
public:
  // Header Representation
  static const unsigned int GC_SHIFT    = 0;
  static const unsigned int TAG_SHIFT   = 1;
  static const unsigned int SIZE_SHIFT  = 17;
  static const unsigned int GEN_SHIFT   = 30;
  static const unsigned int MAX_TAGSIZE = ((1 << 16) - 1);
  static const unsigned int MAX_HBSIZE  = ((1 << 13) - 1);
  static const unsigned int GEN_LIMIT[];
  // Header Decode Masks
  static const unsigned int GC_MASK     = 0x00000001;
  static const unsigned int TAG_MASK    = 0x0001FFFE;
  static const unsigned int SIZE_MASK   = 0x3FFE0000;
  static const unsigned int GEN_MASK    = 0xC0000000;
};

#endif
