#ifndef __headerop_hh__
#define __headerop_hh__

#include "base.hh"
#include "headerdef.hh"

class HeaderOp : private HeaderDef {
public:
  // Header Creation and Acess
  static void EncodeHeader(Tuple *t, t_label l, t_size s, u_int g) {
    ((u_int *) t)[0] =  0x00 | (((u_int) s) << SIZE_SHIFT) |
      (((u_int) l) << TAG_SHIFT) | (g << GEN_SHIFT);
  }
  static u_int GetHeader(Tuple *p) {
    return *((u_int *) p);
  }
  // Label Access
  static void EncodeLabel(Transient *p, t_label l) {
    ((u_int *) p)[0] = ((((u_int *) p)[0] & ~TAG_MASK) | (((u_int) l) << TAG_SHIFT));
  }
  static t_label DecodeLabel(Tuple *p) {
    return (t_label) ((((u_int *) p)[0] & TAG_MASK) >> TAG_SHIFT);
  }
  // Size Access
  static t_size BlankDecodeSize(Tuple *p) {
    return (t_size) ((((u_int *) p)[0] & SIZE_MASK) >> SIZE_SHIFT);
  }
  static t_size DecodeSize(Tuple *p) {
    t_size s = BlankDecodeSize(p);
    return (t_size) ((s < MAX_HBSIZE) ? s : (*((u_int *) ((char *) p - 4)) >> 1));
  }
};

#endif
