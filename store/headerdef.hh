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
#ifndef __HEADERDEF_HH__
#define __HEADERDEF_HH__

#if defined(INTERFACE)
#pragma interface
#endif

typedef unsigned int t_label;

// KNOWN Block Labels
class BlockLabel {
public:
  static t_label MIN_LSIZE;
  static t_label STACK;
  static t_label MAX_LSIZE;
  static t_label CHUNK;
  static t_label PROMISE;
  static t_label FUTURE;
  static t_label REF;
  static t_label CANCELLED;
  static t_label BYNEED;

  static void CreateLabel(unsigned int size);
};

class HeaderDef {
public:
  // Header Representation
  static unsigned int GC_SHIFT;
  static unsigned int TAG_SHIFT;
  static unsigned int SIZE_SHIFT;
  static unsigned int GEN_SHIFT;
  static unsigned int MAX_TAGSIZE;
  static unsigned int MAX_HBSIZE;
  static unsigned int *GEN_LIMIT;
  // Header Decode Masks
  static unsigned int GC_MASK;
  static unsigned int TAG_MASK;
  static unsigned int SIZE_MASK;
  static unsigned int GEN_MASK;

  static void CreateHeader(int width, int tag, int size, int generations);
};

#endif
