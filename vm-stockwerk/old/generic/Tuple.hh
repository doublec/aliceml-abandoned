//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__TUPLE_HH__
#define __GENERIC__TUPLE_HH__

#if defined(INTERFACE)
#pragma interface "generic/Tuple.hh"
#endif

#include "store/Store.hh"

class Tuple: private Block {
public:
  using Block::ToWord;

  static Tuple *New(u_int n) {
    return static_cast<Tuple *>(Store::AllocBlock(TUPLE_LABEL, n));
  }
  static Tuple *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == TUPLE_LABEL);
    return static_cast<Tuple *>(b);
  }
  static Tuple *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == TUPLE_LABEL);
    return static_cast<Tuple *>(b);
  }

  u_int GetWidth() {
    return GetSize();
  }
  void Init(u_int index, word value) {
    InitArg(index + 1, value);
  }
  word Sel(u_int index) {
    return GetArg(index + 1);
  }
};

#endif __GENERIC__TUPLE_HH__
