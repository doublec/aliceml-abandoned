//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000-2002
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

class SeamDll Tuple: private Block {
public:
  using Block::ToWord;

  // Tuple Constructor
  static Tuple *New(u_int n) {
    return static_cast<Tuple *>(Store::AllocBlock(TUPLE_LABEL, n));
  }
  // Tuple Untagging
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

  // Tuple Accessors
  void AssertWidth(u_int n) {
    Assert(Store::SizeToBlockSize(n) == GetSize()); n = n;
  }
  void Init(u_int index, word value) {
    InitArg(index, value);
  }
  word Sel(u_int index) {
    return GetArg(index);
  }
};

#endif
