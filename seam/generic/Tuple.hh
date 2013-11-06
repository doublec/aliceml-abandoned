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

  // Tuple Constructors
  
  static Tuple *New(u_int n) {
    return static_cast<Tuple *>(Store::AllocBlock(TUPLE_LABEL, n));
  }
  
  static Tuple *Single(word a) {
    Tuple *t = New(1);
    t->Init(0, a);
    return t;
  }
  
  static Tuple *Pair(word a, word b) {
    Tuple *t = New(2);
    t->Init(0, a);
    t->Init(1, b);
    return t;
  }
  
  static Tuple *Triple(word a, word b, word c) {
    Tuple *t = New(3);
    t->Init(0, a);
    t->Init(1, b);
    t->Init(2, c);
    return t;
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
