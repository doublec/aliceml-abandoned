//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000-2001
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__CLOSURE_HH__
#define __GENERIC__CLOSURE_HH__

#if defined(INTERFACE)
#pragma interface "emulator/Closure.hh"
#endif

#include "store/Store.hh"

class Closure: private Block {
private:
  static const u_int CONCRETE_CODE_POS = 0;
  static const u_int BASE_SIZE         = 1;
public:
  using Block::ToWord;

  // Closure Constructor
  static Closure *New(word concreteCode, u_int size) {
    Block *b = Store::AllocBlock(CLOSURE_LABEL, BASE_SIZE + size);
    b->InitArg(CONCRETE_CODE_POS, concreteCode);
    return static_cast<Closure *>(b);
  }
  // Closure Untagging
  static Closure *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == CLOSURE_LABEL);
    return static_cast<Closure *>(b);
  }
  static Closure *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == CLOSURE_LABEL);
    return static_cast<Closure *>(b);
  }

  // Closure Accessors
  word GetConcreteCode() {
    return GetArg(CONCRETE_CODE_POS);
  }
  u_int GetSize() {
    return Block::GetSize() - BASE_SIZE;
  }
  void Init(u_int index, word value) {
    InitArg(index + BASE_SIZE, value);
  }
  word Sub(u_int index) {
    return GetArg(index + BASE_SIZE);
  }
};

#endif
