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

#ifndef __GENERIC__CLOSURE_HH__
#define __GENERIC__CLOSURE_HH__

#if defined(INTERFACE)
#pragma interface "generic/Closure.hh"
#endif

#include "generic/ConcreteCode.hh"

class Closure: private Block {
private:
  static const u_int CONCRETE_CODE_POS = 1;
public:
  using Block::ToWord;

  static Closure *New(ConcreteCode *concreteCode, u_int size) {
    Block *b = Store::AllocBlock(CLOSURE_LABEL, 1 + size);
    b->InitArg(CONCRETE_CODE_POS, concreteCode->ToWord());
    return static_cast<Closure *>(b);
  }
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

  ConcreteCode *GetConcreteCode() {
    return ConcreteCode::FromWordDirect(GetArg(CONCRETE_CODE_POS));
  }
  u_int GetSize() {
    return Block::GetSize() - 1;
  }
  void Init(u_int index, word value) {
    InitArg(index + 2, value);
  }
  word Sub(u_int index) {
    return GetArg(index + 2);
  }
};

#endif __GENERIC__CLOSURE_HH__
