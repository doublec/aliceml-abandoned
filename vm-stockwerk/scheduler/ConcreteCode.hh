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

#ifndef __CONCRETE_CODE_HH__
#define __CONCRETE_CODE_HH__

#include "store/store.hh"

static const BlockLabel ConcreteCodeLabel = Store::MakeLabel(0); //--**

class Interpreter;

class ConcreteCode: private Block {
private:
  static const int SIZE = 2;
  static const int ABSTRACT_CODE_POS = 1;
  static const int INTERPRETER_POS = 2;
public:
  using Block::ToWord;

  static ConcreteCode *New(word abstractCode, Interpreter *interpreter,
			   u_int size) {
    Block *b = Store::AllocBlock(ConcreteCodeLabel, SIZE + size);
    b->InitArg(ABSTRACT_CODE_POS, abstractCode);
    b->InitArg(INTERPRETER_POS, Store::UnmanagedPointerToWord(reinterpret_cast<void *>(interpreter)));
    return static_cast<ConcreteCode *>(b);
  }
  static ConcreteCode *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == ConcreteCodeLabel);
    return static_cast<ConcreteCode *>(b);
  }

  word GetAbstractCode() {
    return GetArg(ABSTRACT_CODE_POS);
  }
  Interpreter *GetInterpreter() {
    return reinterpret_cast<Interpreter *>(Store::WordToUnmanagedPointer(GetArg(INTERPRETER_POS)));
  }
  void Init(u_int index, word value) {
    InitArg(SIZE + index + 1, value);
  }
  word Get(u_int index) {
    return GetArg(SIZE + index + 1);
  }
};

#endif __CONCRETE_CODE_HH__
