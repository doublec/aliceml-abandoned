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

#ifndef __SCHEDULER__CONCRETE_CODE_HH__
#define __SCHEDULER__CONCRETE_CODE_HH__

#if defined(INTERFACE)
#pragma interface "scheduler/ConcreteCode.hh"
#endif

#include "store/Store.hh"

class Interpreter;

class ConcreteCode: private Block {
private:
  static const u_int SIZE = 2;
  static const u_int ABSTRACT_CODE_POS = 1;
  static const u_int INTERPRETER_POS = 2;
public:
  using Block::ToWord;

  static ConcreteCode *New(word abstractCode, Interpreter *interpreter,
			   u_int size) {
    //--** will be a block with handler
    Block *b = Store::AllocBlock(CONCRETECODE_LABEL, SIZE + size);
    b->InitArg(ABSTRACT_CODE_POS, abstractCode);
    b->InitArg(INTERPRETER_POS, Store::UnmanagedPointerToWord(interpreter));
    return static_cast<ConcreteCode *>(b);
  }
  static ConcreteCode *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == CONCRETECODE_LABEL);
    return static_cast<ConcreteCode *>(b);
  }
  static ConcreteCode *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == CONCRETECODE_LABEL);
    return static_cast<ConcreteCode *>(b);
  }

  word GetAbstractCode() {
    return GetArg(ABSTRACT_CODE_POS);
  }
  Interpreter *GetInterpreter() {
    return static_cast<Interpreter *>
      (Store::DirectWordToUnmanagedPointer(GetArg(INTERPRETER_POS)));
  }
  void Init(u_int index, word value) {
    InitArg(SIZE + index + 1, value);
  }
  word Get(u_int index) {
    return GetArg(SIZE + index + 1);
  }
};

#endif __SCHEDULER__CONCRETE_CODE_HH__
