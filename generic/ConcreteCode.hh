//
// Authors:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000-2001
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__CONCRETE_CODE_HH__
#define __GENERIC__CONCRETE_CODE_HH__

#if defined(INTERFACE)
#pragma interface "generic/ConcreteCode.hh"
#endif

#include "generic/Interpreter.hh"

class ConcreteCode: private Block {
private:
  static const u_int INTERPRETER_POS = 0;
  static const u_int BASE_SIZE       = 1;
public:
  using Block::ToWord;

  // ConcreteCode Constructor
  static ConcreteCode *New(Interpreter *interpreter, u_int size) {
    Block *b = Store::AllocBlock(CONCRETE_LABEL, BASE_SIZE + size);
    b->InitArg(INTERPRETER_POS, Store::UnmanagedPointerToWord(interpreter));
    return static_cast<ConcreteCode *>(b);
  }
  // ConcreteCode Untagging
  static ConcreteCode *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == CONCRETECODE_LABEL);
    return static_cast<ConcreteCode *>(b);
  }
  static ConcreteCode *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == CONCRETE_LABEL);
    return static_cast<ConcreteCode *>(b);
  }

  // ConcreteCode Accessors
  Interpreter *GetInterpreter() {
    return static_cast<Interpreter *>
      (Store::DirectWordToUnmanagedPointer(GetArg(INTERPRETER_POS)));
  }
  void Init(u_int index, word value) {
    InitArg(BASE_SIZE + index, value);
  }
  word Get(u_int index) {
    return GetArg(BASE_SIZE + index);
  }
};

#endif
