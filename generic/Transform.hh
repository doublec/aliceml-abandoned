//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __EMULATOR_TRANSFORM_HH__
#define __EMULATOR_TRANSFORM_HH__

class Transform : private Block {
private:
  static const u_int NAME_POS     = 0;
  static const u_int ARGUMENT_POS = 1;
  static const u_int SIZE         = 2;
public:
  using Block::ToWord;

  // Transform Constructor
  static Transform *New(Chunk *name, word argument) {
    Block *p = Store::AllocBlock(TRANSFORM_LABEL, 2);
    p->InitArg(NAME_POS, name->ToWord());
    p->InitArg(ARGUMENT_POS, argument);
    return static_cast<Transform *>(p);
  }
  // Transform Untagging
  static Transform *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER || p->GetLabel() == TRANSFORM_LABEL);
    return static_cast<Transform *>(p);
  }
  static Transform *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert(p->GetLabel() == TRANSFORM_LABEL);
    return static_cast<Transform *>(p);
  }

  // Transform Accessors
  Chunk *GetName() {
    return Store::DirectWordToChunk(GetArg(NAME_POS));
  }
  word GetArgument() {
    return GetArg(ARGUMENT_POS);
  }
};

#endif
