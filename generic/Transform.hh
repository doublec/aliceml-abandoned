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

#ifndef __GENERIC__TRANSFORM_HH__
#define __GENERIC__TRANSFORM_HH__

#include "store/Store.hh"

class SeamDll Transform: private Block {
protected:
  enum { NAME_POS, ARGUMENT_POS, SIZE };
public:
  using Block::ToWord;

  // Transform Constructor
  static Transform *New(Chunk *name, word argument) {
    Block *p = Store::AllocBlock(TRANSFORM_LABEL, 2);
    p->InitArg(NAME_POS, name->ToWord());
    p->InitArg(ARGUMENT_POS, argument);
    return STATIC_CAST(Transform *, p);
  }
  // Transform Untagging
  static Transform *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER || p->GetLabel() == TRANSFORM_LABEL);
    return STATIC_CAST(Transform *, p);
  }
  static Transform *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert(p->GetLabel() == TRANSFORM_LABEL);
    return STATIC_CAST(Transform *, p);
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
