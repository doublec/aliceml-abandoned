//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__UNIQUE_STRING_HH__
#define __GENERIC__UNIQUE_STRING_HH__

#if defined(INTERFACE)
#pragma interface "generic/UniqueString.hh"
#endif

#include "generic/String.hh"

class SeamDll UniqueString: private Block {
protected:
  enum { STRING_POS, HASH_VALUE_POS, SIZE };
public:
  using Block::ToWord;

  static void Init();

  static UniqueString *New(String *s);

  String *ToString() {
    return String::FromWordDirect(GetArg(STRING_POS));
  }
  u_int Hash() {
    return Store::DirectWordToInt(GetArg(HASH_VALUE_POS));
  }

  static UniqueString *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == UNIQUESTRING_LABEL);
    return STATIC_CAST(UniqueString *, b);
  }
  static UniqueString *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == UNIQUESTRING_LABEL);
    return STATIC_CAST(UniqueString *, b);
  }
};

#endif
