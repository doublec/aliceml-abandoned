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

#ifndef __GENERIC_GUID__
#define __GENERIC_GUID__

#if defined(INTERFACE)
#pragma interface "emulator/Guid.hh"
#endif

#include "Tuple.hh"

class Guid: private Tuple {
private:
  static const u_int SIZE = 4;
public:
  using Block::ToWord;

  static word vmGuid;
  static void Init();

  static Guid *New();
  static Guid *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == TUPLE_LABEL && b->GetSize() == SIZE); //--**
    return static_cast<Guid *>(b);
  }
  static Guid *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == TUPLE_LABEL && b->GetSize() == SIZE); //--**
    return static_cast<Guid *>(b);
  }

  static int Compare(Guid *guid1, Guid *guid2);
  u_int Hash();
};

#endif
