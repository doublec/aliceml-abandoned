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

#ifndef __ALICE__GUID__
#define __ALICE__GUID__

#if defined(INTERFACE)
#pragma interface "alice/Guid.hh"
#endif

#include "Seam.hh"
#include "alice/Base.hh"

class AliceDll Guid: private String {
public:
  using Block::ToWord;
  using String::Hash;

  static word vmGuid;
  static void Init();

  static Guid *New();
  static Guid *FromWord(word x) {
    return static_cast<Guid *>(String::FromWord(x));
  }
  static Guid *FromWordDirect(word x) {
    return static_cast<Guid *>(String::FromWordDirect(x));
  }

  static int Compare(Guid *guid1, Guid *guid2);
};

#endif
