//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/primitives/Authoring.hh"

static inline int GetPlatform() {
#if defined(__CYGWIN32__) || defined(__MINGW32__) || defined(_MSC_VER)
  return 1;
#else
  return 0;
#endif
}

word UnsafeConfig(void) {
  Tuple *t = Tuple::New(2);
  t->Init(0, Store::IntToWord(GetPlatform()));
  t->Init(1, String::New("stockwerk")->ToWord());
  RETURN_STRUCTURE(t);
}
