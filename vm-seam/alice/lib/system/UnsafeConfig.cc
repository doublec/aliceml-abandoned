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

#include "emulator/Authoring.hh"

static inline int GetPlatform() {
#if defined(__CYGWIN32__) || defined(__MINGW32__)
  return 1;
#else
  return 0;
#endif
}

word UnsafeConfig(void) {
  Tuple *t = Tuple::New(1);
  t->Init(0, Store::IntToWord(GetPlatform()));
  RETURN_STRUCTURE(t);
}
