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

static inline s_int GetPlatform() {
#if defined(__CYGWIN32__) || defined(__MINGW32__) || defined(_MSC_VER)
  return 1; // WIN32
#else
  return 0; // UNIX
#endif
}

word UnsafeConfig() {
  Record *record = Record::New(2);
  record->Init("platform", Store::IntToWord(GetPlatform()));
  record->Init("vm", String::New("stockwerk")->ToWord());
  RETURN_STRUCTURE("UnsafeConfig$", record);
}
