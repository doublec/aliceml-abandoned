//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "Base.hh"
#endif

#include <cstdio>
#include "Base.hh"

void AssertOutline(const char *file, int line, const char *message) {
  std::fprintf(stderr, "%s:%d assertion '%s' failed\n", file, line, message);
  std::fflush(stderr);
  static_cast<char *>(NULL)[0] = 0;
  std::exit(1);
}

void ErrorOutline(const char *file, int line, const char *message) {
  std::fprintf(stderr, "%s:%d error '%s'\n", file, line, message);
  std::fflush(stderr);
  static_cast<char *>(NULL)[0] = 0;
  std::exit(1);
}
