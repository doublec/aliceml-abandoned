//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#ifndef __BASE_HH__
#define __BASE_HH__

#include <cstdio>

#ifdef DEBUG_CHECK
#define Assert(Cond)                                                                 \
  if (!(Cond)) {                                                                      \
    char *t = NULL;                                                                   \
    std::fprintf(stderr, "%s:%d assertion '%s' failed\n", __FILE__, __LINE__, #Cond); \
    t[0] = 0;                                                                         \
    exit(0);                                                                          \
  }
#else
#define Assert(Cond)
#endif

#define Error(s)							\
  std::fprintf(stderr, "%s:%d error '%s'\n", __FILE__, __LINE__, s);	\
  static_cast<char *>(NULL)[0] = 0;					\
  exit(0);

#include "types.hh"

#endif
