#ifndef __base_hh__
#define __base_hh__

#include <stdio.h>

#ifdef DEBUG_CHECK
#define Assert(Cond)                                                             \
  if (!(Cond)) {                                                                 \
    char *t = NULL;                                                              \
    fprintf(stderr, "%s:%d assertion '%s' failed\n", __FILE__, __LINE__, #Cond); \
    t[0] = 0;                                                                 \
    exit(0);                                                                     \
  }
#else
#define Assert(Cond)
#endif

#include "types.hh"

#endif
