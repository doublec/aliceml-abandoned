#ifndef __SYSDEP_H_SEEN
#define __SYSDEP_H_SEEN

#include "lightning.h"

#ifndef HAVE_MEMCPY
#define memcpy(d, s, n)  bcopy((s),(d),(n))
#endif

#endif
