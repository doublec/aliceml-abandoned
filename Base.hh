//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __BASE_HH__
#define __BASE_HH__

#if defined(INTERFACE)
#pragma interface "Base.hh"
#endif

#ifndef MAX_PATH
# ifdef _POSIX_PATH_MAX
#  define MAX_PATH _POSIX_PATH_MAX
# else
// use some reasonable default
#  define MAX_PATH (1025)
# endif
#endif

#include <cstdio>
#include <cstdlib>
#include <unistd.h>

#if defined(__MINGW32__) || defined(_MSC_VER)
# if defined(STOCKWERK_FOREIGN)
#  define DllExport __declspec(dllimport)
# else
#  define DllExport __declspec(dllexport)
# endif
#else
# define DllExport
#endif

DllExport void AssertOutline(const char *file, int line, const char *message);

#define AssertBase(cond, message)				\
  if (!(cond)) {						\
    AssertOutline(__FILE__, __LINE__, message); exit(1);	\
  } else {}

//--** should be removed
#ifdef DEBUG_CHECK
#define Assert(cond)						\
  if (!(cond)) {						\
    AssertOutline(__FILE__, __LINE__, #cond); exit(1);	\
  } else {}
#else
#define Assert(cond)
#endif

DllExport void ErrorOutline(const char *file, int line, const char *message);

#define Error(message) {					\
  ErrorOutline(__FILE__, __LINE__, message); exit(1);	\
}

#endif
