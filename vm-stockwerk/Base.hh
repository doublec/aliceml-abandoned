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

#ifndef MAX_PATH
# ifdef _POSIX_PATH_MAX
#  define MAX_PATH _POSIX_PATH_MAX
# else
// use some reasonable default
#  define MAX_PATH (1025)
# endif
#endif

#include <cstdlib>

#if defined(__MINGW32__) || defined(_MSC_VER)
# if defined(STOCKWERK_FOREIGN)
#  define DllExport __declspec(dllimport)
# else
#  define DllExport __declspec(dllexport)
# endif
#else
# define DllExport
#endif

void AssertOutline(const char *file, int line, const char *message);
#define AssertBase(cond, message) \
  if (!(cond)) { AssertOutline(__FILE__, __LINE__, message); exit(0); } else {}

//--** should be removed
#ifdef DEBUG_CHECK
#define Assert(cond) \
  if (!(cond)) { AssertOutline(__FILE__, __LINE__, #cond); exit(0); } else {}
#else
#define Assert(cond)
#endif

void ErrorOutline(const char *file, int line, const char *message);
#define Error(message) { ErrorOutline(__FILE__, __LINE__, message); exit(0); }

#endif
