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

// Eclispe test
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

#if HAVE_DLLS
# if defined(SEAM_FOREIGN)
#  define SeamDll __declspec(dllimport)
# else
#  define SeamDll __declspec(dllexport)
# endif
#else
# define SeamDll
#endif

// We surely have a ms compiler (-Ze option enabled)
#if defined(_MSC_EXTENSIONS)
#define SeamMemberDll

#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0510
#endif

#include <windows.h>

#else
#define SeamMemberDll SeamDll
#endif

SeamDll void AssertOutline(const char *file, int line, const char *message);

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

SeamDll void ErrorOutline(const char *file, int line, const char *message);

#define Error(message) {					\
  ErrorOutline(__FILE__, __LINE__, message); exit(1);	\
}

#endif
