//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __JAVA__BASE_HH__
#define __JAVA__BASE_HH__

#if defined(__MINGW32__) || defined(_MSC_VER)
# if defined(JAVA_FOREIGN)
#  define JavaDll __declspec(dllimport)
# else
#  define JavaDll __declspec(dllexport)
# endif
#else
# define JavaDll
#endif

#endif
