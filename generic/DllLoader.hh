//
// Author:
//   Guido Tack <tack@ps.uni-sb.de>
//
// Copyright:
//   Guido Tack, 2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__DLLLOADER_HH__
#define __GENERIC__DLLLOADER_HH__

#if defined(INTERFACE)
#pragma interface "generic/DllLoader.hh"
#endif

#include "ltdl.h"
#include "generic/String.hh"

class SeamDll DllLoader {
public:
#if HAVE_LIBLTDL
typedef lt_dlhandle libhandle;
#elif HAVE_LOADLIBRARY
typedef HMODULE libhandle;
#endif

  static void Init();
  static libhandle OpenLibrary(String *filename);
  static void CloseLibrary(libhandle handle);
  static void *GetSymbol(libhandle libraryHandle,
			 String *symbolName);
  static String *GetLastError();

};

#endif
