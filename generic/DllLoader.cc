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

#include "generic/DllLoader.hh"

void DllLoader::Init() {
#if HAVE_LIBLTDL
    lt_dlinit();
#endif
}

DllLoader::libhandle DllLoader::OpenLibrary(String *fileName) {
#if HAVE_LIBLTDL
  return lt_dlopen(fileName->ExportC());
#elif HAVE_LOADLIBRARY
  return LoadLibrary(fileName->ExportC());
#endif
}

void DllLoader::CloseLibrary(DllLoader::libhandle handle) {
#if HAVE_LIBLTDL
  lt_dlclose(handle);
#elif HAVE_LOADLIBRARY
  FreeLibrary(handle);
#endif
}

void *DllLoader::GetSymbol(DllLoader::libhandle libraryHandle,
			   String *symbolName) {
#if HAVE_LIBLTDL
  return (void *) lt_dlsym(libraryHandle, symbolName->ExportC());
#elif HAVE_LOADLIBRARY
  return (void *) GetProcAddress(libraryHandle, symbolName->ExportC());
#endif
}

String *DllLoader::GetLastError() {
#if HAVE_LIBLTDL
  const char *msg = lt_dlerror();
  return String::New(msg? msg: "no error");
#elif HAVE_LOADLIBRARY
  DWORD errorCode = GetLastError();
  char *lpMsgBuf;
  int n = FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
			FORMAT_MESSAGE_IGNORE_INSERTS |
			FORMAT_MESSAGE_FROM_SYSTEM |
			FORMAT_MESSAGE_MAX_WIDTH_MASK, NULL, errorCode,
			MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
			(LPTSTR) &lpMsgBuf, 0, NULL);
  String *s;
  if (!n) {
    static char buffer[32];
    std::sprintf(buffer, "Error code %ld", errorCode);
    s = String::New(buffer);
  } else
    s = String::New(lpMsgBuf, n);
  LocalFree(lpMsgBuf);
  return s;
#endif
}

