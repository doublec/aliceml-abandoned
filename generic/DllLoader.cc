//
// Author:
//   Guido Tack <tack@ps.uni-sb.de>
//   Andreas Rossberg <rossberg@ps.uni-sb.de>
//
// Copyright:
//   Guido Tack and Andreas Rossberg, 2003-2005
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "generic/DllLoader.hh"

void trace(const char *s, const char *arg = NULL) {
  static const char *logname = getenv("ALICE_TRACE_DLL");
  if (logname) {
    static FILE *logfile = strcmp(logname, "-") ? fopen(logname, "w") : stderr;
    if (logfile) {
      fprintf(logfile, s, arg);
      fflush(logfile);
    }
  }
}

void DllLoader::Init() {
    trace("[DllLoader::Init()");
#if !HAVE_LOADLIBRARY && HAVE_LIBLTDL
    lt_dlinit();
    trace(" = ()");
#endif
    trace("]\n");
}

DllLoader::libhandle DllLoader::OpenLibrary(String *fileName) {
    trace("[DllLoader::OpenLibrary(%s)", fileName->ExportC());
    DllLoader::libhandle ret;
#if HAVE_LOADLIBRARY
    ret = LoadLibrary(fileName->ExportC());
#elif HAVE_LIBLTDL
    ret = lt_dlopen(fileName->ExportC());
#endif
    trace(" = %p]\n", (char*)ret);
    return ret;
}

void DllLoader::CloseLibrary(DllLoader::libhandle handle) {
    trace("[DllLoader::CloseLibrary(%p)", (char*)handle);
#if HAVE_LOADLIBRARY
    FreeLibrary(handle);
#elif HAVE_LIBLTDL
    lt_dlclose(handle);
#endif
    trace("]\n");
}

void *DllLoader::GetSymbol(DllLoader::libhandle libraryHandle,
			   String *symbolName) {
#if HAVE_LOADLIBRARY
  return (void *) GetProcAddress(libraryHandle, symbolName->ExportC());
#elif HAVE_LIBLTDL
  return (void *) lt_dlsym(libraryHandle, symbolName->ExportC());
#endif
}

String *DllLoader::GetLastError() {
#if HAVE_LOADLIBRARY
  DWORD errorCode = ::GetLastError();
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
#elif HAVE_LIBLTDL
  const char *msg = lt_dlerror();
  return String::New(msg? msg: "no error");
#endif
}
