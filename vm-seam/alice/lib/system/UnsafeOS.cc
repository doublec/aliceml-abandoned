//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstaedt@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstdio>
#include <cstdlib>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#if defined(__MINGW32__) || defined(_MSC_VER)
#include <windows.h>
#endif

#include "generic/RootSet.hh"
#include "generic/Closure.hh"
#include "generic/Properties.hh"
#include "alice/primitives/Authoring.hh"

#if PROFILE
#include "generic/Profiler.hh"
#endif

// Global OS.sysErr Exception
static word SysErrConstructor;

#define RAISE_SYS_ERR(a, b)						\
  {									\
    ConVal *conVal =							\
      ConVal::New(Constructor::FromWordDirect(SysErrConstructor), 2);	\
    conVal->Init(0, a);							\
    conVal->Init(1, b);							\
    RAISE(conVal->ToWord());						\
  }

//
// UnsafeOS.FileSys Structure
//

DEFINE1(UnsafeOS_FileSys_chDir) {
  DECLARE_STRING(name, x0);
  if (chdir(name->ExportC())) {
    const char *err = "chDir: cannot change directory";
    RAISE_SYS_ERR(String::New(err)->ToWord(), Store::IntToWord(0));
  }
  RETURN_UNIT;
} END

DEFINE0(UnsafeOS_FileSys_getDir) {
  char buf[MAX_PATH];
  if (!getcwd(buf, MAX_PATH)) {
    const char *err = "getDir: cannot get directory";
    RAISE_SYS_ERR(String::New(err)->ToWord(), Store::IntToWord(0));
  }
  RETURN(String::New(buf)->ToWord());
} END

DEFINE1(UnsafeOS_FileSys_mkDir) {
  DECLARE_STRING(name, x0);
#if defined(__MINGW32__) || defined(_MSC_VER)
  int res = mkdir(name->ExportC());
#else
  int res = mkdir(name->ExportC(),
		  S_IRUSR | S_IWUSR | S_IXUSR |
		  S_IRGRP | S_IWGRP | S_IXGRP |
		  S_IROTH | S_IWOTH | S_IXOTH);
#endif
  if (res) {
    const char *err = "mkDir: cannot create directory";
    RAISE_SYS_ERR(String::New(err)->ToWord(), Store::IntToWord(0));
  }
  RETURN_UNIT;
} END

DEFINE1(UnsafeOS_FileSys_isDir) {
  DECLARE_STRING(name, x0);
  struct stat info;
  if (stat(name->ExportC(), &info)) {
    const char *err = "isDir: cannot get file attributes";
    RAISE_SYS_ERR(String::New(err)->ToWord(), Store::IntToWord(0));
  }
  RETURN_BOOL(S_ISDIR(info.st_mode));
} END

DEFINE1(UnsafeOS_FileSys_fileSize) {
  DECLARE_STRING(name, x0);
  struct stat info;
  if (stat(name->ExportC(), &info)) {
    const char *err = "fileSize: cannot get file size";
    RAISE_SYS_ERR(String::New(err)->ToWord(), Store::IntToWord(0));
  }
  RETURN_INT(info.st_size);
} END

DEFINE1(UnsafeOS_FileSys_modTime) {
  DECLARE_STRING(name, x0);
  struct stat info;
  if (stat(name->ExportC(), &info)) {
    const char *err = "modTime: cannot get file time";
    RAISE_SYS_ERR(String::New(err)->ToWord(), Store::IntToWord(0));
  }
  RETURN_INT(info.st_mtime * 1000000);
} END

DEFINE1(UnsafeOS_FileSys_remove) {
  DECLARE_STRING(name, x0);
  if (unlink(name->ExportC())) {
    const char *err = "remove: cannot remove file";
    RAISE_SYS_ERR(String::New(err)->ToWord(), Store::IntToWord(0));
  }
  RETURN_UNIT;
} END

DEFINE0(UnsafeOS_FileSys_tmpName) {
#if defined(__MINGW32__) || defined(_MSC_VER)
  char prefix[MAX_PATH];
  DWORD ret = GetTempPath(sizeof(prefix),prefix);
  if (ret == 0 || ret >= sizeof(prefix))
    strcpy(prefix,"C:\\TEMP\\");
  char s[MAX_PATH];
  static int counter = 0;
  while (true) {
    std::sprintf(s, "%salice%d", prefix, counter);
    counter = (counter++) % 10000;
    if (access(s, F_OK))
      break;
  }
  RETURN(String::New(s)->ToWord());
#else
  static const char path[] = "/tmp/aliceXXXXXX";
  String *s = String::New(path, sizeof(path));
  mkstemp(reinterpret_cast<char *>(s->GetValue()));
  RETURN(s->ToWord());
#endif
} END

static word UnsafeOS_FileSys() {
  Record *record = Record::New(8);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "chDir",
		 UnsafeOS_FileSys_chDir, 1, true);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "getDir",
		 UnsafeOS_FileSys_getDir, 0, true);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "mkDir",
		 UnsafeOS_FileSys_mkDir, 1, true);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "isDir",
		 UnsafeOS_FileSys_isDir, 1, true);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "fileSize",
		 UnsafeOS_FileSys_fileSize, 1, true);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "modTime",
		 UnsafeOS_FileSys_modTime, 1, true);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "remove",
		 UnsafeOS_FileSys_remove, 1, true);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "tmpName",
		 UnsafeOS_FileSys_tmpName, 1, true);
  return record->ToWord();
}

//
// UnsafeOS.Process Structure
//

DEFINE1(UnsafeOS_Process_system) {
  DECLARE_STRING(command, x0);
  RETURN_INT(system(command->ExportC()));
} END

DEFINE1(UnsafeOS_Process_exit) {
  DECLARE_INT(code, x0);
#if PROFILE
  Profiler::DumpInfo();
#endif
  exit(code);
} END

DEFINE1(UnsafeOS_Process_atExn) {
  DECLARE_CLOSURE(closure, x0);
  Properties::atExn = closure->ToWord();
  RETURN_UNIT;
} END

DEFINE1(UnsafeOS_Process_getEnv) {
  DECLARE_STRING(envVar, x0);
  char *envVal = getenv(envVar->ExportC());
  if (envVal != NULL) {
    TagVal *val = TagVal::New(1, 1); // SOME
    val->Init(0, String::New(envVal)->ToWord());
    RETURN(val->ToWord());
  } else
    RETURN(Store::IntToWord(0)); // NONE
} END

static word UnsafeOS_Process() {
  Record *record = Record::New(6);
  record->Init("success", Store::IntToWord(0));
  record->Init("failure", Store::IntToWord(1));
  INIT_STRUCTURE(record, "UnsafeOS.Process", "system",
		 UnsafeOS_Process_system, 1, true);
  INIT_STRUCTURE(record, "UnsafeOS.Process", "exit",
		 UnsafeOS_Process_exit, 1, true);
  INIT_STRUCTURE(record, "UnsafeOS.Process", "atExn",
		 UnsafeOS_Process_atExn, 1, true);
  INIT_STRUCTURE(record, "UnsafeOS.Process", "getEnv",
		 UnsafeOS_Process_getEnv, 1, true);
  return record->ToWord();
}

//
// UnsafeOS Structure
//

DEFINE2(UnsafeOS_SysErr) {
  Constructor *ccVal = Constructor::FromWord(SysErrConstructor);
  ConVal *conVal     = ConVal::New(ccVal, 2);
  conVal->Init(0, x0);
  conVal->Init(1, x1);
  RETURN(conVal->ToWord());
} END

word UnsafeOS() {
  SysErrConstructor =
    UniqueConstructor::New(String::New("OS.SysErr"))->ToWord();
  RootSet::Add(SysErrConstructor);

  Record *record = Record::New(4);
  record->Init("'SysErr", SysErrConstructor);
  INIT_STRUCTURE(record, "UnsafeOS", "SysErr",
		 UnsafeOS_SysErr, 2, true);
  record->Init("FileSys$", UnsafeOS_FileSys());
  record->Init("Process$", UnsafeOS_Process());
  RETURN_STRUCTURE("UnsafeOS$", record);
}
