//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
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

#include "emulator/Authoring.hh"
#include "emulator/RootSet.hh"
#include "emulator/Closure.hh"
#include "emulator/Properties.hh"

// to be done: respect windows/unix differences

// String Handling
static char *ExportCString(String *s) {
  u_int sLen = s->GetSize();
  String *e  = String::New(sLen + 1);
  u_char *eb = e->GetValue();
  std::memcpy(eb, s->GetValue(), sLen);
  eb[sLen] = '\0';
  return reinterpret_cast<char *>(eb);
}

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

// FileSys Functor
DEFINE1(FileSys_chDir) {
  DECLARE_STRING(name, x0);
  int res = chdir(ExportCString(name));
  if (res) {
    const char *err = "chDir: cannot change directory";
    RAISE_SYS_ERR(String::New(err)->ToWord(), Store::IntToWord(0));
  }
  else {
    RETURN_UNIT;
  }
} END

DEFINE1(FileSys_fileSize) {
  DECLARE_STRING(name, x0);
  struct stat info;
  int res = stat(ExportCString(name), &info);
  if (res) {
    RETURN_INT(info.st_size);
  }
  else {
    const char *err = "fileSize: cannot get file size";
    RAISE_SYS_ERR(String::New(err)->ToWord(), Store::IntToWord(0));
  }
} END

DEFINE0(FileSys_getDir) {
  char buf[MAX_PATH];
  if (getcwd(buf, MAX_PATH)) {
    RETURN(String::New(buf)->ToWord());
  }
  else {
    const char *err = "getDir: cannot get directory";
    RAISE_SYS_ERR(String::New(err)->ToWord(), Store::IntToWord(0));
  }
} END

DEFINE1(FileSys_mkDir) {
  DECLARE_STRING(name, x0);
#if defined(__MINGW32__) || defined(_MSC_VER)
  int res = mkdir(ExportCString(name));
#else
  int res = mkdir(ExportCString(name),
		  S_IRUSR | S_IWUSR | S_IXUSR |
		  S_IRGRP | S_IWGRP | S_IXGRP |
		  S_IROTH | S_IWOTH | S_IXOTH);
#endif
  if (!res) {
    RETURN_UNIT;
  } else {
    const char *err = "mkDir: cannot create directory";
    RAISE_SYS_ERR(String::New(err)->ToWord(), Store::IntToWord(0));
  }
} END

DEFINE1(FileSys_modTime) {
  DECLARE_STRING(name, x0);
  struct stat info;
  int res = stat(ExportCString(name), &info);
  if (!res) {
    RETURN_INT(info.st_mtime * 100000000);
  }
  else {
    const char *err = "modTime: cannot get file time";
    RAISE_SYS_ERR(String::New(err)->ToWord(), Store::IntToWord(0));
  }
} END

DEFINE1(FileSys_remove) {
  DECLARE_STRING(name, x0);
  int res = unlink(ExportCString(name));
  if (!res) {
    RETURN_UNIT;
  }
  else {
    const char *err = "remove: cannot remove file";
    RAISE_SYS_ERR(String::New(err)->ToWord(), Store::IntToWord(0));
  }
} END

DEFINE0(FileSys_tmpName) {
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

static word FileSys(void) {
  Tuple *t = Tuple::New(8);
  t->Init(0, Primitive::MakeClosure("FileSys.chDir",
				    FileSys_chDir, 1, true));
  t->Init(1, Primitive::MakeClosure("FileSys.fileSize",
				    FileSys_fileSize, 1, true));
  t->Init(2, Primitive::MakeClosure("FileSys.getDir",
				    FileSys_getDir, 0, true));
  t->Init(3, Primitive::MakeClosure("FileSys.mkDir",
				    FileSys_mkDir, 1, true));
  t->Init(4, Primitive::MakeClosure("FileSys.modTime",
				    FileSys_modTime, 1, true));
  t->Init(5, Primitive::MakeClosure("Filesys.remove",
				    FileSys_remove, 1, true));
  t->Init(6, Primitive::MakeClosure("FileSys.tmpName",
				    FileSys_tmpName, 1, true));
  return t->ToWord();
}

// Process Functor
DEFINE1(Process_system) {
  DECLARE_STRING(s, x0);
  RETURN_INT(system(ExportCString(s)));
} END

DEFINE1(Process_atExn) {
  DECLARE_CLOSURE(closure, x0);
  Properties::atExn = closure->ToWord();
  RETURN_UNIT;
} END

DEFINE1(Process_exit) {
  DECLARE_INT(code, x0);
  exit(code);
  RETURN_UNIT;
} END

DEFINE1(Process_getEnv) {
  DECLARE_STRING(envVar, x0);
  char *envVal = getenv(ExportCString(envVar));
  if (envVal != NULL) {
    TagVal *val = TagVal::New(1, 1); // SOME
    val->Init(0, String::New(envVal)->ToWord());
    RETURN(val->ToWord());
  }
  else {
    RETURN(Store::IntToWord(0)); // NONE
  }
} END

static word Process(void) {
  Tuple *t = Tuple::New(6);
  t->Init(0, Primitive::MakeClosure("Process.atExn", Process_atExn, 1, true));
  t->Init(1, Primitive::MakeClosure("Process.exit", Process_exit, 1, true));
  t->Init(2, Store::IntToWord(1)); // Process.failure
  t->Init(3, Primitive::MakeClosure("Process.getEnv", Process_getEnv, 1, true));
  t->Init(4, Store::IntToWord(0)); // Process.success
  t->Init(5, Primitive::MakeClosure("Process.system", Process_system, 1, true));
  return t->ToWord();
}

// UnsafeOS Functor

DEFINE2(UnsafeOS_SysErr) {
  Constructor *ccVal = Constructor::FromWord(SysErrConstructor);
  ConVal *conVal     = ConVal::New(ccVal, 2);
  conVal->Init(0, x0);
  conVal->Init(1, x1);
  RETURN(conVal->ToWord());
} END

word UnsafeOS(void) {
  SysErrConstructor =
    UniqueConstructor::New(String::New("OS.SysErr"))->ToWord();
  RootSet::Add(SysErrConstructor);

  Tuple *t = Tuple::New(4);
  t->Init(0, Primitive::MakeClosure("UnsafeOS.SysErr",
				    UnsafeOS_SysErr, 2, true));
  t->Init(1, FileSys());
  t->Init(2, Process());
  t->Init(3, SysErrConstructor);
  RETURN_STRUCTURE(t);
}
