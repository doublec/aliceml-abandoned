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
#include "emulator/Authoring.hh"
#include "emulator/RootSet.hh"

// to be done: respect windows/unix differences

// String Handling
static String *ExportString(String *s) {
  u_int sLen = s->GetSize();
  String *e  = String::New(sLen + 1);
  char *eb   = e->GetValue();
  memcpy(eb, s->GetValue(), sLen);
  eb[sLen] = 0x00;
  return e;
}

static char *ExportChar(String *s) {
  return ExportString(s)->GetValue();
}

// Global OS.sysErr Exception
static word SysErrConstructor;

#define RAISE_SYS_ERR_EXCEPTION(A, B)                            \
  Constructor *ccVal = Constructor::FromWord(SysErrConstructor); \
  ConVal *conVal = ConVal::New(ccVal, 2);                        \
  conVal->Init(0, A);                                            \
  conVal->Init(1, B);                                            \
  Scheduler::currentData = conVal->ToWord();                     \
  return Interpreter::RAISE;

// FileSys Functor
DEFINE1(FileSys_chDir) {
  DECLARE_STRING(name, x0);
  int res = chdir(ExportChar(name));
  if (res) {
    const char *err = "chDir: cannot change directory";
    RAISE_SYS_ERR_EXCEPTION(String::New(err)->ToWord(), Store::IntToWord(0));
  }
  else {
    RETURN_UNIT;
  }
} END

DEFINE1(FileSys_fileSize) {
  DECLARE_STRING(name, x0);
  struct stat info;
  int res = stat(ExportChar(name), &info);
  if (res) {
    RETURN_INT(info.st_size);
  }
  else {
    const char *err = "fileSize: cannot get file size";
    RAISE_SYS_ERR_EXCEPTION(String::New(err)->ToWord(), Store::IntToWord(0));
  }
} END

DEFINE0(FileSys_getDir) {
  static u_int size = 1024;
  static char *buf  = (char *) malloc(sizeof(char) * size);
  while (getcwd(buf, size) == NULL) {
    size = (size * 3) >> 2;
    free(buf);
    buf = (char *) malloc(sizeof(char) * size);
  }
  RETURN(String::New(buf)->ToWord());
} END

DEFINE1(FileSys_mkDir) {
  DECLARE_STRING(name, x0);
  int res = mkdir(ExportChar(name),
		  S_IRUSR | S_IWUSR | S_IXUSR |
		  S_IRGRP | S_IWGRP | S_IXGRP |
		  S_IROTH | S_IWOTH | S_IXOTH);
  if (!res) {
    RETURN_UNIT;
  }
  else {
    const char *err = "mkDir: cannot create directory";
    RAISE_SYS_ERR_EXCEPTION(String::New(err)->ToWord(), Store::IntToWord(0));
  }
} END

DEFINE1(FileSys_modTime) {
  DECLARE_STRING(name, x0);
  struct stat info;
  int res = stat(ExportChar(name), &info);
  if (!res) {
    RETURN_INT(info.st_mtime * 100000000);
  }
  else {
    const char *err = "modTime: cannot get file time";
    RAISE_SYS_ERR_EXCEPTION(String::New(err)->ToWord(), Store::IntToWord(0));
  }
} END

DEFINE1(FileSys_remove) {
  DECLARE_STRING(name, x0);
  int res = unlink(ExportChar(name));
  if (!res) {
    RETURN_UNIT;
  }
  else {
    const char *err = "remove: cannot remove file";
    RAISE_SYS_ERR_EXCEPTION(String::New(err)->ToWord(), Store::IntToWord(0));
  }
} END

DEFINE0(FileSys_tmpName) {
  RETURN(String::New(tmpnam(NULL))->ToWord());
} END

static word FileSys(void) {
  Tuple *t = Tuple::New(8);
  t->Init(0, Primitive::MakeClosure(FileSys_chDir, 1));
  t->Init(1, Primitive::MakeClosure(FileSys_fileSize, 1));
  t->Init(2, Primitive::MakeClosure(FileSys_getDir, 0));
  t->Init(3, Primitive::MakeClosure(FileSys_mkDir, 1));
  t->Init(4, Primitive::MakeClosure(FileSys_modTime, 1));
  t->Init(5, Primitive::MakeClosure(FileSys_remove, 1));
  t->Init(6, Primitive::MakeClosure(FileSys_tmpName, 1));
  return t->ToWord();
}

// Process Functor
DEFINE1(Process_system) {
  DECLARE_STRING(s, x0);
  RETURN_INT(system(ExportChar(s)));
} END

DEFINE1(Process_atExn) {
  DECLARE_CLOSURE(closure, x0);
  // to be done: Storing the closure
  closure = closure;
  RETURN_UNIT;
} END

DEFINE1(Process_exit) {
  DECLARE_INT(code, x0);
  exit(code);
  RETURN_UNIT;
} END

DEFINE1(Process_getEnv) {
  DECLARE_STRING(envVar, x0);
  char *envVal = getenv(ExportChar(envVar));
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
  t->Init(0, Primitive::MakeClosure(Process_atExn, 1));
  t->Init(1, Primitive::MakeClosure(Process_exit, 1));
  t->Init(2, Store::IntToWord(0)); // failure,  to be done
  t->Init(3, Primitive::MakeClosure(Process_getEnv, 1));
  t->Init(4, Store::IntToWord(1)); // success, to be done
  t->Init(5, Primitive::MakeClosure(Process_system, 1));
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
  t->Init(0, Primitive::MakeClosure(UnsafeOS_SysErr, 2));
  t->Init(1, FileSys());
  t->Init(2, Process());
  t->Init(3, SysErrConstructor);
  RETURN_STRUCTURE(t);
}
