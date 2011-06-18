//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstaedt@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cctype>

#include "alice/NativeCodeJitter.hh"
#include "alice/Authoring.hh"


#if defined(__MINGW32__) || defined(_MSC_VER)

#include <windows.h>
#include <shlobj.h>

#else

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <dirent.h>
#define Interruptible(res, call)        \
  int res;                  \
  do {                      \
    res = call;                 \
  } while (res < 0 && errno == EINTR);
#define GetLastError() errno

#define DECLARE_LINUXDIR(ld, x) \
  DECLARE_WRAPPEDUNMANAGEDPOINTER(DIR, ld, x) \

#endif

class DirFinalizationSet : public FinalizationSet {
public:
  virtual void Finalize(word w) {
#if defined(__MINGW32__) || defined(_MSC_VER)
    Tuple *tuple = Tuple::FromWord(w);
    Cell *closedCell = Cell::FromWord(tuple->Sel(0));
    if (Store::WordToInt(closedCell->Access()) == 0) {
      Cell *handleCell = Cell::FromWord(tuple->Sel(2));
      HANDLE handle = Store::WordToUnmanagedPointer(handleCell->Access());
      closedCell->Assign(Store::IntToWord(1));
      FindClose(handle);
    }
#else
    WrappedUnmanagedPointer<DIR> *ld = WrappedUnmanagedPointer<DIR>::FromWord(w);
    if (!ld->IsNull()) {
      closedir(ld->GetValue());
      ld->SetNull();
    }
#endif
  }
};

static DirFinalizationSet *dirFinalizationSet;
static word wBufferString;

// Also Needed for UnsafeUnix
word SysErrConstructor;
#include "SysErr.icc"

static const char *DIRECTORY_STREAM_CLOSED = "Directory stream is closed and cannot be used.";

//
// UnsafeOS.FileSys Structure
//

DEFINE1(UnsafeOS_FileSys_openDir) {
  DECLARE_STRING(name, x0);
#if defined(__MINGW32__) || defined(_MSC_VER)
  char buf[MAX_PATH];
  std::strcpy(buf, name->ExportC());
  std::strcat(buf, "/*");

  WIN32_FIND_DATA findData;
  HANDLE handle = FindFirstFile(buf, &findData);
  word entry;
  if (handle == INVALID_HANDLE_VALUE) {
    if (GetLastError() != ERROR_NO_MORE_FILES) RAISE_SYS_ERR();
    entry = Store::IntToWord(0);
  } else {
    entry = String::New(findData.cFileName)->ToWord();
  }

  String *dir = String::New(buf);
  Cell *closedCell = Cell::New(Store::IntToWord(0));
  Cell *handleCell = Cell::New(Store::UnmanagedPointerToWord(handle));
  Cell *entryCell = Cell::New(entry);
  Tuple *tuple = Tuple::New(4);
  tuple->Init(0, closedCell->ToWord());
  tuple->Init(1, dir->ToWord());
  tuple->Init(2, handleCell->ToWord());
  tuple->Init(3, entryCell->ToWord());
  
  dirFinalizationSet->Register(tuple->ToWord());
  RETURN(tuple->ToWord());
#else
  DIR *d = opendir(name->ExportC());
  if (!d) RAISE_SYS_ERR();
  
  word ld = WrappedUnmanagedPointer<DIR>::New(d)->ToWord();
  dirFinalizationSet->Register(ld);
  RETURN(ld);
#endif
} END

DEFINE1(UnsafeOS_FileSys_readDir) {
#if defined(__MINGW32__) || defined(_MSC_VER)
  DECLARE_TUPLE(tuple, x0);
  Cell *closedCell = Cell::FromWord(tuple->Sel(0));
  
  if (Store::WordToInt(closedCell->Access()) != 0) {
    RAISE(MakeSysErr(DIRECTORY_STREAM_CLOSED));
  }
  
  Cell *handleCell = Cell::FromWord(tuple->Sel(2));
  Cell *entryCell = Cell::FromWord(tuple->Sel(3));
  HANDLE handle = Store::WordToUnmanagedPointer(handleCell->Access());
  word entry = entryCell->Access();
  word newEntry;

  if (Store::WordToInt(entry) != INVALID_INT) {
    RETURN_INT(0);
  }

  WIN32_FIND_DATA findData;
  if (FindNextFile(handle, &findData) == FALSE) {
    if (GetLastError() != ERROR_NO_MORE_FILES) RAISE_SYS_ERR();
    newEntry = Store::IntToWord(0);
  } else {
    newEntry = String::New(findData.cFileName)->ToWord();
  }
  entryCell->Assign(newEntry);

  TagVal *some = TagVal::New(1,1);
  some->Init(0, entry);
  RETURN(some->ToWord());
#else
  DECLARE_LINUXDIR(ld, x0);
  
  if (ld->IsNull()) {
    RAISE(MakeSysErr(DIRECTORY_STREAM_CLOSED));
  }
  
  errno = 0;
  if (struct dirent *n = readdir(ld->GetValue())) {
    TagVal *some = TagVal::New(Types::SOME, 1);
    some->Init(0, String::New(n->d_name)->ToWord());
    RETURN(some->ToWord());
  } else if (errno) {
    RAISE_SYS_ERR();
  } else {
    RETURN_INT(Types::NONE);
  }
#endif
} END

DEFINE1(UnsafeOS_FileSys_rewindDir) {
#if defined(__MINGW32__) || defined(_MSC_VER)
  DECLARE_TUPLE(tuple, x0);
  Cell *closedCell = Cell::FromWord(tuple->Sel(0));
  
  if (Store::WordToInt(closedCell->Access()) != 0) {
    RAISE(MakeSysErr(DIRECTORY_STREAM_CLOSED));
  }
  
  String *dir = String::FromWord(tuple->Sel(1));
  Cell *handleCell = Cell::FromWord(tuple->Sel(2));
  Cell *entryCell = Cell::FromWord(tuple->Sel(3));
  HANDLE handle = Store::WordToUnmanagedPointer(handleCell->Access());

  if (FindClose(handle) == FALSE) {
    closedCell->Assign(Store::IntToWord(1));
    RAISE_SYS_ERR();
  }

  WIN32_FIND_DATA findData;
  handle = FindFirstFile(dir->ExportC(), &findData);
  word entry;
  if (handle == INVALID_HANDLE_VALUE) {
    if (GetLastError() != ERROR_NO_MORE_FILES) {
      closedCell->Assign(Store::IntToWord(1));
      RAISE_SYS_ERR();
    }
    entry = Store::IntToWord(0);
  } else {
    entry = String::New(findData.cFileName)->ToWord();
  }

  handleCell->Assign(Store::UnmanagedPointerToWord(handle));
  entryCell->Assign(entry);

  RETURN_UNIT;
#else
  DECLARE_LINUXDIR(ld, x0);
  
  if (ld->IsNull()) {
    RAISE(MakeSysErr(DIRECTORY_STREAM_CLOSED));
  }
  
  rewinddir(ld->GetValue());

  RETURN_UNIT;
#endif
} END

DEFINE1(UnsafeOS_FileSys_closeDir) {
#if defined(__MINGW32__) || defined(_MSC_VER)
  DECLARE_TUPLE(tuple, x0);
  Cell *closedCell = Cell::FromWord(tuple->Sel(0));
  if (Store::WordToInt(closedCell->Access()) == 0) {
    Cell *handleCell = Cell::FromWord(tuple->Sel(2));
    HANDLE handle = Store::WordToUnmanagedPointer(handleCell->Access());
    closedCell->Assign(Store::IntToWord(1));
    if (FindClose(handle) == FALSE) RAISE_SYS_ERR();
  }
  RETURN_UNIT;
#else
  DECLARE_LINUXDIR(ld, x0);
  
  if (!ld->IsNull()) {
    DIR *d = ld->GetValue();
    if (closedir(d) == 0)
      ld->SetNull();
    else
      RAISE_SYS_ERR();
  }
  
  RETURN_UNIT;
#endif
} END

DEFINE1(UnsafeOS_FileSys_chDir) {
  DECLARE_STRING(name, x0);
#if defined(__MINGW32__) || defined(_MSC_VER)
  if (SetCurrentDirectory(name->ExportC()) == FALSE) RAISE_SYS_ERR();
#else
  Interruptible(res, chdir(name->ExportC()));
  if (res) RAISE_SYS_ERR();
#endif
  RETURN_UNIT;
} END

DEFINE0(UnsafeOS_FileSys_getDir) {
  String *buffer = String::FromWordDirect(wBufferString);
  u_int size = buffer->GetSize();
 retry:
  {
    char *buf = reinterpret_cast<char *>(buffer->GetValue());
#if defined(__MINGW32__) || defined(_MSC_VER)
    u_int n = GetCurrentDirectory(size, (CHAR *) buf);
    if (n == 0) RAISE_SYS_ERR();
    if (n > size) {
      size = n + 1;
      buffer = String::New(size);
      wBufferString = buffer->ToWord();
      goto retry;
    }
    // make canonical
    buf[0] = tolower(buf[0]);
    for (n--; n>0; n--)
      if (buf[n] == '\\') buf[n] = '/';
#else
    if (getcwd(buf, size) == NULL) {
      if (errno != ERANGE) RAISE_SYS_ERR();
      size = size * 3 / 2;
      buffer = String::New(size);
      wBufferString = buffer->ToWord();
      goto retry;
    }
#endif
    RETURN(String::New(buf)->ToWord());
  }
} END

DEFINE1(UnsafeOS_FileSys_mkDir) {
  DECLARE_STRING(name, x0);
#if defined(__MINGW32__) || defined(_MSC_VER)
  if (CreateDirectory(name->ExportC(), NULL) == FALSE) RAISE_SYS_ERR();
#else
  int res = mkdir(name->ExportC(), S_IRWXU | S_IRWXG | S_IRWXO);
  if (res) RAISE_SYS_ERR();
#endif
  RETURN_UNIT;
} END

DEFINE1(UnsafeOS_FileSys_rmDir) {
  DECLARE_STRING(name, x0);
#if defined(__MINGW32__) || defined(_MSC_VER)
  if (RemoveDirectory(name->ExportC()) == FALSE) RAISE_SYS_ERR();
#else
  if (rmdir(name->ExportC())) RAISE_SYS_ERR();
#endif
  RETURN_UNIT;
} END

DEFINE1(UnsafeOS_FileSys_isDir) {
  DECLARE_STRING(name, x0);
#if defined(__MINGW32__) || defined(_MSC_VER)
  DWORD attr = GetFileAttributes(name->ExportC());
  if (attr == INVALID_FILE_ATTRIBUTES) RAISE_SYS_ERR();
  RETURN_BOOL(attr & FILE_ATTRIBUTE_DIRECTORY);
#else
  struct stat info;
  Interruptible(res, stat(name->ExportC(), &info));
  if (res) RAISE_SYS_ERR();
  RETURN_BOOL(S_ISDIR(info.st_mode));
#endif
} END

DEFINE1(UnsafeOS_FileSys_isLink) {
  DECLARE_STRING(name, x0);
#if defined(__MINGW32__) || defined(_MSC_VER)
  DWORD attr = GetFileAttributes(name->ExportC());
  if (attr == INVALID_FILE_ATTRIBUTES) RAISE_SYS_ERR();
  RETURN_BOOL(false); // no support for symbolic links
#else
  struct stat info;
  Interruptible(res, lstat(name->ExportC(), &info));
  if (res) RAISE_SYS_ERR();
  RETURN_BOOL(S_ISLNK(info.st_mode));
#endif
} END

DEFINE1(UnsafeOS_FileSys_readLink) {
  DECLARE_STRING(name, x0);
#if defined(__MINGW32__) || defined(_MSC_VER)
  // raise SysErr unconditionally
  name = name;
  ConVal *sysErr =
    ConVal::New(Store::DirectWordToBlock(SysErrConstructor), 2);
  sysErr->Init(0, String::New("symbolic links not supported")->ToWord());
  sysErr->Init(1, Store::IntToWord(Types::NONE));
  RAISE(sysErr->ToWord());
#else
  String *buffer = String::FromWordDirect(wBufferString);
  u_int size = buffer->GetSize();
 retry:
  s_int res = readlink(name->ExportC(), reinterpret_cast<char *>(buffer->GetValue()), size);
  if (res < 0) RAISE_SYS_ERR();
  if (static_cast<u_int>(res) == size) {
    size = size * 3 / 2;
    buffer = String::New(size);
    wBufferString = buffer->ToWord();
    goto retry;
  }
  RETURN(String::New(reinterpret_cast<char *>(buffer->GetValue()), res)->ToWord());
#endif
} END

DEFINE1(UnsafeOS_FileSys_fileSize) {
  DECLARE_STRING(name, x0);
  //--** truncates the file size if not representable
#if defined(__MINGW32__) || defined(_MSC_VER)
  HANDLE hFile =
    CreateFile(
	  name->ExportC(),
	  0,
	  FILE_SHARE_READ | FILE_SHARE_WRITE,
	  NULL,
	  OPEN_EXISTING,
	  FILE_ATTRIBUTE_NORMAL | FILE_FLAG_BACKUP_SEMANTICS,
	  NULL
	);
  if (hFile == INVALID_HANDLE_VALUE) RAISE_SYS_ERR();
  DWORD n = GetFileSize(hFile, NULL);
  CloseHandle(hFile);
  if (n == INVALID_FILE_SIZE) RAISE_SYS_ERR();
  RETURN_INT(n);
#else
  struct stat info;
  Interruptible(res, stat(name->ExportC(), &info));
  if (res) RAISE_SYS_ERR();
  RETURN_INT(info.st_size);
#endif
} END

DEFINE1(UnsafeOS_FileSys_modTime) {
  DECLARE_STRING(name, x0);
#if defined(__MINGW32__) || defined(_MSC_VER)
  HANDLE hFile =
    CreateFile(
	  name->ExportC(),
	  0,
	  FILE_SHARE_READ | FILE_SHARE_WRITE,
	  NULL,
	  OPEN_EXISTING,
	  FILE_ATTRIBUTE_NORMAL | FILE_FLAG_BACKUP_SEMANTICS,
	  NULL
	);
  if (hFile == INVALID_HANDLE_VALUE) RAISE_SYS_ERR();
  FILETIME fileTime;
  BOOL success = GetFileTime(hFile, NULL, NULL, &fileTime);
  CloseHandle(hFile);
  if (success == FALSE) RAISE_SYS_ERR();
  
  BigInt *b = BigInt::New((unsigned int)fileTime.dwHighDateTime);
  mpz_mul_2exp(b->big(), b->big(), 32);
  mpz_add_ui(b->big(), b->big(), fileTime.dwLowDateTime);
  mpz_fdiv_q_ui(b->big(), b->big(), 10000);
  
  // convert microsoft timestamp into unix timestamp
  mpz_t offset;
  mpz_init(offset);
  mpz_set_str(offset, "11644473600000", 10);
  mpz_sub(b->big(), b->big(), offset);
  mpz_clear(offset);
  
  RETURN_INTINF(b);
#else
  struct stat info;
  Interruptible(res, stat(name->ExportC(), &info));
  if (res) RAISE_SYS_ERR();
  BigInt *b = BigInt::New(static_cast<u_int>(info.st_mtime));
  mpz_mul_ui(b->big(), b->big(), 1000);
  RETURN_INTINF(b);
#endif
} END

DEFINE1(UnsafeOS_FileSys_remove) {
  DECLARE_STRING(name, x0);
#if defined(__MINGW32__) || defined(_MSC_VER)
  if (DeleteFile(name->ExportC()) == FALSE) RAISE_SYS_ERR();
#else
  Interruptible(res, unlink(name->ExportC()));
  if (res) RAISE_SYS_ERR();
#endif
  RETURN_UNIT;
} END

DEFINE0(UnsafeOS_FileSys_tmpName) {
#if defined(__MINGW32__) || defined(_MSC_VER)
  String *buffer = String::FromWordDirect(wBufferString);
  u_int size = buffer->GetSize();
 retry:
  char *buf = (char *) buffer->GetValue();
  DWORD res = GetTempPath(size, buf);
  if (res == 0) RAISE_SYS_ERR();
  if (res > size) {
    size = size * 3 / 2;
    buffer = String::New(size);
    wBufferString = buffer->ToWord();
    goto retry;
  }
  String *name = String::New(res + 10);
  // make canonical
  buf[0] = tolower(buf[0]);
  for (res--; res>0; res--)
    if (buf[res] == '\\') buf[res] = '/';
  char *s = (char *) name->GetValue();
  static int counter = 0;
  while (true) {
    std::sprintf(s, "%salice%d", buf, counter);
    counter = (counter++) % 10000;
    if (GetFileAttributes(s) == INVALID_FILE_ATTRIBUTES)
      break;
  }
  RETURN(name->ToWord());
#else
  static const char path[] = "/tmp/aliceXXXXXX";
  String *s = String::New(path, static_cast<u_int>(sizeof(path)));
  if (mkstemp(reinterpret_cast<char *>(s->GetValue())) == -1) {
    RAISE_SYS_ERR();
  }
  RETURN(s->ToWord());
#endif
} END

DEFINE0(UnsafeOS_FileSys_getHomeDir) {
  String *buffer = String::FromWordDirect(wBufferString);
  u_int size = buffer->GetSize();
#if defined(__MINGW32__) || defined(_MSC_VER)
  ITEMIDLIST* pidl;
  HRESULT hRes = SHGetSpecialFolderLocation( NULL, CSIDL_PERSONAL, &pidl );
  if (hRes==NOERROR) {
    SHGetPathFromIDList( pidl, (CHAR *) buffer->GetValue());
  } else {
    RAISE_SYS_ERR();
  }

  IMalloc* palloc; 
  hRes = SHGetMalloc(&palloc); 
  palloc->Free( (void*)pidl ); 
  palloc->Release();
  char *buf = (char *) buffer->GetValue();
  // make canonical
  buf[0] = tolower(buf[0]);
  for (char *p = buf; *p; p++)
    if (*p == '\\') *p = '/';
  RETURN(String::New(buf)->ToWord());
#else
  char *envVal = std::getenv("HOME");

  if (envVal==NULL) {
  retry:
    if (getcwd(reinterpret_cast<char *>(buffer->GetValue()), size) == NULL) {
      if (errno != ERANGE) RAISE_SYS_ERR();
      size = size * 3 / 2;
      buffer = String::New(size);
      wBufferString = buffer->ToWord();
      goto retry;
    }
    RETURN(String::New(reinterpret_cast<char *>(buffer->GetValue()))->ToWord());
  }
  RETURN(String::New(envVal)->ToWord());
#endif
} END

DEFINE0(UnsafeOS_FileSys_getApplicationConfigDir) {
  String *buffer = String::FromWordDirect(wBufferString);
  char *buf = reinterpret_cast<char *>(buffer->GetValue());
  u_int size = buffer->GetSize();
#if defined(__MINGW32__) || defined(_MSC_VER)
  ITEMIDLIST* pidl;
  HRESULT hRes = SHGetSpecialFolderLocation( NULL, CSIDL_APPDATA, &pidl );
  if (hRes==NOERROR) {
    SHGetPathFromIDList( pidl, (CHAR *) buf);
  } else {
    RAISE_SYS_ERR();
  }

  IMalloc* palloc; 
  hRes = SHGetMalloc(&palloc); 
  palloc->Free( (void*)pidl ); 
  palloc->Release();
  // make canonical
  buf[0] = tolower(buf[0]);
  for (char *p = buf; *p; p++)
    if (*p == '\\') *p = '/';
  strcat(buf, "/Alice");
  RETURN(String::New(buf)->ToWord());
#else
  static const char *const alice = "/.alice";
  static const int pluslen = strlen(alice);
  char *envVal = std::getenv("HOME");

  if (envVal==NULL) {
  retry:
    if (getcwd(buf, size-pluslen) == NULL) {
      if (errno != ERANGE) RAISE_SYS_ERR();
      size = size * 3 / 2;
      buffer = String::New(size);
      wBufferString = buffer->ToWord();
      goto retry;
    }
  } else {
    u_int len = strlen(envVal);
    if (len + pluslen >= size) {
      size = len + pluslen + 1;
      buffer = String::New(size);
      buf = reinterpret_cast<char *>(buffer->GetValue());
      wBufferString = buffer->ToWord();
    }
    strcpy(buf, envVal);
  }
  strcat(buf, alice);
  RETURN(String::New(buf)->ToWord());
#endif
} END

static word UnsafeOS_FileSys() {
  Record *record = Record::New(17);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "chDir",
		 UnsafeOS_FileSys_chDir, 1);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "getDir",
		 UnsafeOS_FileSys_getDir, 0);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "mkDir",
		 UnsafeOS_FileSys_mkDir, 1);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "rmDir",
		 UnsafeOS_FileSys_rmDir, 1);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "isDir",
		 UnsafeOS_FileSys_isDir, 1);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "isLink",
		 UnsafeOS_FileSys_isLink, 1);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "readLink",
		 UnsafeOS_FileSys_readLink, 1);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "fileSize",
		 UnsafeOS_FileSys_fileSize, 1);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "modTime",
		 UnsafeOS_FileSys_modTime, 1);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "remove",
		 UnsafeOS_FileSys_remove, 1);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "tmpName",
         UnsafeOS_FileSys_tmpName, 0);
  INIT_STRUCTURE(record, "UnsafeOS_FileSys", "openDir",
		 UnsafeOS_FileSys_openDir, 1);
  INIT_STRUCTURE(record, "UnsafeOS_FileSys", "readDir",
		 UnsafeOS_FileSys_readDir, 1);
  INIT_STRUCTURE(record, "UnsafeOS_FileSys", "rewindDir",
		 UnsafeOS_FileSys_rewindDir, 1);
  INIT_STRUCTURE(record, "UnsafeOS_FileSys", "closeDir",
		 UnsafeOS_FileSys_closeDir, 1);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "getHomeDir",
		 UnsafeOS_FileSys_getHomeDir, 0);
  INIT_STRUCTURE(record, "UnsafeOS.FileSys", "getApplicationConfigDir",
		 UnsafeOS_FileSys_getApplicationConfigDir, 0);
  return record->ToWord();
}

//
// UnsafeOS.Process Structure
//

DEFINE1(UnsafeOS_Process_system) {
  //--** Windows: NT/2000? see Mozart implementation
  //--** Unix: interruptibility? see Mozart implementation
  DECLARE_STRING(command, x0);
  RETURN_INT(system(command->ExportC()));
} END

DEFINE1(UnsafeOS_Process_terminate) {
  DECLARE_INT(code, x0);
#if PROFILE
  Profiler::DumpInfo();
#endif
#if defined(JIT_APPLY_STATISTIC)
  extern void DumpApplyStatistics();
  DumpApplyStatistics();
#endif
#if HAVE_LIGHTNING && defined(INSTRUCTION_COUNTS)
  NativeCodeJitter::DumpInstructionCounts();
#endif
  EXIT(code);
} END

DEFINE1(UnsafeOS_Process_atExn) {
  DECLARE_CLOSURE(closure, x0);
  TaskStack::AddExnClosure(closure->ToWord());
  RETURN_UNIT;
} END

#if defined(__MINGW32__) || defined(_MSC_VER)
// Platform SDK: 32767 maximum size excluding zero
#define MAX_ENV_VALUE_SIZE 32768
static char envValBuf[MAX_ENV_VALUE_SIZE];
#endif

DEFINE1(UnsafeOS_Process_getEnv) {
  DECLARE_STRING(envVar, x0);
  char *envVal;
#if defined(__MINGW32__) || defined(_MSC_VER)
  if (!GetEnvironmentVariable(envVar->ExportC(), envValBuf, MAX_ENV_VALUE_SIZE))
    envVal = NULL;
  else
    envVal = envValBuf;
#else
  envVal = std::getenv(envVar->ExportC());
#endif
  if (envVal != NULL) {
    TagVal *val = TagVal::New(Types::SOME, 1);
    val->Init(0, String::New(envVal)->ToWord());
    RETURN(val->ToWord());
  } else
    RETURN(Store::IntToWord(Types::NONE));
} END

static word UnsafeOS_Process() {
  Record *record = Record::New(6);
  record->Init("success", Store::IntToWord(0));
  record->Init("failure", Store::IntToWord(1));
  INIT_STRUCTURE(record, "UnsafeOS.Process", "system",
		 UnsafeOS_Process_system, 1);
  INIT_STRUCTURE(record, "UnsafeOS.Process", "terminate",
		 UnsafeOS_Process_terminate, 1);
  INIT_STRUCTURE(record, "UnsafeOS.Process", "atExn",
		 UnsafeOS_Process_atExn, 1);
  INIT_STRUCTURE(record, "UnsafeOS.Process", "getEnv",
		 UnsafeOS_Process_getEnv, 1);
  return record->ToWord();
}

//
// UnsafeOS Structure
//

DEFINE2(UnsafeOS_SysErr) {
  Block *ccVal = Store::DirectWordToBlock(SysErrConstructor);
  ConVal *conVal = ConVal::New(ccVal, 2);
  conVal->Init(0, x0);
  conVal->Init(1, x1);
  RETURN(conVal->ToWord());
} END

DEFINE1(UnsafeOS_errorMsg) {
  DECLARE_INT(errorCode, x0);
  RETURN(ErrorCodeToString(static_cast<int>(errorCode))->ToWord());
} END

AliceDll word UnsafeOS() {
  SysErrConstructor =
    UniqueConstructor::New("SysErr", "OS.SysErr")->ToWord();
  RootSet::Add(SysErrConstructor);
#if defined(__MINGW32__) || defined(_MSC_VER)
  wBufferString = String::New(MAX_PATH+20)->ToWord();
#else
  wBufferString = String::New(1024)->ToWord();
#endif
  RootSet::Add(wBufferString);
  dirFinalizationSet = new DirFinalizationSet();

  Record *record = Record::New(5);
  record->Init("'SysErr", SysErrConstructor);
  INIT_STRUCTURE(record, "UnsafeOS", "SysErr",
		 UnsafeOS_SysErr, 2);
  INIT_STRUCTURE(record, "UnsafeOS", "errorMsg",
		 UnsafeOS_errorMsg, 1);
  record->Init("FileSys$", UnsafeOS_FileSys());
  record->Init("Process$", UnsafeOS_Process());
  RETURN_STRUCTURE("UnsafeOS$", record);
}
