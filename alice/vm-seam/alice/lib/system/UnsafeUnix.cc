//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstaedt@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2004
//   Leif Kornstaedt, 2004
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstdio>
#if defined(__MINGW32__) || defined(_MSC_VER)
#include <windows.h>
#include <winsock.h>
#else
#include <unistd.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/wait.h>

#define ioctlsocket ioctl
#define GetLastError() errno
#define WSAGetLastError() errno
#define Interruptible(res, call)		\
  int res;					\
  do {						\
    res = call;					\
  } while (res < 0 && GetLastError() == EINTR);

#endif

#include "alice/Authoring.hh"

// Defined in UnsafeOS.cc
extern word SysErrConstructor;
#include "SysErr.icc"

DEFINE2(UnsafeUnix_execute) {
  DECLARE_STRING(prog, x0);
  word args = x1;
  TagVal *tagVal;
  // Request list and its content
  u_int nbArgs = 0;
  while ((tagVal = TagVal::FromWord(args)) != INVALID_POINTER) {
    Assert(tagVal->GetTag() == Types::cons);
    DECLARE_STRING(arg, tagVal->Sel(0));
    arg = arg; // Ignored
    nbArgs++;
    args = tagVal->Sel(1);
  }
  if (Store::WordToInt(args) == INVALID_INT)
    REQUEST(args);
  Assert(Store::WordToInt(args) == Types::nil);
  // Create the pipes and its associated process
  word pHandle;
  IODesc *reader, *writer;
#if defined(__MINGW32__) || defined(_MSC_VER)
  HANDLE stdinRd, stdinWr, stdoutRd, stdoutWr, stderrWr;
  HANDLE stdinWrDup, stdoutRdDup;
  // Create pipes
  SECURITY_ATTRIBUTES sa1;
  sa1.nLength              = sizeof(sa1);
  sa1.lpSecurityDescriptor = NULL;
  sa1.bInheritHandle       = TRUE;
  SECURITY_ATTRIBUTES sa2;
  sa2.nLength              = sizeof(sa2);
  sa2.lpSecurityDescriptor = NULL;
  sa2.bInheritHandle       = TRUE;
  if (!CreatePipe(&stdinRd, &stdinWr, &sa1, 0)  ||
      !CreatePipe(&stdoutRd, &stdoutWr, &sa2, 0)) {
    RAISE_SYS_ERR();
  }
  // The child must only inherit one side of each pipe.
  // Else the inherited handle will cause the pipe to remain open
  // even though we may have closed it, resulting in the child
  // never getting an EOF on it.
  if (!DuplicateHandle(GetCurrentProcess(), stdinWr,
		       GetCurrentProcess(), &stdinWrDup, 0,
		       FALSE, DUPLICATE_SAME_ACCESS) ||
      !DuplicateHandle(GetCurrentProcess(), stdoutRd,
		       GetCurrentProcess(), &stdoutRdDup, 0,
		       FALSE, DUPLICATE_SAME_ACCESS)) {
    RAISE_SYS_ERR();
  }
  CloseHandle(stdinWr);
  CloseHandle(stdoutRd);
  // We need to duplicate the handle in case the child closes
  // either its output or its error handle.
  if (!DuplicateHandle(GetCurrentProcess(), stdoutWr,
		       GetCurrentProcess(), &stderrWr, 0,
		       TRUE, DUPLICATE_SAME_ACCESS)) {
    RAISE_SYS_ERR();
  }
  // Create the child process
  STARTUPINFO si;
  ZeroMemory(&si, sizeof(si));
  si.cb         = sizeof(si);
  si.dwFlags    = STARTF_FORCEOFFFEEDBACK | STARTF_USESTDHANDLES;
  si.hStdInput  = stdinRd;
  si.hStdOutput = stdoutWr;
  si.hStdError  = stderrWr;
  PROCESS_INFORMATION pinf;
  ZeroMemory(&pinf, sizeof(pinf));
  // Build the command string
  static char cmdLine[4096];
  strcpy(cmdLine, "\"");
  strcat(cmdLine, prog->ExportC());
  strcat(cmdLine, "\"");
  args = x1;
  while ((tagVal = TagVal::FromWord(args)) != INVALID_POINTER) {
    strcat(cmdLine, " \"");
    strcat(cmdLine, String::FromWord(tagVal->Sel(0))->ExportC());
    strcat(cmdLine, "\"");
    args = tagVal->Sel(1);
  }
  if (!CreateProcess(NULL, cmdLine,
		     NULL, NULL, TRUE, 0, NULL, NULL, &si, &pinf)) {
    RAISE_SYS_ERR();
  }
  
  //  int pid = pinf.dwProcessId;
  // Relase no longer needed handle resources
  CloseHandle(stdinRd);
  CloseHandle(stdoutWr);
  CloseHandle(stderrWr);
  CloseHandle(pinf.hThread);
  // Wrap the handles
  reader =
    IODesc::NewForwarded(IODesc::DIR_READER, String::New("reader"), stdoutRdDup);
  writer =
    IODesc::NewForwarded(IODesc::DIR_WRITER, String::New("writer"), stdinWrDup);

  Chunk *tmpHandle = Store::AllocChunk(sizeof(HANDLE));
  HANDLE *tmpHandlePtr = reinterpret_cast<HANDLE*>(tmpHandle->GetBase());
  tmpHandlePtr[0] = pinf.hProcess;
  pHandle = tmpHandle->ToWord();
#else
  int sv[2];
  Interruptible(ret, socketpair(PF_UNIX, SOCK_STREAM, 0, sv));
  if (ret < 0) {
    RAISE_SOCK_ERR();
  }
  // Populate arguments array
  char *argArr[nbArgs];
  argArr[0] = prog->ExportC();
  args = x1;
  u_int i = 1;
  while ((tagVal = TagVal::FromWord(args)) != INVALID_POINTER) {
    argArr[i++] = String::FromWord(tagVal->Sel(0))->ExportC();
    args = tagVal->Sel(1);
  }
  argArr[i] = NULL;
  int pid = fork();
  switch (pid) {
  case 0: // Child process
    {
      // Do not produce core files
      struct rlimit rlim;
      rlim.rlim_cur = 0;
      rlim.rlim_max = 0;
      if (setrlimit(RLIMIT_CORE, &rlim) < 0) {
	fprintf(stderr, "setrlimit failed\n");
	exit(-1);
      }
      // Some stdIn/stdOut stuff
      for (int i = 0; i < FD_SETSIZE; i++)
	if (i != sv[1])
	  close(i);
      dup(sv[1]);
      dup(sv[1]);
      dup(sv[1]);
      // Execute command
      if (execvp(argArr[0], argArr) < 0) {
	fprintf(stderr, "execvp failed\n");
	exit(-1);
      }
    }
    break;
  case -1:
    {
      RAISE_SYS_ERR();
    }
    break;
  default: // Parent process
    break;
  }
  close(sv[1]);
  unsigned long arg = true;
  ioctlsocket(sv[0], FIONBIO, &arg);
  // We can use the same socket fd for both reading and writing
  reader = IODesc::NewFromFD(IODesc::DIR_READER, String::New("reader"), sv[0]);
  writer = IODesc::NewFromFD(IODesc::DIR_WRITER, String::New("writer"), sv[0]);
  pHandle = Store::IntToWord(pid);
#endif
  RETURN3(reader->ToWord(), writer->ToWord(), pHandle);
} END


DEFINE1(UnsafeUnix_waitnh) {
  word option;
#if defined(__MINGW32__) || defined(_MSC_VER)
  Chunk *tmpHandle = Store::DirectWordToChunk(x0);
  HANDLE *tmpHandlePtr = reinterpret_cast<HANDLE*>(tmpHandle->GetBase());
  HANDLE hProcess = tmpHandlePtr[0];
  unsigned int exitCode;
  if(GetExitCodeProcess(hProcess, reinterpret_cast<LPDWORD>(&exitCode))) {
    if(exitCode == STILL_ACTIVE) {
      option = Store::IntToWord(Types::NONE);
    } else {
      TagVal *some = TagVal::New(Types::SOME, 1);
      some->Init(0, Store::IntToWord(exitCode));
      option = some->ToWord();
      CloseHandle(hProcess);
    }
  } else { // error
    fprintf(stderr, "GetExitCodeProcess failed\n");
    exit(-1);
  }
#else
  DECLARE_INT(pid, x0);
  int status;
  s_int ret = waitpid(static_cast<pid_t>(pid), &status, WNOHANG);
  if(ret == 0) { // process is still alive
    option = Store::IntToWord(Types::NONE);
  } else if(ret == pid) { // process is dead
    TagVal *some = TagVal::New(Types::SOME,1);
    // check why the process is dead
    if(WIFEXITED(status)) {
      some->Init(0, Store::IntToWord(WEXITSTATUS(status)));
      option = some->ToWord();
    } else if(WIFSIGNALED(status)) {
      some->Init(0, Store::IntToWord(256 + WTERMSIG(status)));
      option = some->ToWord();
    } else  {            
      option = Store::IntToWord(Types::NONE);
    }
  } else { // error
    fprintf(stderr, "waitpid failed\n");
    exit(-1);
  }
#endif
  RETURN(option);
} END

DEFINE1(UnsafeUnix_streamsOf) {
  DECLARE_TUPLE(triple, x0);
  RETURN2(triple->Sel(0), triple->Sel(1));
} END

DEFINE1(UnsafeUnix_pHandleOf) {
  DECLARE_TUPLE(triple, x0);
  RETURN(triple->Sel(2));
} END

AliceDll word UnsafeUnix() {
  Record *record = Record::New(4);
  INIT_STRUCTURE_N(record, "UnsafeUnix", "execute",
		   UnsafeUnix_execute, 2, 3);
  INIT_STRUCTURE_N(record, "UnsafeUnix", "wait'",
		   UnsafeUnix_waitnh, 1, 1);
  INIT_STRUCTURE_N(record, "UnsafeUnix", "streamsOf",
		   UnsafeUnix_streamsOf, 1, 2);
  INIT_STRUCTURE_N(record, "UnsafeUnix", "pHandleOf",
		   UnsafeUnix_pHandleOf, 1, 1);
  RETURN_STRUCTURE("UnsafeUnix$", record);
}
