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
#else
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>

#define WSAGetLastError() errno
#define Interruptible(res, call)		\
  int res;					\
  do {						\
    res = call;					\
  } while (res < 0 && WSAGetLastError() == EINTR);

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
  while ((tagVal = TagVal::FromWord(args)) != INVALID_POINTER) {
    Assert(tagVal->GetTag() == Types::cons);
    DECLARE_STRING(arg, tagVal->Sel(0));
    arg = arg; // Ignored
    args = tagVal->Sel(1);
  }
  if (Store::WordToInt(args) == INVALID_INT)
    REQUEST(args);
  Assert(Store::WordToInt(args) == Types::nil);
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
  // Create the pipes and its associated process
  Tuple *streams = Tuple::New(2);
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
  if (!CreateProcess(NULL, cmdLine,
		     NULL, NULL, TRUE, 0, NULL, NULL, &si, &pinf)) {
    RAISE_SYS_ERR();
  }
  int pid = pinf.dwProcessId;
  // Relase no longer needed handle resources
  CloseHandle(stdinRd);
  CloseHandle(stdoutWr);
  CloseHandle(stderrWr);
  CloseHandle(pinf.hProcess); //--** this is unsafe! keep open while pid used
  CloseHandle(pinf.hThread);
  // Wrap the handles
  reader =
    IODesc::NewForwarded(IODesc::DIR_READER, String::New("reader"), stdoutRdDup);
  writer =
    IODesc::NewForwarded(IODesc::DIR_WRITER, String::New("writer"), stdinWrDup);
#else
  int sv[2];
  Interruptible(ret, socketpair(PF_UNIX, SOCK_STREAM, 0, sv));
  if (ret < 0) {
    RAISE_SOCK_ERR();
  }
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
      if (execvp(prog, argArr) < 0) {
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
  // We can use the same socket fd for both reading and writing
  reader = IODesc::NewFromFD(IODesc::DIR_READER, String::New("reader"), sv[0]);
  writer = IODesc::NewFromFD(IODesc::DIR_WRITER, String::New("writer"), sv[0]);
#endif
  streams->Init(0, reader->ToWord());
  streams->Init(1, writer->ToWord());
  RETURN(streams->ToWord());
} END

DEFINE1(UnsafeUnix_streamsOf) {
  DECLARE_TUPLE(streams, x0);
  RETURN2(streams->Sel(0), streams->Sel(1));
} END

AliceDll word UnsafeUnix() {
  Record *record = Record::New(2);
  INIT_STRUCTURE_N(record, "UnsafeUnix", "execute",
		   UnsafeUnix_execute, 2, 1);
  INIT_STRUCTURE_N(record, "UnsafeUnix", "streamsOf",
		   UnsafeUnix_streamsOf, 1, 2);
  RETURN_STRUCTURE("UnsafeUnix$", record);
}
