//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "generic/IODesc.hh"
#endif

#if defined(__MINGW32__) || defined(_MSC_VER)
#include <windows.h>
#include <winsock.h>
#define GetLastSocketError() WSAGetLastError()
#define Interruptible(res, call) int res = call; res = res;
#define closesocket(s) closesocket(s)
#else
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#define GetLastSocketError() errno
#define GetLastError() errno
#define Interruptible(res, call)			\
  int res;						\
  do {							\
    res = call;						\
  } while (res < 0 && GetLastSocketError() == EINTR);
#define closesocket(s) close(s)
#endif

#include "generic/String.hh"
#include "generic/IOHandler.hh"
#include "generic/IODesc.hh"

//
// Windows: Forwarding between file handles and sockets
//
#if defined(__MINGW32__) || defined(_MSC_VER)
class IOSupport {
private:
  class InOut {
  public:
    SOCKET fd1;
    HANDLE fd2;
    InOut(SOCKET f1, HANDLE f2): fd1(f1), fd2(f2) {}
  };

  static const u_int BUFFER_SIZE = 1024;
  static DWORD __stdcall ReaderThread(void *p);
  static DWORD __stdcall WriterThread(void *p);
public:
  static int SocketPair(int, int type, int, int *sb) {
    int res = -1;
    SOCKET insock, outsock, newsock;
    struct sockaddr_in sock_in;
    int len = sizeof (sock_in);

    newsock = socket(AF_INET, type, 0);
    if (newsock == INVALID_SOCKET) {
      goto done;
    }

    /* bind the socket to any unused port */
    sock_in.sin_family = AF_INET;
    sock_in.sin_port = 0;
    sock_in.sin_addr.s_addr = INADDR_ANY;
    if (bind(newsock, (struct sockaddr *) &sock_in, sizeof (sock_in)) < 0) {
      goto done;
    }

    if (getsockname(newsock, (struct sockaddr *) &sock_in, &len) < 0) {
      closesocket(newsock);
      goto done;
    }
    listen(newsock, 2);

    /* create a connecting socket */
    outsock = socket(AF_INET, type, 0);
    if (outsock == INVALID_SOCKET) {
      closesocket(newsock);
      goto done;
    }
    sock_in.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

    /* Do a connect and accept the connection */
    if (connect(outsock, (struct sockaddr *) &sock_in, sizeof (sock_in)) < 0) {
      closesocket(newsock);
      closesocket(outsock);
      goto done;
    }
    insock = accept(newsock, (struct sockaddr *) &sock_in, &len);
    if (insock == INVALID_SOCKET) {
      closesocket(newsock);
      closesocket(outsock);
      goto done;
    }

    closesocket(newsock);
    res = 0;

    sb[0] = insock;
    sb[1] = outsock;
  done:
    return res;
  }
  static void CreateReader(SOCKET s, HANDLE h) {
    DWORD threadId;
    HANDLE hThread =
      CreateThread(NULL, 1024, &ReaderThread, new InOut(s, h), 0, &threadId);
    CloseHandle(hThread);
  }
  static void CreateWriter(SOCKET s, HANDLE h) {
    DWORD threadId;
    HANDLE hThread =
      CreateThread(NULL, 1024, &WriterThread, new InOut(s, h), 0, &threadId);
    CloseHandle(hThread);
  }
};

DWORD __stdcall IOSupport::ReaderThread(void *p) {
  InOut *io = (InOut*) p;
  SOCKET out = io->fd1;
  HANDLE in = io->fd2;
  delete io;

  // This one solves a problem with W2K SP2.
  // ReaderThread cause the system to freeze if we
  // don't call gethostname() (?load ws2_32.dll? changed with SP2)
  // before ReadFile().
  char dummyBuf[1024];
  gethostname(dummyBuf, sizeof(dummyBuf));

  char buf[BUFFER_SIZE];
  while (true) {
    DWORD count;
    if (ReadFile(in, buf, BUFFER_SIZE, &count, NULL) == FALSE) {
      if (GetLastError() != ERROR_BROKEN_PIPE)
	std::fprintf(stderr, "ReadFile failed: %ld\n", GetLastError());
      break;
    }
    if (count == 0)
      break;
    u_int totalSent = 0;
  loop:
    Interruptible(sent, send(out, &buf[totalSent], count, 0));
    if (sent == SOCKET_ERROR) {
      std::fprintf(stderr, "send(%d) failed: %d\n", out, GetLastSocketError());
      break;
    }
    count -= sent;
    totalSent += sent;
    if (count > 0)
      goto loop;
  }
  CloseHandle(in);
  closesocket(out);
  return 0;
}

DWORD __stdcall IOSupport::WriterThread(void *p) {
  InOut *io = (InOut*) p;
  SOCKET in = io->fd1;
  HANDLE out = io->fd2;
  delete io;

  char buf[BUFFER_SIZE];
  while (true) {
    Interruptible(got, recv(in, buf, BUFFER_SIZE, 0));
    if (got == SOCKET_ERROR) {
      std::fprintf(stderr, "recv(%d) failed: %d\n", in, GetLastSocketError());
      break;
    }
    if (got == 0)
      break;
    u_int totalWritten = 0;
  loop:
    DWORD count;
    if (WriteFile(out, &buf[totalWritten], got, &count, 0) == FALSE) {
      std::fprintf(stderr, "WriteFile failed: %ld\n", GetLastError());
      break;
    }
    totalWritten += count;
    got -= count;
    if (got > 0)
      goto loop;
  }
  closesocket(in);
  CloseHandle(out);
  return 0;
}
#endif

//
// IODescFinalizationSet Implementation
//
void IODescFinalizationSet::Finalize(word value) {
  IODesc::FromWordDirect(value)->Close();
}

//
// IODesc Implementation
//
IODescFinalizationSet *IODesc::finalizationSet;

void IODesc::Init() {
  finalizationSet = new IODescFinalizationSet();
}

IODesc *IODesc::NewClosed(String *name) {
  Block *p = Store::AllocBlock(IODESC_LABEL, SIZE);
  p->InitArg(FLAGS_POS, TYPE_CLOSED);
  p->InitArg(NAME_POS, name->ToWord());
  return static_cast<IODesc *>(p);
}

IODesc *IODesc::NewFromFD(u_int dir, String *name, int fd) {
  Block *p = Store::AllocBlock(IODESC_LABEL, SIZE);
  p->InitArg(FLAGS_POS, TYPE_FD|dir);
  p->InitArg(NAME_POS, name->ToWord());
  p->InitArg(FD_POS, fd);
  p->InitArg(FINALIZATION_KEY_POS, finalizationSet->Register(p->ToWord()));
  return static_cast<IODesc *>(p);
}

#if defined(__MINGW32__) || defined(_MSC_VER)
IODesc *IODesc::NewFromHandle(u_int dir, String *name, HANDLE handle) {
  Block *p = Store::AllocBlock(IODESC_LABEL, SIZE);
  p->InitArg(FLAGS_POS, TYPE_HANDLE|dir);
  p->InitArg(NAME_POS, name->ToWord());
  p->InitArg(HANDLE_POS, Store::UnmanagedPointerToWord(handle));
  p->InitArg(FINALIZATION_KEY_POS, finalizationSet->Register(p->ToWord()));
  return static_cast<IODesc *>(p);
}

IODesc *IODesc::NewForwarded(u_int dir, String *name, HANDLE handle) {
  int fd;
  // Forward from the handle to a socket, so that we can use select():
  int sv[2];
  if (IOSupport::SocketPair(PF_UNIX, SOCK_STREAM, 0, sv) == -1)
    Error("socket pair failed");
  switch (dir) {
  case DIR_READER:
    IOSupport::CreateReader(sv[0], handle);
    fd = sv[1];
    break;
  case DIR_WRITER:
    IOSupport::CreateWriter(sv[1], handle);
    fd = sv[0];
    break;
  default:
    Error("invalid direction");
  }
  Block *p = Store::AllocBlock(IODESC_LABEL, SIZE);
  p->InitArg(FLAGS_POS, TYPE_FORWARDED|dir);
  p->InitArg(NAME_POS, name->ToWord());
  p->InitArg(FD_POS, fd);
  p->InitArg(HANDLE_POS, Store::UnmanagedPointerToWord(handle));
  p->InitArg(FINALIZATION_KEY_POS, finalizationSet->Register(p->ToWord()));
  return static_cast<IODesc *>(p);
}
#endif

IODesc::kind IODesc::GetKind() {
#if defined(__MINGW32__) || defined(_MSC_VER)
  switch (GetType()) {
  case TYPE_CLOSED: return CLOSED;
  case TYPE_FD: return SOCKET;
  case TYPE_HANDLE:
    switch (GetFileType(GetHandle())) {
    case FILE_TYPE_DISK: return FILE;
    case FILE_TYPE_CHAR: return TTY;
    case FILE_TYPE_PIPE: return PIPE;
    case FILE_TYPE_UNKNOWN:
    default:
      return UNKNOWN;
    }
  case TYPE_FORWARDED: return PIPE;
  default:
    Error("unknown type");
  }
#else
  if (GetType() == TYPE_CLOSED) return CLOSED;
  struct stat info;
  Interruptible(res, fstat(GetFD(), &info));
  if (res) return UNKNOWN;
  if (S_ISREG(info.st_mode)) return FILE;
  else if (S_ISDIR(info.st_mode)) return DIR;
  else if (S_ISCHR(info.st_mode)) return TTY; //--** is this correct?
  else if (S_ISBLK(info.st_mode)) return DEVICE;
  else if (S_ISFIFO(info.st_mode)) return PIPE;
  else if (S_ISLNK(info.st_mode)) return SYMLINK;
  else if (S_ISSOCK(info.st_mode)) return SOCKET;
  else return UNKNOWN;
#endif
}

void IODesc::Close() {
  u_int flags = GetFlags();
  switch (flags & TYPE_MASK) {
  case TYPE_CLOSED: break;
  case TYPE_FD:
    closesocket(GetFD());
    break;
#if defined(__MINGW32__) || defined(_MSC_VER)
  case TYPE_HANDLE:
    CloseHandle(GetHandle());
    break;
  case TYPE_FORWARDED:
    closesocket(GetFD());
    CloseHandle(GetHandle());
    break;
#endif
  }
  InitArg(FLAGS_POS, (flags & ~TYPE_MASK) | TYPE_CLOSED);
}

//
// Initialization of IODescs for Standard Handles
//
#if defined(__MINGW32__) || defined(_MSC_VER)
static IODesc *MakeStdIODesc(const char *name, DWORD nStdHandle, u_int dir) {
  HANDLE hStd = GetStdHandle(nStdHandle);
  if (hStd == INVALID_HANDLE_VALUE) // assume that it has been closed
    return IODesc::NewClosed(String::New(name));
  return IODesc::NewForwarded(dir, String::New(name), hStd);
}

IODesc *IODesc::NewFromStdIn() {
  IODesc *ioDesc =
    MakeStdIODesc("stdIn", STD_INPUT_HANDLE, IODesc::DIR_READER);
  if (ioDesc->GetType() != IODesc::TYPE_CLOSED)
    IOHandler::SetDefaultBlockFD(ioDesc->GetFD());
  return ioDesc;
}
IODesc *IODesc::NewFromStdOut() {
  return MakeStdIODesc("stdOut", STD_OUTPUT_HANDLE, IODesc::DIR_WRITER);
}
IODesc *IODesc::NewFromStdErr() {
  return MakeStdIODesc("stdErr", STD_OUTPUT_HANDLE, IODesc::DIR_WRITER);
}
#else
static IODesc *MakeStdIODesc(const char *name, int fd, u_int dir) {
  // Try to make the file descriptor nonblocking:
  int flags = fcntl(fd, F_GETFL, 0);
  if (flags == -1) {
    Assert(errno == EBADF);
    return IODesc::NewClosed(String::New(name));
  } else {
    fcntl(fd, F_SETFL, flags|O_NONBLOCK); // ignore result
    return IODesc::NewFromFD(dir, String::New(name), fd);
  }
}

IODesc *IODesc::NewFromStdIn() {
  IODesc *ioDesc = MakeStdIODesc("stdIn", 0, IODesc::DIR_READER);
  if (ioDesc->GetType() != IODesc::TYPE_CLOSED)
    IOHandler::SetDefaultBlockFD(ioDesc->GetFD());
  return ioDesc;
}
IODesc *IODesc::NewFromStdOut() {
  return MakeStdIODesc("stdOut", 1, IODesc::DIR_WRITER);
}
IODesc *IODesc::NewFromStdErr() {
  return MakeStdIODesc("stdErr", 2, IODesc::DIR_WRITER);
}
#endif
