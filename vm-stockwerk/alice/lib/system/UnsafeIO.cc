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
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>

#if defined(__MINGW32__) || defined(_MSC_VER)
#include <windows.h>
#include <winsock.h>
#define GetLastSocketError() WSAGetLastError()
#define Interruptible(res, call) int res = call; res = res;
#else
#include <sys/socket.h>
#define GetLastSocketError() errno
#define Interruptible(res, call)			\
  int res;						\
  do {							\
    res = call;						\
  } while (res < 0 && GetLastSocketError() == EINTR);
#endif

#include "store/Store.hh"
#include "generic/RootSet.hh"
#include "generic/FinalizationSet.hh"
#include "generic/StackFrame.hh"
#include "generic/Interpreter.hh"
#include "generic/Scheduler.hh"
#include "generic/IOHandler.hh"
#include "generic/Transients.hh"
#include "alice/Authoring.hh"

static word IoConstructor;
static word ClosedStreamConstructor;

//--** to be done: set cause to something sensible
#define RAISE_IO(cause, function, name)					\
  {									\
    ConVal *conVal =							\
      ConVal::New(Constructor::FromWordDirect(IoConstructor), 3);	\
    conVal->Init(0, cause);						\
    conVal->Init(1, String::New(function)->ToWord());			\
    conVal->Init(2, name->ToWord());					\
    RAISE(conVal->ToWord());						\
  }

// IOStream Classes
enum IOStreamType {
  IO_IN  = MIN_DATA_LABEL,
  IO_OUT = IO_IN + 1
};

class IOStreamFinalizationSet: public FinalizationSet {
public:
  virtual void Finalize(word value);
};

class IOStream: private Block {
private:
  enum { STREAM_POS, NAME_POS, FINALIZATION_KEY_POS, SIZE };

  static IOStreamFinalizationSet *finalizationSet;
protected:
  static BlockLabel IOStreamTypeToBlockLabel(IOStreamType type) {
    return static_cast<BlockLabel>(static_cast<int>(type));
  }

  static IOStream *New(IOStreamType type, std::FILE *file, String *name) {
    Block *p = Store::AllocBlock(IOStreamTypeToBlockLabel(type), SIZE);
    p->InitArg(STREAM_POS, Store::UnmanagedPointerToWord(file));
    p->InitArg(NAME_POS, name->ToWord());
    p->InitArg(FINALIZATION_KEY_POS, finalizationSet->Register(p->ToWord()));
    return static_cast<IOStream *>(p);
  }
public:
  using Block::ToWord;

  static void Init() {
    finalizationSet = new IOStreamFinalizationSet();
  }

  static IOStream *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert(p->GetLabel() == IOStreamTypeToBlockLabel(IO_IN) ||
	   p->GetLabel() == IOStreamTypeToBlockLabel(IO_OUT));
    return static_cast<IOStream *>(p);
  }

  std::FILE *GetFile() {
    return static_cast<std::FILE *>
      (Store::DirectWordToUnmanagedPointer(GetArg(STREAM_POS)));
  }
  String *GetName() {
    return String::FromWordDirect(GetArg(NAME_POS));
  }

  void Close() {
    FILE *file = GetFile();
    if (file != NULL) {
      u_int key = Store::DirectWordToInt(GetArg(FINALIZATION_KEY_POS));
      finalizationSet->Unregister(key);
      std::fclose(file);
      ReplaceArg(STREAM_POS, Store::UnmanagedPointerToWord(NULL));
    }
  }
};

void IOStreamFinalizationSet::Finalize(word value) {
  IOStream::FromWordDirect(value)->Close();
}

IOStreamFinalizationSet *IOStream::finalizationSet;

class Instream: public IOStream {
public:
  static Instream *New(std::FILE *file, String *name) {
    return static_cast<Instream *>(IOStream::New(IO_IN, file, name));
  }
  static Instream *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER ||
	   p->GetLabel() == IOStreamTypeToBlockLabel(IO_IN));
    return static_cast<Instream *>(p);
  }
};

class Outstream: public IOStream {
public:
  static Outstream *New(std::FILE *file, String *name) {
    return static_cast<Outstream *>(IOStream::New(IO_OUT, file, name));
  }
  static Outstream *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER ||
	   p->GetLabel() == IOStreamTypeToBlockLabel(IO_OUT));
    return static_cast<Outstream *>(p);
  }
};

#define DECLARE_INSTREAM(file, x)  DECLARE_BLOCKTYPE(Instream, file, x);
#define DECLARE_OUTSTREAM(file, x) DECLARE_BLOCKTYPE(Outstream, file, x);

static String *Concat(String *a, const char *b, u_int bLen) {
  u_int aLen = a->GetSize();
  String *s  = String::New(aLen + bLen);
  u_char *sb = s->GetValue();
  std::memcpy(sb, a->GetValue(), aLen);
  std::memcpy(sb + aLen, b, bLen);
  return s;
}

//
// Asynchronous File IO
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

class FdIO {
private:
  static const u_int BUF_SIZE = 8192;
  int fd;
  u_int nBytes;
  bool eof;
  char buf[BUF_SIZE];
public:
  // FdIO Constructor
  FdIO(int fHandle) {
    fd = fHandle;
    nBytes = 0;
    eof = false;
  }
  u_int GetNBytes() {
    return nBytes;
  }
  char *GetBuffer() {
    return buf;
  }
  bool IsEof() {
    return eof;
  }
  Future *Fill() {
    // A return value of INVALID_POINTER indicates that there is some
    // data in the buffer or we have reached EOF.
    if (nBytes != 0)
      return INVALID_POINTER;
    Future *future = IOHandler::CheckReadable(fd);
    if (future != INVALID_POINTER)
      return future;
#if defined(__MINGW32__) || defined(_MSC_VER)
    Interruptible(rdBytes, recv(fd, buf + nBytes, BUF_SIZE - nBytes, 0));
#else
    Interruptible(rdBytes, read(fd, buf + nBytes, BUF_SIZE - nBytes));
#endif
    if (rdBytes < 0) {
      // to be done: raise io something here
      Error("FdIO::Fill: critical io error");
    } else if (rdBytes == 0) { // eof
      eof = true;
    } else
      nBytes += rdBytes;
    return INVALID_POINTER;
  }
  void Commit(u_int count) {
    nBytes -= count;
    std::memmove(buf, buf + count, nBytes);
  }
};

static FdIO *stdinWrapper;

class FdInputFrame: public StackFrame {
private:
  enum { FILE_IO_POS, STRING_POS, READ_ALL_POS, SIZE };
public:
  using Block::ToWord;
  using StackFrame::GetInterpreter;

  // FdInputFrame Constructor
  static FdInputFrame *New(Interpreter *interpreter,
			   FdIO *fdIO, bool readAll) {
    StackFrame *frame = StackFrame::New(FD_INPUT_FRAME, interpreter, SIZE);
    String *string    = String::New(static_cast<u_int>(0));
    frame->InitArg(FILE_IO_POS, Store::UnmanagedPointerToWord(fdIO));
    frame->InitArg(STRING_POS, string->ToWord());
    frame->InitArg(READ_ALL_POS, readAll);
    return static_cast<FdInputFrame *>(frame);
  }
  // FdInputFrame Untagging
  static FdInputFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == FD_INPUT_FRAME);
    return static_cast<FdInputFrame *>(p);
  }

  // FdInputFrame Accessors
  FdIO *GetFdIO() {
    return static_cast<FdIO *>
      (Store::DirectWordToUnmanagedPointer(GetArg(FILE_IO_POS)));
  }
  String *GetString() {
    return String::FromWordDirect(GetArg(STRING_POS));
  }
  void SetString(String *string) {
    ReplaceArg(STRING_POS, string->ToWord());
  }
  bool ReadAll() {
    return Store::DirectWordToInt(GetArg(READ_ALL_POS));
  }
};

class IOInterpreter: public Interpreter {
public:
  static IOInterpreter *self;
  // IOInterpreter Constructor
  IOInterpreter(): Interpreter() {}
  // IOInterpreter Static Constructor
  static void Init() {
    self = new IOInterpreter();
  }
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// IOInterpreter Methods
//
IOInterpreter *IOInterpreter::self;

Interpreter::Result IOInterpreter::Run() {
  FdInputFrame *frame = FdInputFrame::FromWordDirect(Scheduler::GetFrame());
  FdIO *fdIO = frame->GetFdIO();
  Future *future = fdIO->Fill();
  if (future != INVALID_POINTER) {
    Scheduler::currentData = future->ToWord();
    return Interpreter::REQUEST;
  }
  // We have data
  String *string = frame->GetString();
  char *buffer   = fdIO->GetBuffer();
  u_int nBytes   = fdIO->GetNBytes();
  bool eof       = fdIO->IsEof();
  if (frame->ReadAll()) { // inputAll
    string = Concat(string, buffer, nBytes);
    fdIO->Commit(nBytes);
  } else { // inputLine
    u_int nBytes = fdIO->GetNBytes();
    for (u_int i = 0; i < nBytes; i++) {
#if defined(__MINGW32__) || defined(_MSC_VER)
      if (i + 1 < nBytes && buffer[i] == '\r' && buffer[i + 1] == '\n') {
	buffer[i] = '\n';
	string = Concat(string, buffer, i + 1);
	fdIO->Commit(i +  2);
	eof = true;
	break;
      } else
#endif
      if (buffer[i] == '\n') {
	string = Concat(string, buffer, i + 1);
	fdIO->Commit(i + 1);
	eof = true;
	break;
      }
    }
  }
  if (eof) {
    Scheduler::PopFrame();
    Scheduler::nArgs = Scheduler::ONE_ARG;
    Scheduler::currentArgs[0] = string->ToWord();
    return Interpreter::CONTINUE;
  } else {
    frame->SetString(string);
    Scheduler::nArgs = 0;
    if (StatusWord::GetStatus() != 0)
      return Interpreter::PREEMPT;
    else
      return Interpreter::CONTINUE;
  }
}

const char *IOInterpreter::Identify() {
  return "IOInterpreter";
}

void IOInterpreter::DumpFrame(word) {
  FdInputFrame *frame = FdInputFrame::FromWordDirect(Scheduler::GetFrame());
  if (frame->ReadAll()) // inputAll
    std::fprintf(stderr, "Primitive UnsafeIO.inputAll\n");
  else // inputLine
    std::fprintf(stderr, "Primitive UnsafeIO.inputLine\n");
}

static const u_int bufSize = 8192;

//
// Primitives
//

DEFINE3(UnsafeIO_Io) {
  ConVal *conVal = ConVal::New(Constructor::FromWordDirect(IoConstructor), 3);
  conVal->Init(0, x0);
  conVal->Init(1, x1);
  conVal->Init(2, x2);
  RETURN(conVal->ToWord());
} END

DEFINE2(UnsafeIO_openIn) {
  DECLARE_BOOL(isText, x0);
  DECLARE_STRING(name, x1);
  const char *flags = (isText? "r": "rb");
  std::FILE *file = std::fopen(name->ExportC(), flags);
  if (file != NULL) {
    RETURN(Instream::New(file, name)->ToWord());
  } else {
    RAISE_IO(Store::IntToWord(0), "openIn", name);
  }
} END

DEFINE1(UnsafeIO_closeIn) {
  DECLARE_INSTREAM(stream, x0);
  stream->Close();
  RETURN_UNIT;
} END

DEFINE1(UnsafeIO_inputAll) {
  DECLARE_INSTREAM(stream, x0);
  std::FILE *file = stream->GetFile();
  if (file == NULL) { // already closed
    RETURN(String::New(static_cast<u_int>(0))->ToWord());
  } if (file == stdin) {
    FdInputFrame *frame =
      FdInputFrame::New(IOInterpreter::self, stdinWrapper, true);
    Scheduler::PushFrame(frame->ToWord());
    RETURN0;
  } else {
    char buf[bufSize];
    String *b = String::New(static_cast<u_int>(0));
    while (!feof(file)) {
      u_int nRead = std::fread(buf, 1, bufSize, file);
      if (ferror(file)) {
	RAISE_IO(Store::IntToWord(0), "inputAll", stream->GetName());
      }
      b = Concat(b, buf, nRead);
    }
    RETURN(b->ToWord());
  }
} END

DEFINE1(UnsafeIO_inputLine) {
  DECLARE_INSTREAM(stream, x0);
  std::FILE *file = stream->GetFile();
  if (file == NULL) { // already closed
    RETURN(String::New(static_cast<u_int>(0))->ToWord());
  } else if (file == stdin) {
    FdInputFrame *frame =
      FdInputFrame::New(IOInterpreter::self, stdinWrapper, false);
    Scheduler::PushFrame(frame->ToWord());
    RETURN0;
  } else {
    char buf[bufSize];
    String *b = String::New(static_cast<u_int>(0));
    u_int index = 0;
    int c = std::fgetc(file);
    while (c != EOF) {
      buf[index++] = c;
      if (c == '\n') {
	if (index != 0)
	  b = Concat(b, buf, index);
	RETURN(b->ToWord());
      }
      if (index == bufSize) {
	if (b->GetSize() + index > String::maxSize) {
	  RAISE(PrimitiveTable::General_Size);
	}
	b = Concat(b, buf, index);
	index = 0;
      }
      c = std::fgetc(file);
    }
    b = Concat(b, buf, index);
    if (b->GetSize())
      b = Concat(b, "\n", 1);
    RETURN(b->ToWord());
  }
} END

DEFINE2(UnsafeIO_openOut) {
  DECLARE_BOOL(isText, x0);
  DECLARE_STRING(name, x1);
  const char *flags = (isText? "w": "wb");
  std::FILE *file = std::fopen(name->ExportC(), flags);
  if (file != NULL) {
    RETURN(Outstream::New(file, name)->ToWord());
  } else {
    RAISE_IO(Store::IntToWord(0), "openOut", name);
  }
} END

DEFINE2(UnsafeIO_openAppend) {
  DECLARE_BOOL(isText, x0);
  DECLARE_STRING(name, x1);
  const char *flags = (isText? "wa": "wab");
  std::FILE *file = std::fopen(name->ExportC(), flags);
  if (file != NULL) {
    RETURN(Outstream::New(file, name)->ToWord());
  } else {
    RAISE_IO(Store::IntToWord(0), "openAppend", name);
  }
} END

DEFINE1(UnsafeIO_closeOut) {
  DECLARE_OUTSTREAM(stream, x0);
  stream->Close();
  RETURN_UNIT;
} END

DEFINE1(UnsafeIO_flushOut) {
  DECLARE_OUTSTREAM(stream, x0);
  FILE *file = stream->GetFile();
  if (file != NULL)
    std::fflush(file);
  RETURN_UNIT;
} END

DEFINE2(UnsafeIO_output) {
  DECLARE_OUTSTREAM(stream, x0);
  DECLARE_STRING(s, x1);
  FILE *file = stream->GetFile();
  if (file == NULL) {
    RAISE_IO(ClosedStreamConstructor, "output", stream->GetName());
  } else {
    u_int size = s->GetSize();
    u_int nWritten = std::fwrite(s->GetValue(), 1, size, file);
    if (nWritten == size) {
      RETURN_UNIT;
    } else {
      RAISE_IO(Store::IntToWord(0), "output", stream->GetName());
    }
  }
} END

DEFINE2(UnsafeIO_output1) {
  DECLARE_OUTSTREAM(stream, x0);
  DECLARE_INT(c, x1);
  FILE *file = stream->GetFile();
  if (file == NULL) { // already closed
    RAISE_IO(ClosedStreamConstructor, "output1", stream->GetName());
  } else if (std::fputc(c, file) != EOF) {
    RETURN_UNIT;
  } else {
    RAISE_IO(Store::IntToWord(0), "output1", stream->GetName());
  }
} END

DEFINE1(UnsafeIO_print) {
  DECLARE_STRING(s, x0);
  std::printf("%.*s", static_cast<int>(s->GetSize()), s->GetValue());
  std::fflush(stdout);
  RETURN_UNIT;
} END

word UnsafeIO() {
  IoConstructor =
    UniqueConstructor::New(String::New("IO.Io"))->ToWord();
  RootSet::Add(IoConstructor);
  ClosedStreamConstructor =
    UniqueConstructor::New(String::New("IO.ClosedStream"))->ToWord();
  RootSet::Add(ClosedStreamConstructor);

  IOStream::Init();
  IOInterpreter::Init();
  int handle;
#if defined(__MINGW32__) || defined(_MSC_VER)
  // to be done: Windows need sockets here; moved from UnsafeSocket
  WSADATA wsa_data;
  WORD req_version = MAKEWORD(1, 1);
  if (WSAStartup(req_version, &wsa_data) != 0)
    Error("no usable WinSock DLL found");
  // We need select on stdin
  int sv[2];
  if (IOSupport::SocketPair(PF_UNIX, SOCK_STREAM, 0, sv) == -1)
    Error("socket pair failed");
  IOSupport::CreateReader(sv[0], GetStdHandle(STD_INPUT_HANDLE));
  handle = sv[1];
#else
  handle = fileno(stdin);
  // Try to make stdin nonblocking
  int flags = fcntl(handle, F_GETFL, 0);
  if (flags == -1)
    Error("unable to query stdin flags");
  flags |= O_NONBLOCK;
  if (fcntl(handle, F_SETFL, flags) == -1)
    Error("unable to make stdin nonblocking");
#endif
  stdinWrapper = new FdIO(handle); //--** also for stdout, stderr
  IOHandler::SetDefaultBlockFD(handle);

  Record *record = Record::New(18);
  record->Init("'Io", IoConstructor);
  INIT_STRUCTURE(record, "UnsafeIO", "Io",
		 UnsafeIO_Io, 3, true);
  record->Init("'ClosedStream", ClosedStreamConstructor);
  record->Init("ClosedStream", ClosedStreamConstructor);
  record->Init("stdIn",
	       Instream::New(stdin, String::New("stdin"))->ToWord());
  record->Init("stdOut",
	       Outstream::New(stdout, String::New("stdout"))->ToWord());
  record->Init("stdErr",
	       Outstream::New(stderr, String::New("stderr"))->ToWord());
  INIT_STRUCTURE(record, "UnsafeIO", "openIn",
		 UnsafeIO_openIn, 2, true);
  INIT_STRUCTURE(record, "UnsafeIO", "closeIn",
		 UnsafeIO_closeIn, 1, true);
  INIT_STRUCTURE(record, "UnsafeIO", "inputAll",
		 UnsafeIO_inputAll, 1, true);
  INIT_STRUCTURE(record, "UnsafeIO", "inputLine",
		 UnsafeIO_inputLine, 1, true);
  INIT_STRUCTURE(record, "UnsafeIO", "openOut",
		 UnsafeIO_openOut, 2, true);
  INIT_STRUCTURE(record, "UnsafeIO", "openAppend",
		 UnsafeIO_openAppend, 2, true);
  INIT_STRUCTURE(record, "UnsafeIO", "closeOut",
		 UnsafeIO_closeOut, 1, true);
  INIT_STRUCTURE(record, "UnsafeIO", "flushOut",
		 UnsafeIO_flushOut, 1, true);
  INIT_STRUCTURE(record, "UnsafeIO", "output",
		 UnsafeIO_output, 2, true);
  INIT_STRUCTURE(record, "UnsafeIO", "output1",
		 UnsafeIO_output1, 2, true);
  INIT_STRUCTURE(record, "UnsafeIO", "print",
		 UnsafeIO_print, 1, true);
  RETURN_STRUCTURE("UnsafeIO$", record);
}
