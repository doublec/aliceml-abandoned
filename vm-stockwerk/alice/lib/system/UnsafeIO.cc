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

#include "store/Store.hh"
#include "generic/RootSet.hh"
#include "generic/StackFrame.hh"
#include "generic/Interpreter.hh"
#include "generic/Scheduler.hh"
#include "generic/IOHandler.hh"
#include "generic/Transients.hh"
#include "alice/primitives/Authoring.hh"

static word IoConstructor;

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

// Builtin IOStream Classes
//--** to be done: finalization
enum IOStreamType {
  IO_IN  = MIN_DATA_LABEL,
  IO_OUT = (IO_IN + 1)
};

class IOStream : private Block {
private:
  static const u_int STREAM_POS = 0;
  static const u_int NAME_POS   = 1;
  static const u_int SIZE       = 2;
protected:
  static BlockLabel IOStreamTypeToBlockLabel(IOStreamType type) {
    return static_cast<BlockLabel>(static_cast<int>(type));
  }
public:
  using Block::ToWord;
  // IOStream Accessors
  std::FILE *GetStream() {
    return static_cast<std::FILE *>
      (Store::DirectWordToUnmanagedPointer(GetArg(STREAM_POS)));
  }
  String *GetName() {
    return String::FromWordDirect(GetArg(NAME_POS));
  }
  // IOStream Constructor
  static IOStream *New(IOStreamType type, std::FILE *file, String *name) {
    Block *p = Store::AllocBlock(IOStreamTypeToBlockLabel(type), SIZE);
    p->InitArg(STREAM_POS, Store::UnmanagedPointerToWord(file));
    p->InitArg(NAME_POS, name->ToWord());
    return static_cast<IOStream *>(p);
  }
};

class InStream : public IOStream {
public:
  // InStream Constructor
  static InStream *New(std::FILE *file, String *name) {
    return static_cast<InStream *>(IOStream::New(IO_IN, file, name));
  }
  // InStream Untagging
  static InStream *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER ||
	   p->GetLabel() == IOStreamTypeToBlockLabel(IO_IN));
    return static_cast<InStream *>(p);
  }
};

class OutStream : public IOStream {
public:
  // OutStream Constructor
  static OutStream *New(std::FILE *file, String *name) {
    return static_cast<OutStream *>(IOStream::New(IO_OUT, file, name));
  }
  // OutStream Untagging
  static OutStream *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER ||
	   p->GetLabel() == IOStreamTypeToBlockLabel(IO_OUT));
    return static_cast<OutStream *>(p);
  }
};

#define DECLARE_INSTREAM(file, x)  DECLARE_BLOCKTYPE(InStream, file, x);
#define DECLARE_OUTSTREAM(file, x) DECLARE_BLOCKTYPE(OutStream, file, x);

static String *Concat(String *a, const char *b, u_int bLen) {
  u_int aLen = a->GetSize();
  String *s  = String::New(aLen + bLen);
  u_char *sb = s->GetValue();
  std::memcpy(sb, a->GetValue(), aLen);
  std::memcpy(sb + aLen, b, bLen);
  return s;
}

static const u_int bufSize = 8192;
static char buf[bufSize];

//
// Asnychronous File IO
//
#if defined(__MINGW32__) || defined(_MSC_VER)
#include <windows.h>
#include <winsock.h>

class InOut {
public:
  SOCKET fd1;
  HANDLE fd2;
  InOut(SOCKET f1, HANDLE f2): fd1(f1), fd2(f2) {}
};
#define bufSz 10000

static DWORD __stdcall ReaderThread(void *p) {
  InOut *io  = (InOut*) p;
  SOCKET out = io->fd1;
  HANDLE in  = io->fd2;
  delete io;

//    // this one solves a problem with W2K SP2.
//    // readerThread cause the system to freeze if we
//    // don't call gethostname() (?load ws2_32.dll? changed with SP2)
//    // before ReadFile().
//    char dummyBuf[1000];
//    gethostname(dummyBuf,sizeof(Dummybuf));

  char buf[bufSz];
  while(1) {
    DWORD count;
    if (ReadFile(in,buf,bufSz,&count,0)==FALSE) {
      fprintf(stderr ,"ReadFile failed: %d\n", GetLastError);
      break;
    }

  loop:
    int sent = send(out,buf,count,0);
    if (sent < 0) {
      fprintf(stderr, "send(%d) failed: %d\n", out, GetLastError);
      break;
    }
    count -= sent;
    if (count > 0)
      goto loop;
  }
  CloseHandle(in);
  ExitThread(1);
  return 1;
}

static DWORD __stdcall WriterThread(void *p) {
  InOut *io = (InOut*) p;
  SOCKET in = io->fd1;
  HANDLE out = io->fd2;
  delete io;
  
  char buf[bufSz];
  while(1) {
    int got = recv(in,buf,bufSz,0);
    if (got<0) {
      fprintf(stderr, "recv(%d) failed: %d\n", in, WSAGetLastError());
      break;
    }

  loop:
    DWORD count;
    if (WriteFile(out,buf,got,&count,0)==FALSE) {
      //message("WriteFile(%d) failed: %d\n",out,GetLastError());
      break;
    }
    got -= count;
    if (got>0)
      goto loop;
  }
  CloseHandle(out);
  ExitThread(1);
  return 1;
}

class IOSupport {
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
    DWORD tid;
    HANDLE th = CreateThread(NULL, 10000, &ReaderThread,
			     new InOut(s,h), 0, &tid);
    CloseHandle(th);
  }
  static void CreateWrite(SOCKET s, HANDLE h) {
    DWORD tid;
    HANDLE th = CreateThread(NULL, 10000, &WriterThread,
			     new InOut(s,h), 0, &tid);
    CloseHandle(th);
  }
};

#define GetLastError WSAGetLastError()

#undef EAGAIN
#define EAGAIN WSAEWOULDBLOCK

#else

#define GetLastError errno
#endif

static word stdinWrapper;

class FileIO {
protected:
  static const u_int BUF_SIZE = 8192;
  int stream;
  u_int nBytes;
  u_int eof;
  char buf[BUF_SIZE];
public:
  // FileIO Constructor and Destructor
  FileIO(int fHandle) {
    stream = fHandle;
    nBytes = 0;
    eof    = 0;
  }
  u_int GetNBytes() {
    return nBytes;
  }
  char *GetBuffer() {
    return buf;
  }
  u_int IsEof() {
    return eof;
  }
  Future *Fill() {
    if (nBytes == (BUF_SIZE - 1))
      return INVALID_POINTER;
  retry:
    Future *future = IOHandler::CheckReadable(stream);
    if (future != INVALID_POINTER)
      return future;
    // to be done: correct policy here
#if defined(__MINGW32__) || defined(_MSC_VER)
    int rdBytes = recv(stream, buf + nBytes, BUF_SIZE - nBytes, 0);
#else
    int rdBytes = read(stream, buf + nBytes, BUF_SIZE - nBytes);
#endif
    if (rdBytes == -1) {
      switch (GetLastError) {
      case EAGAIN:
	// Can this happen here?
	fprintf(stderr, "io would block\n");
	goto retry;
      default:
	// to be done: raise io something here
	Error("FileIO::Fill: critical io error\n");
	break;
      }
    }
    else
      nBytes += rdBytes;
    return INVALID_POINTER;
  }
  void Commit(u_int count) {
    nBytes -= count;
    memmove(buf, buf + count, nBytes);
  }
  // FileIO Tagging and Untagging
  word ToWord() {
    return Store::UnmanagedPointerToWord(this);
  }
  static FileIO *FileIO::FromWordDirect(word io) {
    return static_cast<FileIO *>(Store::DirectWordToUnmanagedPointer(io));
  }
};

class IOFrame : public StackFrame {
protected:
  static const u_int FILE_IO_POS = 0;
  static const u_int STRING_POS  = 1;
  static const u_int ALL_POS     = 2;
  static const u_int SIZE        = 3;
public:
  using Block::ToWord;
  using StackFrame::GetInterpreter;
  // IOFrame Accessors
  FileIO *GetFileIO() {
    return static_cast<FileIO *>
      (Store::DirectWordToUnmanagedPointer(StackFrame::GetArg(FILE_IO_POS)));
  }
  String *GetString() {
    return String::FromWordDirect(StackFrame::GetArg(STRING_POS));
  }
  void SetString(String *string) {
    StackFrame::ReplaceArg(STRING_POS, string->ToWord());
  }
  word NeedAll() {
    return StackFrame::GetArg(ALL_POS);
  }
  // IOFrame Constructor
  static IOFrame *New(Interpreter *interpreter, FileIO *stream,
		      u_int readAll) {
    StackFrame *frame = StackFrame::New(IO_FRAME, interpreter, SIZE);
    String *string    = String::New(static_cast<u_int>(0));
    frame->InitArg(FILE_IO_POS, Store::UnmanagedPointerToWord(stream));
    frame->InitArg(STRING_POS, string->ToWord());
    frame->InitArg(ALL_POS, Store::IntToWord(readAll));
    return static_cast<IOFrame *>(frame);
  }
  // IOFrame Untagging
  static IOFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == IO_FRAME);
    return static_cast<IOFrame *>(p);
  }
};

class IOInterpreter : public Interpreter {
public:
  static IOInterpreter *self;
  // IOInterpreter Constructor
  IOInterpreter() : Interpreter() {}
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

static Interpreter::Result CheckPreempt() {
  if (StatusWord::GetStatus(Store::GCStatus() | Scheduler::PreemptStatus())) {
    PREEMPT;
  }
  else {
    RETURN0;
  }
}

Interpreter::Result IOInterpreter::Run() {
  IOFrame *frame = IOFrame::FromWordDirect(Scheduler::GetFrame());
  FileIO *stream = frame->GetFileIO();
  Future *future = stream->Fill();
  if (future != INVALID_POINTER) {
    Scheduler::currentData = future->ToWord();
    return Interpreter::REQUEST;
  }
  // We have data
  String *string = frame->GetString();
  u_int nBytes   = stream->GetNBytes();
  u_int eof      = 0;
  if (frame->NeedAll() == Store::IntToWord(0)) {
    char *buf    = stream->GetBuffer();
    u_int nBytes = stream->GetNBytes();
    u_int i      = 0;
    while (i < nBytes) {
#if defined(__MINGW32__) || defined(_MSC_VER)
      // to be done: proper character conversion
      if ((buf[i] == 0x0d) &&
	  ((i + 1) < nBytes) && (buf[i + 1] == 0x0a)) {
	string = Concat(string, stream->GetBuffer(), i + 1);
	stream->Commit(i +  2);
	eof = 1;
	break;
      }
#else
      if (buf[i] == '\n') {
	string = Concat(string, stream->GetBuffer(), i + 1);
	stream->Commit(i + 1);
	eof = 1;
	break;
      }
#endif
      else
	i++;
    }
  }
  else {
    string = Concat(string, stream->GetBuffer(), nBytes);
    stream->Commit(nBytes);
    eof = stream->IsEof();
  }
  if (eof) {
    Scheduler::PopFrame();
    RETURN(string->ToWord());
  }
  else {
    frame->SetString(string);
    return CheckPreempt();
  }
}

const char *IOInterpreter::Identify() {
  return "IOInterpreter";
}

void IOInterpreter::DumpFrame(word) {
  std::fprintf(stderr, "IOInterpreter frame\n");
}

DEFINE3(UnsafeIO_Io) {
  ConVal *conVal = ConVal::New(Constructor::FromWordDirect(IoConstructor), 3);
  conVal->Init(0, x0);
  conVal->Init(1, x1);
  conVal->Init(2, x2);
  RETURN(conVal->ToWord());
} END

DEFINE2(UnsafeIO_openIn) {
  DECLARE_BOOL(binary, x0);
  DECLARE_STRING(name, x1);
  const char *flags = (binary ? "rb" : "r");
  std::FILE *file = std::fopen(name->ExportC(), flags);
  if (file != NULL) {
    RETURN(InStream::New(file, name)->ToWord());
  } else {
    RAISE_IO(Store::IntToWord(0), "openIn", name);
  }
} END

DEFINE1(UnsafeIO_closeIn) {
  DECLARE_INSTREAM(stream, x0);
  std::fclose(stream->GetStream());
  RETURN_UNIT;
} END

DEFINE1(UnsafeIO_inputAll) {
  DECLARE_INSTREAM(stream, x0);
  std::FILE *file = stream->GetStream();
  if (file == stdin) {
    FileIO *io     = FileIO::FromWordDirect(stdinWrapper);
    IOFrame *frame = IOFrame::New(IOInterpreter::self, io, 1);
    Scheduler::PushFrame(frame->ToWord());
    RETURN0;
  }
  else {
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
  std::FILE *file = stream->GetStream();
  if (file == stdin) {
    FileIO *io     = FileIO::FromWordDirect(stdinWrapper);
    IOFrame *frame = IOFrame::New(IOInterpreter::self, io, 0);
    Scheduler::PushFrame(frame->ToWord());
    RETURN0;
  }
  else {
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
  DECLARE_BOOL(binary, x0);
  DECLARE_STRING(name, x1);
  const char *flags = (binary ? "wb" : "w");
  std::FILE *file = std::fopen(name->ExportC(), flags);
  if (file != NULL) {
    RETURN(OutStream::New(file, name)->ToWord());
  } else {
    RAISE_IO(Store::IntToWord(0), "openOut", name);
  }
} END

DEFINE2(UnsafeIO_openAppend) {
  DECLARE_BOOL(binary, x0);
  DECLARE_STRING(name, x1);
  const char *flags = (binary ? "wab" : "wa");
  std::FILE *file = std::fopen(name->ExportC(), flags);
  if (file != NULL) {
    RETURN(OutStream::New(file, name)->ToWord());
  } else {
    RAISE_IO(Store::IntToWord(0), "openAppend", name);
  }
} END

DEFINE1(UnsafeIO_closeOut) {
  DECLARE_OUTSTREAM(stream, x0);
  std::fclose(stream->GetStream());
  RETURN_UNIT;
} END

DEFINE1(UnsafeIO_flushOut) {
  DECLARE_OUTSTREAM(stream, x0);
  std::fflush(stream->GetStream());
  RETURN_UNIT;
} END

DEFINE2(UnsafeIO_output) {
  DECLARE_OUTSTREAM(stream, x0);
  DECLARE_STRING(s, x1);
  u_int size = s->GetSize();
  u_int nWritten = std::fwrite(s->GetValue(), 1, size, stream->GetStream());
  if (nWritten == size) {
    RETURN_UNIT;
  } else {
    RAISE_IO(Store::IntToWord(0), "output", stream->GetName());
  }
} END

DEFINE2(UnsafeIO_output1) {
  DECLARE_OUTSTREAM(stream, x0);
  DECLARE_INT(c, x1);
  if (std::fputc(c, stream->GetStream()) != EOF) {
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
  IoConstructor = UniqueConstructor::New(String::New("IO.Io"))->ToWord();
  RootSet::Add(IoConstructor);

  IOInterpreter::Init();
  int handle;
#if defined(__MINGW32__) || defined(_MSC_VER)
  // to be done: Windows need sockets here; moved from UnsafeSocket
  WSADATA wsa_data;
  WORD req_version = MAKEWORD(1, 1);
  if (WSAStartup(req_version, &wsa_data) != 0) {
    Error("No usable WinSock DLL found");
  }
  // We need select on stdin
  int sv[2];
  if (IOSupport::SocketPair(PF_UNIX, SOCK_STREAM, 0, sv) == -1) {
    fprintf(stderr, "UnsafeIO: socket pair failed\n");
    exit(1);
  }
  IOSupport::CreateReader(sv[0], GetStdHandle(STD_INPUT_HANDLE));
  handle = sv[1];
#else
  handle = fileno(stdin);
  // Try to make stdin non blocking
  int flags = fcntl(handle, F_GETFL, 0);
  if (flags == -1) {
    Error("Unable to query stdin flags\n");
  }
  flags |= O_NONBLOCK;
  if (fcntl(handle, F_SETFL, flags) == -1) {
    Error("Unable to make stdin nonblocking\n");
  }
#endif
  FileIO *io = new FileIO(handle);
  stdinWrapper = io->ToWord();
  IOHandler::SetDefaultBlockFD(handle);
  RootSet::Add(stdinWrapper);

  Record *record = Record::New(16);
  record->Init("'Io", IoConstructor);
  INIT_STRUCTURE(record, "UnsafeIO", "Io",
		 UnsafeIO_Io, 3, true);
  record->Init("stdIn",
	       InStream::New(stdin, String::New("stdin"))->ToWord());
  record->Init("stdOut",
	       OutStream::New(stdout, String::New("stdout"))->ToWord());
  record->Init("stdErr",
	       OutStream::New(stderr, String::New("stderr"))->ToWord());
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
