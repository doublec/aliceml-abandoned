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
#include "generic/RootSet.hh"
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
  String *b = String::New(static_cast<u_int>(0));
  while (!feof(file)) {
    u_int nRead = std::fread(buf, 1, bufSize, file);
    if (ferror(file)) {
      RAISE_IO(Store::IntToWord(0), "inputAll", stream->GetName());
    }
    b = Concat(b, buf, nRead);
  }
  RETURN(b->ToWord());
} END

DEFINE1(UnsafeIO_inputLine) {
  DECLARE_INSTREAM(stream, x0);
  std::FILE *file = stream->GetStream();
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
  b = Concat(b, "\n", 1);
  RETURN(b->ToWord());
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
