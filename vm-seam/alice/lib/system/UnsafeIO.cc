//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Contributors:
//   Leif Kornstaedt <kornstaedt@ps.uni-sb.de>
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
#include "emulator/Authoring.hh"
#include "emulator/RootSet.hh"

static word IoConstructor;

//--** to be done: set cause to something sensible
#define RAISE_IO(cause, function, name)					\
  {									\
    ConVal *conVal =							\
      ConVal::New(Constructor::FromWordDirect(IoConstructor), 3);	\
    conVal->Init(0, cause);						\
    conVal->Init(1, String::New(function)->ToWord());			\
    conVal->Init(1, name->ToWord());					\
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
  FILE *GetStream() {
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

// String Handling
static char *ExportCString(String *s) {
  u_int sLen = s->GetSize();
  String *e  = String::New(sLen + 1);
  u_char *eb = e->GetValue();
  std::memcpy(eb, s->GetValue(), sLen);
  eb[sLen] = '\0';
  return reinterpret_cast<char *>(eb);
}

static String *Concat(String *a, const char *b, u_int bLen) {
  u_int aLen = a->GetSize();
  String *s  = String::New(aLen + bLen);
  u_char *sb = s->GetValue();
  std::memcpy(sb, a->GetValue(), aLen);
  std::memcpy(sb + aLen, b, bLen);
  return s;
}

// Builtins
DEFINE3(UnsafeIO_Io) {
  ConVal *conVal = ConVal::New(Constructor::FromWordDirect(IoConstructor), 3);
  conVal->Init(0, x0);
  conVal->Init(1, x1);
  conVal->Init(2, x2);
  RETURN(conVal->ToWord());
} END

DEFINE1(UnsafeIO_closeIn) {
  DECLARE_INSTREAM(stream, x0);
  std::fclose(stream->GetStream());
  RETURN_UNIT;
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

DEFINE1(UnsafeIO_inputAll) {
  static const u_int bufSize = 8192;
  static char *buf = static_cast<char *>(std::malloc(sizeof(char) * bufSize));
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
  static const u_int bufSize = 1024;
  static char *buf = static_cast<char *>(std::malloc(sizeof(char) * bufSize));
  DECLARE_INSTREAM(stream, x0);
  std::FILE *file = stream->GetStream();
  String *b  = String::New(static_cast<u_int>(0));
  while (true) {
    u_int nRead = std::fread(buf, 1, bufSize, file);
    if (ferror(file)) {
      RAISE_IO(Store::IntToWord(0), "inputLine", stream->GetName());
    }
    u_int seek = 0;
    while (seek < nRead) {
      if (buf[seek++] == '\n') {
	if (seek < nRead) {
	  std::fseek(file, seek - nRead, SEEK_CUR);
	}
	if (b->GetSize() + seek > String::maxSize) {
	  RAISE(PrimitiveTable::General_Size);
	} else {
	  RETURN(Concat(b, buf, seek)->ToWord());
	}
      }
    }
    if (b->GetSize() + nRead + 1 > String::maxSize) { // space for final '\n'
      RAISE(PrimitiveTable::General_Size);
    }
    b = Concat(b, buf, nRead);
    if (feof(file)) {
      if (b->GetSize() == 0) {
	RETURN(b->ToWord());
      } else {
	RETURN(Concat(b, "\n", 1)->ToWord());
      }
    }
  }
} END

DEFINE2(UnsafeIO_openAppend) {
  DECLARE_BOOL(binary, x0);
  DECLARE_STRING(name, x1);
  const char *flags = (binary ? "wab" : "wa");
  std::FILE *file = std::fopen(ExportCString(name), flags);
  if (file != NULL) {
    RETURN(OutStream::New(file, name)->ToWord());
  } else {
    RAISE_IO(Store::IntToWord(0), "openAppend", name);
  }
} END

DEFINE2(UnsafeIO_openIn) {
  DECLARE_BOOL(binary, x0);
  DECLARE_STRING(name, x1);
  const char *flags = (binary ? "rb" : "r");
  std::FILE *file = std::fopen(ExportCString(name), flags);
  if (file != NULL) {
    RETURN(InStream::New(file, name)->ToWord());
  } else {
    RAISE_IO(Store::IntToWord(0), "openIn", name);
  }
} END

DEFINE2(UnsafeIO_openOut) {
  DECLARE_BOOL(binary, x0);
  DECLARE_STRING(name, x1);
  const char *flags = (binary ? "wb" : "w");
  std::FILE *file = std::fopen(ExportCString(name), flags);
  if (file != NULL) {
    RETURN(OutStream::New(file, name)->ToWord());
  } else {
    RAISE_IO(Store::IntToWord(0), "openOut", name);
  }
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

word UnsafeIO(void) {
  IoConstructor = UniqueConstructor::New(String::New("IO.Io"))->ToWord();
  RootSet::Add(IoConstructor);

  Tuple *t = Tuple::New(16);
  t->Init(0, IoConstructor);
  t->Init(1, Primitive::MakeClosure("UnsafeIO_Io",
				    UnsafeIO_Io, 3, true));
  t->Init(2, Primitive::MakeClosure("UnsafeIO_closeIn",
				    UnsafeIO_closeIn, 1, true));
  t->Init(3, Primitive::MakeClosure("UnsafeIO_closeOut",
				    UnsafeIO_closeOut, 1, true));
  t->Init(4, Primitive::MakeClosure("UnsafeIO_flushOut",
				    UnsafeIO_flushOut, 1, true));
  t->Init(5, Primitive::MakeClosure("UnsafeIO_inputAll",
				    UnsafeIO_inputAll, 1, true));
  t->Init(6, Primitive::MakeClosure("UnsafeIO_inputLine",
				    UnsafeIO_inputLine, 1, true));
  t->Init(7, Primitive::MakeClosure("UnsafeIO_openAppend",
				    UnsafeIO_openAppend, 2, true));
  t->Init(8, Primitive::MakeClosure("UnsafeIO_openIn",
				    UnsafeIO_openIn, 2, true));
  t->Init(9, Primitive::MakeClosure("UnsafeIO_openOut",
				    UnsafeIO_openOut, 2, true));
  t->Init(10, Primitive::MakeClosure("UnsafeIO_output",
				     UnsafeIO_output, 2, true));
  t->Init(11, Primitive::MakeClosure("UnsafeIO_output1",
				     UnsafeIO_output1, 2, true));
  t->Init(12, Primitive::MakeClosure("UnsafeIO_print",
				     UnsafeIO_print, 1, true));
  t->Init(13, OutStream::New(stderr, String::New("stderr"))->ToWord());
  t->Init(14, InStream::New(stdin, String::New("stdin"))->ToWord());
  t->Init(15, OutStream::New(stdout, String::New("stdout"))->ToWord());
  RETURN_STRUCTURE(t);
}
