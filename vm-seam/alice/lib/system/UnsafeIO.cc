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

typedef enum {
  IO_IN  = MIN_DATA_LABEL,
  IO_OUT = (IO_IN + 1)
} IOStreamType;

// Builtin IoStream Classes
//--** to be done: finalization
class IOStream : private Block {
private:
  static const u_int STREAM_POS = 0;
  static const u_int NAME_POS   = 1;
  static const u_int SIZE       = 2;
public:
  using Block::ToWord;
  // IoStream Accessors
  FILE *GetStream() {
    return (FILE *) Store::DirectWordToUnmanagedPointer(GetArg(STREAM_POS));
  }
  String *GetName() {
    return String::FromWordDirect(GetArg(NAME_POS));
  }
  // IoStream Constructor
  static IOStream *New(IOStreamType type, FILE *file, String *name) {
    Block *p = Store::AllocBlock((BlockLabel) type, SIZE);
    p->InitArg(STREAM_POS, Store::UnmanagedPointerToWord(file));
    p->InitArg(NAME_POS, name->ToWord());
    return (IOStream *) p;
  }
};

class InStream : public IOStream {
public:
  // InStream Constructor
  static InStream *New(FILE *file, String *name) {
    return (InStream *) IOStream::New(IO_IN, file, name);
  }
  // InStream Untagging
  static InStream *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER || p->GetLabel() == (BlockLabel) IO_IN);
    return (InStream *) p;
  }
};

class OutStream : public IOStream {
public:
  // OutStream Constructor
  static OutStream *New(FILE *file, String *name) {
    return (OutStream *) IOStream::New(IO_OUT, file, name);
  }
  // OutStream Untagging
  static OutStream *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER || p->GetLabel() == (BlockLabel) IO_OUT);
    return (OutStream *) p;
  }
};

#define DECLARE_INSTREAM(file, x) DECLARE_BLOCKTYPE(InStream, file, x);
#define DECLARE_OUTSTREAM(file, x) DECLARE_BLOCKTYPE(OutStream, file, x);

// String Handling
static String *ExportString(String *s) {
  u_int sLen = s->GetSize();
  String *e  = String::New(sLen + 1);
  char *eb   = e->GetValue();
  memcpy(eb, s->GetValue(), sLen);
  eb[sLen] = 0x00;
  return e;
}

static String *Concat(String *a, char *b, u_int bLen) {
  u_int aLen = a->GetSize();
  String *s  = String::New(aLen + bLen);
  char *sb   = s->GetValue();
  memcpy(sb, a->GetValue(), aLen);
  memcpy(sb + aLen, b, bLen);
  return s;
}

static word IoConstructor;

#define RAISE_IO_EXCEPTION(cause, function, name)		\
  ConVal *conVal =						\
    ConVal::New(Constructor::FromWordDirect(IoConstructor), 3);	\
  conVal->Init(0, cause);					\
  conVal->Init(1, String::New(function)->ToWord());		\
  conVal->Init(1, name->ToWord());				\
  Scheduler::currentData = conVal->ToWord();			\
  return Interpreter::RAISE;

DEFINE3(UnsafeIO_Io) {
  ConVal *conVal = ConVal::New(Constructor::FromWordDirect(IoConstructor), 3);
  conVal->Init(0, x0);
  conVal->Init(1, x1);
  conVal->Init(2, x2);
  RETURN(conVal->ToWord());
} END

DEFINE1(UnsafeIO_closeIn) {
  DECLARE_INSTREAM(stream, x0);
  fclose(stream->GetStream());
  RETURN_UNIT;
} END

DEFINE1(UnsafeIO_closeOut) {
  DECLARE_OUTSTREAM(stream, x0);
  fclose(stream->GetStream());
  RETURN_UNIT;
} END

DEFINE1(UnsafeIO_flushOut) {
  DECLARE_OUTSTREAM(stream, x0);
  fflush(stream->GetStream());
  RETURN_UNIT;
} END

DEFINE1(UnsafeIO_inputAll) {
  static char *buf = (char *) malloc(sizeof(char) * 8192);
  DECLARE_INSTREAM(stream, x0);
  FILE *file = stream->GetStream();
  String *b  = String::New((u_int) 0);
  while (!feof(file)) {
    u_int rdBytes = fread(buf, 1, 8192, file);
    if (ferror(file)) {
      RAISE_IO_EXCEPTION(Store::IntToWord(0), "inputAll", stream->GetName());
    }
    b = Concat(b, buf, rdBytes);
  }
  RETURN(b->ToWord());
} END

DEFINE1(UnsafeIO_inputLine) {
  //--** to be done: raise Size if > String::maxSize
  static char *buf = (char *) malloc(sizeof(char) * 1024);
  DECLARE_INSTREAM(stream, x0);
  FILE *file = stream->GetStream();
  String *b  = String::New((u_int) 0);
  u_int stop = 0;
  while (!stop) {
    u_int rdBytes = fread(buf, 1, 1024, file);
    if (ferror(file)) {
      RAISE_IO_EXCEPTION(Store::IntToWord(0), "inputLine", stream->GetName());
    }
    u_int seek    = 0;
    while (!stop && (seek < rdBytes)) {
      if (buf[seek++] == '\n') {
	stop = 1;
      }
    }
    b = Concat(b, buf, seek);
    if (seek < rdBytes) {
      fseek(file, rdBytes - seek, SEEK_CUR);
    }
  }
  RETURN(b->ToWord());
} END

DEFINE2(UnsafeIO_openAppend) {
  DECLARE_BOOL(b, x0);
  DECLARE_STRING(s, x1);
  const char *flags = (b ? "wab" : "wa");
  String *name = ExportString(s);
  FILE *file   = fopen(name->GetValue(), flags);
  if (file != NULL) {
    RETURN(OutStream::New(file, s)->ToWord());
  }
  else {
    RAISE_IO_EXCEPTION(Store::IntToWord(0), "openAppend", s);
  }
} END

DEFINE2(UnsafeIO_openIn) {
  DECLARE_BOOL(b, x0);
  DECLARE_STRING(s, x1);
  const char *flags = (b ? "rb" : "r");
  String *name = ExportString(s);
  FILE *file   = fopen(name->GetValue(), flags);
  if (file != NULL) {
    RETURN(InStream::New(file, s)->ToWord());
  }
  else {
    RAISE_IO_EXCEPTION(Store::IntToWord(0), "openIn", s);
  }
} END

DEFINE2(UnsafeIO_openOut) {
  DECLARE_BOOL(b, x0);
  DECLARE_STRING(s, x1);
  const char *flags = (b ? "wb" : "w");
  String *name = ExportString(s);
  FILE *file   = fopen(name->GetValue(), flags);
  if (file != NULL) {
    RETURN(OutStream::New(file, s)->ToWord());
  }
  else {
    RAISE_IO_EXCEPTION(Store::IntToWord(0), "openOut", s);
  }
} END

DEFINE2(UnsafeIO_output) {
  DECLARE_OUTSTREAM(stream, x0);
  DECLARE_STRING(s, x1);
  u_int wrtBytes = s->GetSize();
  u_int nbBytes  = fwrite(s->GetValue(), 1, wrtBytes, stream->GetStream());
  if (nbBytes == wrtBytes) {
    RETURN_UNIT;
  }
  else {
    RAISE_IO_EXCEPTION(Store::IntToWord(0), "output", stream->GetName());
  }
} END

DEFINE2(UnsafeIO_output1) {
  DECLARE_OUTSTREAM(stream, x0);
  DECLARE_INT(c, x1);
  u_int nbBytes = fwrite(&c, 1, 1, stream->GetStream());
  if (nbBytes == 1) {
    RETURN_UNIT;
  }
  else {
    RAISE_IO_EXCEPTION(Store::IntToWord(0), "output1", stream->GetName());
  }
} END

DEFINE1(UnsafeIO_print) {
  DECLARE_STRING(s, x0);
  fprintf(stdout, "%.*s", (int) s->GetSize(), s->GetValue());
  fflush(stdout);
  RETURN_UNIT;
} END

word UnsafeIO(void) {
  IoConstructor = UniqueConstructor::New(String::New("IO.Io"))->ToWord();
  RootSet::Add(IoConstructor);

  Tuple *t = Tuple::New(16);
  t->Init(0, IoConstructor);
  t->Init(1, Primitive::MakeClosure("UnsafeIO_Io", UnsafeIO_Io, 3));
  t->Init(2, Primitive::MakeClosure("UnsafeIO_closeIn", UnsafeIO_closeIn, 1));
  t->Init(3, Primitive::MakeClosure("UnsafeIO_closeOut", UnsafeIO_closeOut, 1));
  t->Init(4, Primitive::MakeClosure("UnsafeIO_flushOut", UnsafeIO_flushOut, 1));
  t->Init(5, Primitive::MakeClosure("UnsafeIO_inputAll", UnsafeIO_inputAll, 1));
  t->Init(6, Primitive::MakeClosure("UnsafeIO_inputLine", UnsafeIO_inputLine, 1));
  t->Init(7, Primitive::MakeClosure("UnsafeIO_openAppend", UnsafeIO_openAppend, 2));
  t->Init(8, Primitive::MakeClosure("UnsafeIO_openIn", UnsafeIO_openIn, 2));
  t->Init(9, Primitive::MakeClosure("UnsafeIO_openOut", UnsafeIO_openOut, 2));
  t->Init(10, Primitive::MakeClosure("UnsafeIO_output", UnsafeIO_output, 2));
  t->Init(11, Primitive::MakeClosure("UnsafeIO_output1", UnsafeIO_output1, 2));
  t->Init(12, Primitive::MakeClosure("UnsafeIO_print", UnsafeIO_print, 1));
  t->Init(13, OutStream::New(stderr, String::New("stderr"))->ToWord());
  t->Init(14, InStream::New(stdin, String::New("stdin"))->ToWord());
  t->Init(15, OutStream::New(stdout, String::New("stdout"))->ToWord());
  RETURN_STRUCTURE(t);
}
