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
#include "emulator/Authoring.hh"

typedef enum {
  IO_IN  = MIN_DATA_LABEL,
  IO_OUT = (IO_IN + 1)
} IOStreamType;

// Builtin IoStream Classes
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
    Assert(p != INVALID_POINTER && p->GetLabel() == (BlockLabel) IO_IN);
    return (InStream *) p;
  }
};

class OutStream : public IOStream {
public:
  // OutStream Constructor
  static OutStream *New(FILE *file, String *name) {
    return (OutStream *) IOStream::New(IO_IN, file, name);
  }
  // OutStream Untagging
  static OutStream *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p != INVALID_POINTER && p->GetLabel() == (BlockLabel) IO_OUT);
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

DEFINE1(UnsafeIO_PrimeIo) {
  // to be done
  RETURN_UNIT;
} END

DEFINE1(UnsafeIO_Io) {
  // to be done
  RETURN_UNIT;
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
    b = Concat(b, buf, rdBytes);
  }
  RETURN(b->ToWord());
} END

DEFINE1(UnsafeIO_inputLine) {
  static char *buf = (char *) malloc(sizeof(char) * 1024);
  DECLARE_INSTREAM(stream, x0);
  FILE *file = stream->GetStream();
  String *b  = String::New((u_int) 0);
  u_int stop = 0;
  while (!stop) {
    u_int rdBytes = fread(buf, 1, 1024, file);
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
  DECLARE_INT(b, x0);
  DECLARE_STRING(s, x1);
  const char *flags = (b ? "wab" : "wa"); 
  String *name = ExportString(s);
  FILE *file   = fopen(name->GetValue(), flags);
  if (file != NULL) {
    RETURN(OutStream::New(file, s)->ToWord());
  }
  else {
    // to be done: proper Exception
    Scheduler::currentData = s->ToWord();
    return Interpreter::RAISE;
  }
} END

DEFINE2(UnsafeIO_openIn) {
  DECLARE_INT(b, x0);
  DECLARE_STRING(s, x1);
  const char *flags = (b ? "rb" : "r"); 
  String *name = ExportString(s);
  FILE *file   = fopen(name->GetValue(), flags);
  if (file != NULL) {
    RETURN(InStream::New(file, s)->ToWord());
  }
  else {
    // to be done: proper Exception
    Scheduler::currentData = s->ToWord();
    return Interpreter::RAISE;
  }
} END

DEFINE2(UnsafeIO_openOut) {
  DECLARE_INT(b, x0);
  DECLARE_STRING(s, x1);
  const char *flags = (b ? "wb" : "w"); 
  String *name = ExportString(s);
  FILE *file   = fopen(name->GetValue(), flags);
  if (file != NULL) {
    RETURN(OutStream::New(file, s)->ToWord());
  }
  else {
    // to be done: proper Exception
    Scheduler::currentData = s->ToWord();
    return Interpreter::RAISE;
  }
} END

DEFINE2(UnsafeIO_output) {
  DECLARE_OUTSTREAM(stream, x0);
  DECLARE_STRING(s, x1);
  u_int wrtBytes = s->GetSize();
  u_int nbBytes  = fwrite(s->GetValue(), 1, wrtBytes, stream->GetStream());
  if (nbBytes == wrtBytes) {
    RETURN(Interpreter::EmptyArg());
  }
  else {
    // to be done: proper Exception
    Scheduler::currentData = Store::IntToWord(0);
    return Interpreter::RAISE;
  }
} END

DEFINE2(UnsafeIO_output1) {
  DECLARE_OUTSTREAM(stream, x0);
  DECLARE_INT(c, x1);
  u_int nbBytes = fwrite(&c, 1, 1, stream->GetStream());
  if (nbBytes == 1) {
    RETURN(Interpreter::EmptyArg());
  }
  else {
    // to be done: proper Exception
    Scheduler::currentData = Store::IntToWord(0);
    return Interpreter::RAISE;
  }
} END

DEFINE1(UnsafeIO_print) {
  DECLARE_STRING(s, x0);
  fprintf(stdout, "%.*s", s->GetSize(), s->GetValue());
  fflush(stdout);
  RETURN_UNIT;
} END

DEFINE1(UnsafeIO_stdErr) {
  RETURN(OutStream::New(stderr, String::New("stderr"))->ToWord());
} END

DEFINE1(UnsafeIO_stdIn) {
  RETURN(InStream::New(stdin, String::New("stdin"))->ToWord());
} END

DEFINE1(UnsafeIO_stdOut) {
  RETURN(OutStream::New(stdout, String::New("stdout"))->ToWord());
} END

word UnsafeIO(void) {
  Tuple *t = Tuple::New(16);
  t->Init(0, Primitive::MakeFunction(UnsafeIO_PrimeIo, 1));
  t->Init(1, Primitive::MakeFunction(UnsafeIO_Io, 1));
  t->Init(2, Primitive::MakeFunction(UnsafeIO_closeIn, 1));
  t->Init(3, Primitive::MakeFunction(UnsafeIO_closeOut, 1));
  t->Init(4, Primitive::MakeFunction(UnsafeIO_flushOut, 1));
  t->Init(5, Primitive::MakeFunction(UnsafeIO_inputAll, 1));
  t->Init(6, Primitive::MakeFunction(UnsafeIO_inputLine, 1));
  t->Init(7, Primitive::MakeFunction(UnsafeIO_openAppend, 2));
  t->Init(8, Primitive::MakeFunction(UnsafeIO_openIn, 2));
  t->Init(9, Primitive::MakeFunction(UnsafeIO_openOut, 2));
  t->Init(10, Primitive::MakeFunction(UnsafeIO_output, 2));
  t->Init(11, Primitive::MakeFunction(UnsafeIO_output1, 2));
  t->Init(12, Primitive::MakeFunction(UnsafeIO_print, 1));
  t->Init(13, Primitive::MakeFunction(UnsafeIO_stdErr, 1));
  t->Init(14, Primitive::MakeFunction(UnsafeIO_stdIn, 1));
  t->Init(15, Primitive::MakeFunction(UnsafeIO_stdOut, 1));
  return t->ToWord();
}
