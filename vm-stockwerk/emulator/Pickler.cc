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

#if defined(INTERFACE)
#pragma implementation "emulator/Pickler.hh"
#endif

#include <cstdio>
#include <cstdlib>
#include "adt/HashTable.hh"
#include "emulator/Interpreter.hh"
#include "emulator/Scheduler.hh"
#include "emulator/Backtrace.hh"
#include "emulator/TaskStack.hh"
#include "emulator/Pickler.hh"
#include "emulator/Closure.hh"
#include "emulator/ConcreteCode.hh"
#include "emulator/Transform.hh"
#include "emulator/Tuple.hh"
#include "emulator/Transients.hh"
#include "emulator/PrimitiveTable.hh"
#include "emulator/AbstractCodeInterpreter.hh"
#include "emulator/Alice.hh"
#include "emulator/RootSet.hh"
#include "emulator/Debug.hh"

// pickle    ::= int | chunk | block | tuple | closure | transform
// int       ::= POSINT <uint> | NEGINT <uint>
// chunk     ::= CHUNK size <byte>*size
// size      ::= <uint>
// block     ::= BLOCK label size field*size
// tuple     ::= TUPLE size field*size
// closure   ::= CLOSURE size field*size
// label     ::= <uint>
// field     ::= pickle | reference
// reference ::= REF id
// id        ::= <uint>
// transform ::= TRANSFORM (chunk|reference) field

// Very convienent Macro
#define CONTINUE(args)           \
  Scheduler::currentArgs = args; \
  return Interpreter::CONTINUE;

//
// Streaming Classes
//
class OutputStream {
public:
  virtual void PutByte(u_int byte) = 0;
  virtual void PutBytes(Chunk *c)  = 0;
  virtual word Close()             = 0;
  void PutUInt(u_int i) {
    while (i >= 0x80) {
      PutByte(i % 0x80 + 0x80);
      i = i / 0x80;
    }
    PutByte(i);
  }
  // Store Interface
  word ToWord() {
    return Store::UnmanagedPointerToWord(this);
  }
  static OutputStream *FromWord(word stream) {
    return static_cast<OutputStream *>(Store::WordToUnmanagedPointer(stream));
  }
};

class FileOutputStream : public OutputStream {
private:
  u_int exception;
  FILE *file;
public:
  // FileOutputStream Constructor
  FileOutputStream(char *filename) {
    file      = fopen(filename, "wb");
    exception = (file == NULL);
  }
  // FileOutputStream Functions
  u_int GotException() {
    return exception;
  }
  virtual void PutByte(u_int byte) {
    fwrite(&byte, 1, 1, file); // to be done: exception stuff
  }
  virtual void PutBytes(Chunk *c) {
    fwrite(c->GetBase(), 1, c->GetSize(), file);
  }
  virtual word Close() {
    fclose(file);
    return Store::IntToWord(0);
  }
};

class StringOutputStream : public OutputStream {
private:
  static const u_int INITIAL_SIZE = 256;
  u_int pos;
  u_int size;
  char *str;
  void Enlarge() {
    size = (size * 3) >> 1;
    char *newStr = (char *) malloc(sizeof(char) * size);
    memcpy(newStr, str, pos);
    free(str);
    str = newStr;
  }
public:
  // StringOutputStream Constructor
  StringOutputStream() : pos(0), size(INITIAL_SIZE) {
    str = (char *) malloc(sizeof(char) * size);
  }
  // StringOutputStream Functions
  virtual void PutByte(u_int byte) {
    str[pos++] = (char) byte;
    if (pos == size) {
      Enlarge();
    }
  }
  virtual void PutBytes(Chunk *c) {
    u_int cSize = c->GetSize();
    while (pos + cSize >= size) {
      Enlarge();
    }
    memcpy(str + pos, c->GetBase(), cSize);
    pos += cSize;
  }
  virtual word Close() {
    word result = String::New(str, pos)->ToWord();
    free(str);
    return result;

  }
};

// PicklingArgs
class PicklingArgs : private Block {
private:
  static const u_int ID_POS     = 0;
  static const u_int STREAM_POS = 1;
  static const u_int SEEN_POS   = 2;
  static const u_int SIZE       = 3;
public:
  using Block::ToWord;
  // PicklingArgs Accessors
  int GetId() {
    return Store::DirectWordToInt(GetArg(ID_POS));
  }
  OutputStream *GetStream() {
    return OutputStream::FromWord(GetArg(STREAM_POS));
  }
  word GetSeen() {
    return GetArg(SEEN_POS);
  }
  // PicklingArgs Constructor
  static PicklingArgs *New(int id, OutputStream *stream, word seen) {
    Block *p = Store::AllocBlock(TUPARGS_LABEL, SIZE);
    p->InitArg(ID_POS, id);
    p->InitArg(STREAM_POS, stream->ToWord());
    p->InitArg(SEEN_POS, seen);
    return static_cast<PicklingArgs *>(p);
  }
  // PicklingArgs Untagging
  static PicklingArgs *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p != INVALID_POINTER && p->GetLabel() == TUPARGS_LABEL);
    return static_cast<PicklingArgs *>(p);
  }
};

// PicklingInterpreter
class PicklingInterpreter : public Interpreter {
private:
// Pickle Tags
  class Tag {
  public:
    enum PickleTags {
      POSINT,
      NEGINT,
      CHUNK,
      BLOCK,
      TUPLE,
      CLOSURE,
      REF,
      TRANSFORM
    };
  };
  static PicklingInterpreter *self;
public:
  // PicklingInterpreter Constructor
  PicklingInterpreter() : Interpreter() {}
  // PicklingInterpreter Static Constructor
  static void Init() {
    self = new PicklingInterpreter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack, word data);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

// PicklingInterpreter Frame
class PicklingFrame : private StackFrame {
private:
  static const u_int DATA_POS = 0;
  static const u_int SIZE     = 1;
public:
  using Block::ToWord;
  // PicklingFrame Accessors
  word GetData() {
    return StackFrame::GetArg(DATA_POS);
  }
  // PicklingFrame Constructor
  static PicklingFrame *New(Interpreter *interpreter, word data) {
    StackFrame *frame = StackFrame::New(PICKLING_FRAME, interpreter, SIZE);
    frame->InitArg(DATA_POS, data);
    return static_cast<PicklingFrame *>(frame);
  }
  // PicklingFrame Untagging
  static PicklingFrame *FromWordDirect(word frame) {
    Block *p = Store::DirectWordToBlock(frame);
    Assert(p != INVALID_POINTER &&
	   p->GetLabel() == (BlockLabel) PICKLING_FRAME);
    return static_cast<PicklingFrame *>(p);
  }
};

//
// PicklingInterpreter Functions
//
PicklingInterpreter *PicklingInterpreter::self;

void PicklingInterpreter::PushFrame(TaskStack *taskStack, word data) {
  taskStack->PushFrame(PicklingFrame::New(self, data)->ToWord());
}

// to be done
static word AddToSeen(word seen, Block *v, u_int id) {
  Tuple *t = Tuple::New(3);
  t->Init(0, Store::IntToWord(id));
  t->Init(1, v->ToWord());
  t->Init(2, seen);
  return t->ToWord();
}

static int FindRef(word x0, word seen) {
  Tuple *cons = Tuple::FromWord(seen);
  while (cons != INVALID_POINTER) {
    if (cons->Sel(1) == x0) {
      return Store::DirectWordToInt(cons->Sel(0));
    }
    cons = Tuple::FromWord(cons->Sel(2));
  }
  return -1;
}

Interpreter::Result PicklingInterpreter::Run(word args, TaskStack *taskStack) {
  PicklingFrame *frame = PicklingFrame::FromWordDirect(taskStack->GetFrame());
  word x0              = frame->GetData();
  if (Store::WordToTransient(x0) != INVALID_POINTER) {
    Scheduler::currentData = x0;
    return Interpreter::REQUEST;
  }
  taskStack->PopFrame();
  PicklingArgs *pargs        = PicklingArgs::FromWord(args);
  int id                     = pargs->GetId();
  OutputStream *outputStream = pargs->GetStream();
  word seen                  = pargs->GetSeen();
  // Check for integer
  int i;
  if ((i = Store::DirectWordToInt(x0)) != INVALID_INT) {
    if (i >= 0) {
      outputStream->PutByte(Tag::POSINT);
      outputStream->PutUInt(i);
    }
    else {
      outputStream->PutByte(Tag::NEGINT);
      outputStream->PutUInt(-(i + 1));
    }
    CONTINUE(args);
  }
  // Search for already known value
  int ref = FindRef(x0, seen);
  if (ref != -1) { // -1 = unit
    outputStream->PutByte(Tag::REF);
    outputStream->PutUInt((u_int) ref);
    CONTINUE(args);
  }
  // Handle new Block Value (non-abstract use)
  Block *v     = Store::WordToBlock(x0);
  BlockLabel l = v->GetLabel();
  switch (l) {
  case CHUNK_LABEL:
    {
      Chunk *c = (Chunk *) v;
      outputStream->PutByte(Tag::CHUNK);
      outputStream->PutUInt(c->GetSize());
      outputStream->PutBytes(c);
      seen = AddToSeen(seen, v, id);
      CONTINUE(PicklingArgs::New(id + 1, outputStream, seen)->ToWord());
    }
    break;
  case TUPLE_LABEL:
    {
      u_int size = v->GetSize(); // to be done 
      Tuple *t   = (Tuple *) v;
      outputStream->PutByte(Tag::TUPLE);
      outputStream->PutUInt(size);
      seen = AddToSeen(seen, v, id);
      for (u_int i = size; i--;) {
	PicklingInterpreter::PushFrame(taskStack, t->Sel(i));
      }
      CONTINUE(PicklingArgs::New(id + 1, outputStream, seen)->ToWord());
    }
    break;
  case CLOSURE_LABEL:
    {
      u_int size = v->GetSize(); // to be done
      outputStream->PutByte(Tag::CLOSURE);
      outputStream->PutUInt(size);
      seen = AddToSeen(seen, v, id);
      for (u_int i = size; i--;) {
	PicklingInterpreter::PushFrame(taskStack, v->GetArg(i));
      }
      CONTINUE(PicklingArgs::New(id + 1, outputStream, seen)->ToWord());
    }
    break;
  case HANDLERBLOCK_LABEL:
    {
      Transform *transform =
	reinterpret_cast<Transform *>(v->GetHandler()->
				      GetAbstractRepresentation(v));
      if (transform == INVALID_POINTER) {
	Scheduler::currentData      = Pickler::Sited;
	Scheduler::currentBacktrace = Backtrace::New(frame->ToWord());
	return Interpreter::RAISE;
      }
      else {
	PicklingInterpreter::PushFrame(taskStack, transform->ToWord());
	CONTINUE(args);
      }
    }
    break;
  case TRANSFORM_LABEL:
    {
      Transform *transform = reinterpret_cast<Transform *>(v);
      outputStream->PutByte(Tag::TRANSFORM);
      seen = AddToSeen(seen, v, id);
      PicklingInterpreter::PushFrame(taskStack, transform->GetArgument());
      PicklingInterpreter::PushFrame(taskStack, transform->GetName()->ToWord());
      CONTINUE(PicklingArgs::New(id + 1, outputStream, seen)->ToWord());
    }
    break;
  default:
    u_int size = v->GetSize(); // to be done
    outputStream->PutByte(Tag::BLOCK);
    outputStream->PutUInt(l);
    seen = AddToSeen(seen, v, id);
    for (u_int i = size; i--;) {
      PicklingInterpreter::PushFrame(taskStack, v->GetArg(i));
    }
    CONTINUE(PicklingArgs::New(id + 1, outputStream, seen)->ToWord());
  }
}

const char *PicklingInterpreter::Identify() {
  return "PicklingInterpreter";
}

void PicklingInterpreter::DumpFrame(word frame) {
  PicklingFrame *pframe = PicklingFrame::FromWordDirect(frame);
  fprintf(stderr, "Pickling Value:\n");
  Debug::Dump(pframe->GetData());
}

// PicklePackInterpreter Frame
class PicklePackFrame : private StackFrame {
private:
  static const u_int SIZE = 0;
public:
  using Block::ToWord;
  // PicklePackFrame Constructor
  static PicklePackFrame *New(Interpreter *interpreter) {
    StackFrame *frame = StackFrame::New(PICKLE_PACK_FRAME, interpreter, SIZE);
    return static_cast<PicklePackFrame *>(frame);
  }
  // PicklePackFrame Untagging
  static PicklePackFrame *FromWordDirect(word frame) {
    Block *p = Store::WordToBlock(frame);
    Assert(p != INVALID_POINTER &&
	   p->GetLabel() == (BlockLabel) PICKLE_PACK_FRAME);
    return static_cast<PicklePackFrame *>(p);
  }
};

// PicklePack Interpreter
class PicklePackInterpreter : public Interpreter {
private:
  static PicklePackInterpreter *self;
public:
  // PicklePackInterpreter Constructor
  PicklePackInterpreter() : Interpreter() {}
  // PicklePackInterpreter Static Constructor
  static void Init() {
    self = new PicklePackInterpreter();
  }
  // Frame Handing
  static void PushFrame(TaskStack *taskStack);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// PicklePackInterpreter Functions
//
PicklePackInterpreter *PicklePackInterpreter::self;

void PicklePackInterpreter::PushFrame(TaskStack *taskStack) {
  taskStack->PushFrame(PicklePackFrame::New(self)->ToWord());
}

Interpreter::Result
PicklePackInterpreter::Run(word args, TaskStack *taskStack) {
  PicklingArgs *pargs = PicklingArgs::FromWord(args);
  OutputStream *os    = pargs->GetStream();
  taskStack->PopFrame();
  CONTINUE(Interpreter::OneArg(os->Close()));
}

const char *PicklePackInterpreter::Identify() {
  return "PicklePackInterpreter";
}

void PicklePackInterpreter::DumpFrame(word frame) {
  frame = frame;
  fprintf(stderr, "PicklePackInterpreter");
}

// PickleSaveInterpreter Frame
class PickleSaveFrame : private StackFrame {
private:
  static const u_int SIZE = 0;
public:
  using Block::ToWord;
  // PickleSaveFrame Constructor
  static PickleSaveFrame *New(Interpreter *interpreter) {
    StackFrame *frame = StackFrame::New(PICKLE_SAVE_FRAME, interpreter, SIZE);
    return static_cast<PickleSaveFrame *>(frame);
  }
  // PickleSaveFrame Untagging
  static PickleSaveFrame *FromWordDirect(word frame) {
    Block *p = Store::WordToBlock(frame);
    Assert(p != INVALID_POINTER &&
	   p->GetLabel() == (BlockLabel) PICKLE_SAVE_FRAME);
    return static_cast<PickleSaveFrame *>(p);
  }
};

// PickleSaveInterpreter
class PickleSaveInterpreter : public Interpreter {
private:
  static PickleSaveInterpreter *self;
public:
  // PickleSaveInterpreter Constructor
  PickleSaveInterpreter() : Interpreter() {}
  // PickleSaveInterpreter Static Constructor
  static void Init() {
    self = new PickleSaveInterpreter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// PickleSaveInterpreter Interpreter Functions
//
PickleSaveInterpreter *PickleSaveInterpreter::self;

void PickleSaveInterpreter::PushFrame(TaskStack *taskStack) {
  taskStack->PushFrame(PickleSaveFrame::New(self)->ToWord());
}

Interpreter::Result
PickleSaveInterpreter::Run(word args, TaskStack *taskStack) {
  PickleSaveFrame *frame =
    PickleSaveFrame::FromWordDirect(taskStack->GetFrame());
  taskStack->PopFrame();
  PicklingArgs *pargs        = PicklingArgs::FromWord(args);
  OutputStream *outputStream = pargs->GetStream();
  outputStream->Close();
  CONTINUE(Interpreter::EmptyArg());
}

const char *PickleSaveInterpreter::Identify() {
  return "PickleSaveInterpreter";
}

void PickleSaveInterpreter::DumpFrame(word frame) {
  frame = frame;
  fprintf(stderr, "PickleSaveInterpreter\n");
}

//
// Pickler Functions
//
word Pickler::Sited;

// String Handling: to be done
static char *ExportString(Chunk *s) {
  u_int sLen = s->GetSize();
  Chunk *e   = Store::AllocChunk(sLen + 1);
  char *eb   = e->GetBase();
  memcpy(eb, s->GetBase(), sLen);
  eb[sLen] = '\0';
  return eb;
}

Interpreter::Result Pickler::Pack(word x, TaskStack *taskStack) {
  StringOutputStream *os = new StringOutputStream();
  taskStack->PopFrame();
  PicklePackInterpreter::PushFrame(taskStack);
  PicklingInterpreter::PushFrame(taskStack, x);
  CONTINUE(PicklingArgs::New(0, os, Store::IntToWord(0))->ToWord());
}

Interpreter::Result
Pickler::Save(Chunk *filename, word x, TaskStack *taskStack) {
  char *szFileName     = ExportString(filename);
  FileOutputStream *os = new FileOutputStream(szFileName);
  if (os->GotException()) {
    delete os;
    Scheduler::currentData      = Store::IntToWord(0); // to be done
    Scheduler::currentBacktrace = Backtrace::New(taskStack->GetFrame());
    taskStack->PopFrame();
    return Interpreter::RAISE;
  }
  taskStack->PopFrame();
  PickleSaveInterpreter::PushFrame(taskStack);
  PicklingInterpreter::PushFrame(taskStack, x);
  CONTINUE(PicklingArgs::New(0, os, Store::IntToWord(0))->ToWord());
}

void Pickler::Init() {
  Sited = UniqueConstructor::New(String::New("Component.Sited"))->ToWord();
  RootSet::Add(Sited);
  PicklingInterpreter::Init();
  PicklePackInterpreter::Init();
  PickleSaveInterpreter::Init();
}
