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
#include "emulator/RootSet.hh"
#include "emulator/Tuple.hh"
#include "emulator/ConcreteCode.hh"
#include "emulator/Closure.hh"
#include "emulator/Backtrace.hh"
#include "emulator/TaskStack.hh"
#include "emulator/Transients.hh"
#include "emulator/Interpreter.hh"
#include "emulator/Scheduler.hh"
#include "emulator/Transform.hh"
#include "emulator/Pickler.hh"

#include "emulator/Debug.hh" //--** remove

//--** this should be factored out:
#include "emulator/Alice.hh"

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

#define CONTINUE(args)				\
  Scheduler::currentArgs = args;		\
  return Interpreter::CONTINUE;

//
// Streaming Classes
//
class OutputStream {
public:
  virtual void PutByte(u_char byte) = 0;
  virtual void PutBytes(Chunk *c) = 0;
  void PutUInt(u_int i) {
    while (i >= 0x80) {
      PutByte(i & 0x7F | 0x80);
      i >>= 7;
    }
    PutByte(i);
  }
  // Store Interface
  word ToWord() {
    return Store::UnmanagedPointerToWord(this);
  }
  static OutputStream *FromWordDirect(word stream) {
    return static_cast<OutputStream *>
      (Store::DirectWordToUnmanagedPointer(stream));
  }
};

class FileOutputStream : public OutputStream { //--** finalization to be done
private:
  std::FILE *file;
  bool exception;
public:
  // FileOutputStream Constructor
  FileOutputStream(char *filename) : OutputStream() {
    file      = std::fopen(filename, "wb");
    exception = (file == NULL);
  }
  // FileOutputStream Functions
  bool GotException() {
    return exception;
  }
  virtual void PutByte(u_char byte) {
    std::fputc(byte, file); //--** to be done: I/O exceptions
  }
  virtual void PutBytes(Chunk *c) {
    std::fwrite(c->GetBase(), 1, c->GetSize(), file);
  }
  void Close() {
    std::fclose(file);
  }
};

class StringOutputStream : public OutputStream {
private:
  static const u_int INITIAL_SIZE = 256;
  u_int pos;
  u_int size;
  u_char *str;
  void Enlarge() {
    size = (size * 3) >> 1;
    u_char *newStr = static_cast<u_char *>(std::malloc(sizeof(u_char) * size));
    std::memcpy(newStr, str, pos);
    std::free(str);
    str = newStr;
  }
public:
  // StringOutputStream Constructor
  StringOutputStream() : pos(0), size(INITIAL_SIZE) {
    str = static_cast<u_char *>(malloc(sizeof(u_char) * size));
  }
  // StringOutputStream Functions
  virtual void PutByte(u_char byte) {
    str[pos++] = byte;
    if (pos == size) {
      Enlarge();
    }
  }
  virtual void PutBytes(Chunk *c) {
    u_int cSize = c->GetSize();
    while (pos + cSize >= size) {
      Enlarge();
    }
    std::memcpy(str + pos, c->GetBase(), cSize);
    pos += cSize;
  }
  word Close() {
    Chunk *chunk = Store::AllocChunk(pos);
    std::memcpy(chunk->GetBase(), str, pos);
    std::free(str);
    return chunk->ToWord();
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
    return OutputStream::FromWordDirect(GetArg(STREAM_POS));
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
  static PicklingArgs *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert(p->GetLabel() == TUPARGS_LABEL);
    return static_cast<PicklingArgs *>(p);
  }
};

// PicklingInterpreter
class PicklingInterpreter : public Interpreter {
private:
  // Pickle Tags
  class Tag { //--** move to Pickle.hh
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
  // PicklingInterpreter Constructor
  PicklingInterpreter() : Interpreter() {}
public:
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

  // PicklingFrame Constructor
  static PicklingFrame *New(Interpreter *interpreter, word data) {
    StackFrame *frame = StackFrame::New(PICKLING_FRAME, interpreter, SIZE);
    frame->InitArg(DATA_POS, data);
    return static_cast<PicklingFrame *>(frame);
  }
  // PicklingFrame Untagging
  static PicklingFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == PICKLING_FRAME);
    return static_cast<PicklingFrame *>(p);
  }

  // PicklingFrame Accessors
  word GetData() {
    return StackFrame::GetArg(DATA_POS);
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

static u_int FindRef(Block *v, word seen) {
  Tuple *cons = Tuple::FromWord(seen);
  while (cons != INVALID_POINTER) {
    if (Store::DirectWordToBlock(cons->Sel(1)) == v) {
      return Store::DirectWordToInt(cons->Sel(0));
    }
    cons = Tuple::FromWord(cons->Sel(2));
  }
  return static_cast<u_int>(-1);
}

Interpreter::Result PicklingInterpreter::Run(word args, TaskStack *taskStack) {
  PicklingFrame *frame = PicklingFrame::FromWordDirect(taskStack->GetFrame());
  word x0              = frame->GetData();
  if (Store::WordToTransient(x0) != INVALID_POINTER) {
    Scheduler::currentData = x0;
    return Interpreter::REQUEST;
  }
  taskStack->PopFrame();
  PicklingArgs *pargs        = PicklingArgs::FromWordDirect(args);
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
  Block *v  = Store::WordToBlock(x0);
  u_int ref = FindRef(v, seen);
  if (ref != static_cast<u_int>(-1)) {
    outputStream->PutByte(Tag::REF);
    outputStream->PutUInt(ref);
    CONTINUE(args);
  }
  // Handle new Block Value (non-abstract use)
  BlockLabel l = v->GetLabel();
  switch (l) {
  case CHUNK_LABEL:
    {
      Chunk *c = static_cast<Chunk *>(v);
      outputStream->PutByte(Tag::CHUNK);
      outputStream->PutUInt(c->GetSize());
      outputStream->PutBytes(c);
      seen = AddToSeen(seen, v, id);
      CONTINUE(PicklingArgs::New(id + 1, outputStream, seen)->ToWord());
    }
    break;
  case TUPLE_LABEL:
    {
      u_int size = v->GetSize();
      outputStream->PutByte(Tag::TUPLE);
      outputStream->PutUInt(size);
      seen = AddToSeen(seen, v, id);
      for (u_int i = size; i--;) {
	PicklingInterpreter::PushFrame(taskStack, v->GetArg(i));
      }
      CONTINUE(PicklingArgs::New(id + 1, outputStream, seen)->ToWord());
    }
    break;
  case CLOSURE_LABEL:
    {
      u_int size = v->GetSize();
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
      Block *block = v->GetHandler()->GetAbstractRepresentation(v);
      if (block == INVALID_POINTER) {
	Scheduler::currentData      = Pickler::Sited;
	Scheduler::currentBacktrace = Backtrace::New(frame->ToWord());
	return Interpreter::RAISE;
      } else {
	std::fprintf(stderr, "handlerblock: label %d\n", block->GetLabel());
	Assert(block->GetLabel() == TRANSFORM_LABEL);
	PicklingInterpreter::PushFrame(taskStack, block->ToWord());
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
    {
      u_int size = v->GetSize(); // to be done
      outputStream->PutByte(Tag::BLOCK);
      outputStream->PutUInt(l);
      outputStream->PutUInt(size);
      seen = AddToSeen(seen, v, id);
      for (u_int i = size; i--;) {
	PicklingInterpreter::PushFrame(taskStack, v->GetArg(i));
      }
      CONTINUE(PicklingArgs::New(id + 1, outputStream, seen)->ToWord());
    }
  }
}

const char *PicklingInterpreter::Identify() {
  return "PicklingInterpreter";
}

void PicklingInterpreter::DumpFrame(word frameWord) {
  PicklingFrame *frame = PicklingFrame::FromWordDirect(frameWord);
  std::fprintf(stderr, "Pickling Value:\n");
  Debug::Dump(frame->GetData());
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
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == PICKLE_PACK_FRAME);
    return static_cast<PicklePackFrame *>(p);
  }
};

// PicklePack Interpreter
class PicklePackInterpreter : public Interpreter {
private:
  static PicklePackInterpreter *self;
  // PicklePackInterpreter Constructor
  PicklePackInterpreter() : Interpreter() {}
public:
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
  PicklingArgs *pargs = PicklingArgs::FromWordDirect(args);
  StringOutputStream *os =
    static_cast<StringOutputStream *>(pargs->GetStream());
  taskStack->PopFrame();
  CONTINUE(Interpreter::OneArg(os->Close()));
}

const char *PicklePackInterpreter::Identify() {
  return "PicklePackInterpreter";
}

void PicklePackInterpreter::DumpFrame(word) {
  std::fprintf(stderr, "PicklePackInterpreter");
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
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == PICKLE_SAVE_FRAME);
    return static_cast<PickleSaveFrame *>(p);
  }
};

// PickleSaveInterpreter
class PickleSaveInterpreter : public Interpreter {
private:
  static PickleSaveInterpreter *self;
  // PickleSaveInterpreter Constructor
  PickleSaveInterpreter() : Interpreter() {}
public:
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
  taskStack->PopFrame();
  PicklingArgs *pargs = PicklingArgs::FromWordDirect(args);
  FileOutputStream *outputStream =
    static_cast<FileOutputStream *>(pargs->GetStream());
  outputStream->Close();
  CONTINUE(Interpreter::EmptyArg());
}

const char *PickleSaveInterpreter::Identify() {
  return "PickleSaveInterpreter";
}

void PickleSaveInterpreter::DumpFrame(word) {
  std::fprintf(stderr, "PickleSaveInterpreter\n");
}

//
// Pickler Functions
//
word Pickler::Sited;

// String Handling: to be done
static char *ExportCString(Chunk *s) {
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
  char *szFileName     = ExportCString(filename);
  FileOutputStream *os = new FileOutputStream(szFileName);
  if (os->GotException()) {
    delete os;
    Scheduler::currentData      = Store::IntToWord(0); // to be done: Io exn
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
