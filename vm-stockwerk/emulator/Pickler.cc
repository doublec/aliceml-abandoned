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

// to be done
class Seen: private Queue {
private:
  static const u_int initialQueueSize = 8; //--** to be checked
public:
  static const u_int NOT_FOUND = static_cast<u_int>(-1);

  using Queue::ToWord;

  static Seen *New() {
    return static_cast<Seen *>(Queue::New(initialQueueSize));
  }
  static Seen *FromWordDirect(word w) {
    return static_cast<Seen *>(Queue::FromWordDirect(w));
  }

  void Add(Block *v) {
    Enqueue(v->ToWord());
  }
  u_int Find(Block *v) {
    for (u_int i = GetNumberOfElements(); i--; )
      if (Store::DirectWordToBlock(GetNthElement(i)) == v)
	return i;
    return NOT_FOUND;
  }
};

// PickleArgs
class PickleArgs {
private:
  static const u_int STREAM_POS = 0;
  static const u_int SEEN_POS   = 1;
public:
  static void New(OutputStream *stream, Seen *seen) {
    Scheduler::currentArgs[STREAM_POS] = stream->ToWord();
    Scheduler::currentArgs[SEEN_POS] = seen->ToWord();
  }
  static OutputStream *GetOutputStream() {
    return OutputStream::FromWordDirect(Scheduler::currentArgs[STREAM_POS]);
  }
  static Seen *GetSeen() {
    return Seen::FromWordDirect(Scheduler::currentArgs[SEEN_POS]);
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
  virtual Result Run(TaskStack *taskStack);
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

Interpreter::Result PicklingInterpreter::Run(TaskStack *taskStack) {
  PicklingFrame *frame = PicklingFrame::FromWordDirect(taskStack->GetFrame());
  word x0              = frame->GetData();
  if (Store::WordToTransient(x0) != INVALID_POINTER) {
    Scheduler::currentData = x0;
    return Interpreter::REQUEST;
  }
  taskStack->PopFrame();
  OutputStream *outputStream = PickleArgs::GetOutputStream();
  // Check for integer
  int i;
  if ((i = Store::DirectWordToInt(x0)) != INVALID_INT) {
    if (i >= 0) {
      outputStream->PutByte(Tag::POSINT);
      outputStream->PutUInt(i);
    } else {
      outputStream->PutByte(Tag::NEGINT);
      outputStream->PutUInt(-(i + 1));
    }
    return Interpreter::CONTINUE;
  }
  // Search for already known value
  Seen *seen = PickleArgs::GetSeen();
  Block *v   = Store::WordToBlock(x0);
  u_int ref  = seen->Find(v);
  if (ref != Seen::NOT_FOUND) {
    outputStream->PutByte(Tag::REF);
    outputStream->PutUInt(ref);
    return Interpreter::CONTINUE;
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
      seen->Add(v);
      return Interpreter::CONTINUE;
    }
    break;
  case TUPLE_LABEL:
    {
      u_int size = v->GetSize();
      outputStream->PutByte(Tag::TUPLE);
      outputStream->PutUInt(size);
      seen->Add(v);
      for (u_int i = size; i--; )
	PicklingInterpreter::PushFrame(taskStack, v->GetArg(i));
      return Interpreter::CONTINUE;
    }
    break;
  case CLOSURE_LABEL:
    {
      u_int size = v->GetSize();
      outputStream->PutByte(Tag::CLOSURE);
      outputStream->PutUInt(size);
      seen->Add(v);
      for (u_int i = size; i--; )
	PicklingInterpreter::PushFrame(taskStack, v->GetArg(i));
      return Interpreter::CONTINUE;
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
	Assert(block->GetLabel() == TRANSFORM_LABEL);
	PicklingInterpreter::PushFrame(taskStack, block->ToWord());
	return Interpreter::CONTINUE;
      }
    }
    break;
  case TRANSFORM_LABEL:
    {
      Transform *transform = reinterpret_cast<Transform *>(v);
      outputStream->PutByte(Tag::TRANSFORM);
      seen->Add(v);
      PicklingInterpreter::PushFrame(taskStack, transform->GetArgument());
      PicklingInterpreter::PushFrame(taskStack, transform->GetName()->ToWord());
      return Interpreter::CONTINUE;
    }
    break;
  default:
    {
      u_int size = v->GetSize(); // to be done
      outputStream->PutByte(Tag::BLOCK);
      outputStream->PutUInt(l);
      outputStream->PutUInt(size);
      seen->Add(v);
      for (u_int i = size; i--; )
	PicklingInterpreter::PushFrame(taskStack, v->GetArg(i));
      return Interpreter::CONTINUE;
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
  virtual Result Run(TaskStack *taskStack);
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

Interpreter::Result PicklePackInterpreter::Run(TaskStack *taskStack) {
  StringOutputStream *os =
    static_cast<StringOutputStream *>(PickleArgs::GetOutputStream());
  taskStack->PopFrame();
  Scheduler::nArgs = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = os->Close();
  return Interpreter::CONTINUE;
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
  virtual Result Run(TaskStack *taskStack);
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

Interpreter::Result PickleSaveInterpreter::Run(TaskStack *taskStack) {
  taskStack->PopFrame();
  FileOutputStream *outputStream =
    static_cast<FileOutputStream *>(PickleArgs::GetOutputStream());
  outputStream->Close();
  Scheduler::nArgs = 0;
  return Interpreter::CONTINUE;
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
  PickleArgs::New(os, Seen::New());
  return Interpreter::CONTINUE;
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
  PickleArgs::New(os, Seen::New());
  return Interpreter::CONTINUE;
}

void Pickler::Init() {
  Sited = UniqueConstructor::New(String::New("Component.Sited"))->ToWord();
  RootSet::Add(Sited);
  PicklingInterpreter::Init();
  PicklePackInterpreter::Init();
  PickleSaveInterpreter::Init();
}
