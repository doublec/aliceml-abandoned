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
#pragma implementation "generic/Pickler.hh"
#endif

#include <cstdio>
#include <cstdlib>
#include "generic/RootSet.hh"
#include "generic/Tuple.hh"
#include "generic/ConcreteCode.hh"
#include "generic/Closure.hh"
#include "generic/Backtrace.hh"
#include "generic/TaskStack.hh"
#include "generic/Transients.hh"
#include "generic/Interpreter.hh"
#include "generic/Scheduler.hh"
#include "generic/Transform.hh"
#include "generic/Pickler.hh"
#include "generic/Pickle.hh"

#include "alice/Data.hh" //--** should not be here

//
// Stream Classes
//
typedef enum {
  FILE_OUTPUT_STREAM   = MIN_DATA_LABEL,
  STRING_OUTPUT_STREAM = (FILE_OUTPUT_STREAM + 1)
} OUT_STREAM_TYPE;

class OutputStream : public Block {
public:
  // OutputStream Accessors
  void PutByte(u_char byte);
  void PutUInt(u_int i);
  void PutBytes(Chunk *c);
  // OutputStream Constructor
  static OutputStream *New(OUT_STREAM_TYPE type, u_int size) {
    Block *p =Store::AllocBlock((BlockLabel) type, size);
    return static_cast<OutputStream *>(p);
  }
  // OutputStream Untagging
  static OutputStream *FromWordDirect(word stream) {
    Block *p = Store::DirectWordToBlock(stream);
    Assert(p != INVALID_POINTER);
    Assert(p->GetLabel() == (BlockLabel) FILE_OUTPUT_STREAM ||
	   p->GetLabel() == (BlockLabel) STRING_OUTPUT_STREAM);
    return static_cast<OutputStream *>(p);
  }
};

class FileOutputStream : public OutputStream {
private:
  static const u_int FILE_POS      = 0;
  static const u_int EXCEPTION_POS = 1;
  static const u_int SIZE          = 2;
public:
  // FileOutputStream Accessors
  FILE *GetFile() {
    return (FILE *) Store::DirectWordToUnmanagedPointer(GetArg(FILE_POS));
  }
  void PutFile(FILE *file) {
    InitArg(FILE_POS, Store::UnmanagedPointerToWord(file));
  }
  u_int GetException() {
    return Store::DirectWordToInt(GetArg(EXCEPTION_POS));
  }
  void PutException(u_int exception) {
    InitArg(EXCEPTION_POS, exception);
  }
  // FileOutputStream Methods
  void PutByte(u_char byte);
  void PutBytes(Chunk *c);
  word Close();
  // FileOutputStream Constructor
  static FileOutputStream *New(char *filename);
};

class StringOutputStream : public OutputStream {
private:
  static const u_int POS_POS    = 0;
  static const u_int SIZE_POS   = 1;
  static const u_int STRING_POS = 2;
  static const u_int SIZE       = 3;

  static const u_int INITIAL_SIZE = 256;
  void Enlarge();
public:
  // StringOutputStream Accessors
  u_int GetPos() {
    return Store::DirectWordToInt(GetArg(POS_POS));
  }
  void SetPos(u_int pos) {
    InitArg(POS_POS, pos);
  }
  u_int GetSize() {
    return Store::DirectWordToInt(GetArg(SIZE_POS));
  }
  void SetSize(u_int size) {
    InitArg(SIZE_POS, size);
  }
  String *GetString() {
    return String::FromWordDirect(GetArg(STRING_POS));
  }
  void SetString(String *string) {
    ReplaceArg(STRING_POS, string->ToWord());
  }
  // StringOutputStream Methods
  void PutByte(u_char byte);
  void PutBytes(Chunk *c);
  word Close();
  // StringOutputStream Constructor
  static StringOutputStream *New();
};

// OutputStream Methods
void OutputStream::PutByte(u_char byte) {
  switch ((OUT_STREAM_TYPE) this->GetLabel()) {
  case FILE_OUTPUT_STREAM:
    ((FileOutputStream *) this)->PutByte(byte); break;
  case STRING_OUTPUT_STREAM:
    ((StringOutputStream *) this)->PutByte(byte); break;
  }
}

void OutputStream::PutBytes(Chunk *c) {
  switch ((OUT_STREAM_TYPE) this->GetLabel()) {
  case FILE_OUTPUT_STREAM:
    ((FileOutputStream *) this)->PutBytes(c); break;
  case STRING_OUTPUT_STREAM:
    ((StringOutputStream *) this)->PutBytes(c); break;
  }
}

void OutputStream::PutUInt(u_int i) {
  while (i >= 0x80) {
    OutputStream::PutByte(i & 0x7F | 0x80);
    i >>= 7;
  }
  OutputStream::PutByte(i);
}

// FileOutputStream Methods
FileOutputStream *FileOutputStream::New(char *filename) {
  FileOutputStream *stream =
    (FileOutputStream *) OutputStream::New(FILE_OUTPUT_STREAM, SIZE);
  FILE *f         = std::fopen(filename, "wb");
  u_int exception = (f == NULL);
  stream->PutFile(f);
  stream->PutException(exception);
  return stream;
}

void FileOutputStream::PutByte(u_char byte) {
  std::fputc(byte, GetFile());
}

void FileOutputStream::PutBytes(Chunk *c) {
  std::fwrite(c->GetBase(), 1, c->GetSize(), GetFile());
}

word FileOutputStream::Close() {
  std::fclose(GetFile());
  return Store::IntToWord(0);
}

// StringOutputStream Methods
void StringOutputStream::Enlarge() {
  u_int newSize     = (GetSize() * 3) >> 1;
  String *newString = String::New(newSize);
  String *oldString = GetString();
  std::memcpy(newString->GetValue(), oldString->GetValue(), GetPos());
  SetSize(newSize);
  SetString(newString);
}

void StringOutputStream::PutByte(u_char byte) {
  u_char *c  = GetString()->GetValue();
  u_int pos  = GetPos();
  u_int size = GetSize();
  c[pos++] = byte;
  SetPos(pos);
  if (pos == size) {
    Enlarge();
  }
}

void StringOutputStream::PutBytes(Chunk *c) {
  u_int cSize = c->GetSize();
  u_int pos   = GetPos();
  while (pos + cSize >= GetSize()) {
    Enlarge();
  }
  std::memcpy(GetString()->GetValue() + pos, c->GetBase(), cSize);
  SetPos(pos + cSize);
}

word StringOutputStream::Close() {
  Block *str = static_cast<Block *>(GetString());
  HeaderOp::EncodeSize(str, GetPos());
  return str->ToWord();
}

StringOutputStream *StringOutputStream::New() {
  StringOutputStream *stream =
    (StringOutputStream *) OutputStream::New(STRING_OUTPUT_STREAM, SIZE);
  stream->SetPos(0);
  stream->SetSize(INITIAL_SIZE);
  stream->SetString(String::New(INITIAL_SIZE));
  return stream;
}

class Seen : private Block {
private:
  static const BlockLabel SEEN_LABEL = MIN_DATA_LABEL;
  static const u_int COUNTER_POS = 0;
  static const u_int TABLE_POS   = 1;
  static const u_int SIZE        = 2;
  static const u_int initialSize = 8; //--** to be checked
public:
  static const u_int NOT_FOUND = static_cast<u_int>(-1);

  using Block::ToWord;

  static Seen *New() {
    Block *p = Store::AllocBlock(SEEN_LABEL, SIZE);
    p->InitArg(COUNTER_POS, 0);
    p->InitArg(TABLE_POS, BlockHashTable::New(initialSize)->ToWord());
    return static_cast<Seen *>(p);
  }
  static Seen *FromWordDirect(word w) {
    Block *b = Store::DirectWordToBlock(w);
    Assert(b->GetLabel() == SEEN_LABEL);
    return static_cast<Seen *>(b);
  }

  void Add(Block *v) {
    word counter          = GetArg(COUNTER_POS);
    BlockHashTable *table = BlockHashTable::FromWordDirect(GetArg(TABLE_POS));
    table->InsertItem(v->ToWord(), counter);
    ReplaceArg(COUNTER_POS, Store::DirectWordToInt(counter) + 1);
  }
  u_int Find(Block *v) {
    word vw               = v->ToWord();
    BlockHashTable *table = BlockHashTable::FromWordDirect(GetArg(TABLE_POS));
    if (table->IsMember(vw))
      return Store::DirectWordToInt(table->GetItem(vw));
    else
      return NOT_FOUND;
  }
};

// PickleArgs
class PickleArgs {
private:
  static const u_int STREAM_POS = 0;
  static const u_int SEEN_POS   = 1;
  static const u_int SIZE       = 2;
public:
  static void New(OutputStream *stream, Seen *seen) {
    Scheduler::nArgs = SIZE;
    Scheduler::currentArgs[STREAM_POS] = stream->ToWord();
    Scheduler::currentArgs[SEEN_POS] = seen->ToWord();
  }
  static OutputStream *GetOutputStream() {
    Assert(Scheduler::nArgs == SIZE);
    return OutputStream::FromWordDirect(Scheduler::currentArgs[STREAM_POS]);
  }
  static Seen *GetSeen() {
    Assert(Scheduler::nArgs == SIZE);
    return Seen::FromWordDirect(Scheduler::currentArgs[SEEN_POS]);
  }
};

// PicklingInterpreter
class PicklingInterpreter : public Interpreter {
private:
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

#define CONTINUE()					\
  if (Scheduler::TestPreempt() || Store::NeedGC())	\
    return Interpreter::PREEMPT;			\
  else							\
    return Interpreter::CONTINUE;

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
  if ((i = Store::WordToInt(x0)) != INVALID_INT) {
    if (i >= 0) {
      outputStream->PutByte(Pickle::POSINT);
      outputStream->PutUInt(i);
    } else {
      outputStream->PutByte(Pickle::NEGINT);
      outputStream->PutUInt(-(i + 1));
    }
    CONTINUE();
  }
  // Search for already known value
  Seen *seen = PickleArgs::GetSeen();
  Block *v   = Store::WordToBlock(x0);
  u_int ref  = seen->Find(v);
  if (ref != Seen::NOT_FOUND) {
    outputStream->PutByte(Pickle::REF);
    outputStream->PutUInt(ref);
    CONTINUE();
  }
  // Handle new Block Value (non-abstract use)
  BlockLabel l = v->GetLabel();
  switch (l) {
  case CHUNK_LABEL:
    {
      Chunk *c = static_cast<Chunk *>(v);
      outputStream->PutByte(Pickle::CHUNK);
      outputStream->PutUInt(c->GetSize());
      outputStream->PutBytes(c);
      seen->Add(v);
      CONTINUE();
    }
    break;
  case TUPLE_LABEL:
    {
      u_int size = v->GetSize();
      outputStream->PutByte(Pickle::TUPLE);
      outputStream->PutUInt(size);
      seen->Add(v);
      for (u_int i = size; i--; )
	PicklingInterpreter::PushFrame(taskStack, v->GetArg(i));
      CONTINUE();
    }
    break;
  case CLOSURE_LABEL:
    {
      u_int size = v->GetSize();
      outputStream->PutByte(Pickle::CLOSURE);
      outputStream->PutUInt(size);
      seen->Add(v);
      for (u_int i = size; i--; )
	PicklingInterpreter::PushFrame(taskStack, v->GetArg(i));
      CONTINUE();
    }
    break;
  case CONCRETE_LABEL:
    {
      ConcreteRepresentationHandler *cr = (ConcreteRepresentationHandler *)
	Store::DirectWordToUnmanagedPointer(v->GetArg(0));
      Block *block = cr->GetAbstractRepresentation(v);
      if (block == INVALID_POINTER) {
	Scheduler::currentData      = Pickler::Sited;
	Scheduler::currentBacktrace = Backtrace::New(frame->ToWord());
	return Interpreter::RAISE;
      } else {
	Assert(block->GetLabel() == TRANSFORM_LABEL);
	PicklingInterpreter::PushFrame(taskStack, block->ToWord());
	CONTINUE();
      }
    }
    break;
  case TRANSFORM_LABEL:
    {
      Transform *transform = reinterpret_cast<Transform *>(v);
      outputStream->PutByte(Pickle::TRANSFORM);
      seen->Add(v);
      PicklingInterpreter::PushFrame(taskStack, transform->GetArgument());
      PicklingInterpreter::PushFrame(taskStack, transform->GetName()->ToWord());
      CONTINUE();
    }
    break;
  default:
    {
      u_int size = v->GetSize(); // to be done
      outputStream->PutByte(Pickle::BLOCK);
      outputStream->PutUInt(l);
      outputStream->PutUInt(size);
      seen->Add(v);
      for (u_int i = size; i--; )
	PicklingInterpreter::PushFrame(taskStack, v->GetArg(i));
      CONTINUE();
    }
  }
}

const char *PicklingInterpreter::Identify() {
  return "PicklingInterpreter";
}

void PicklingInterpreter::DumpFrame(word frameWord) {
  PicklingFrame *frame = PicklingFrame::FromWordDirect(frameWord);
  std::fprintf(stderr, "Pickling Task\n");
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
  // Frame Handling
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

Interpreter::Result Pickler::Pack(word x, TaskStack *taskStack) {
  StringOutputStream *os = StringOutputStream::New();
  taskStack->PopFrame();
  PicklePackInterpreter::PushFrame(taskStack);
  PicklingInterpreter::PushFrame(taskStack, x);
  PickleArgs::New(os, Seen::New());
  return Interpreter::CONTINUE;
}

Interpreter::Result
Pickler::Save(String *filename, word x, TaskStack *taskStack) {
  char *szFileName     = filename->ExportC();
  FileOutputStream *os = FileOutputStream::New(szFileName);
  if (os->GetException()) {
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
  PicklingInterpreter::Init();
  PicklePackInterpreter::Init();
  PickleSaveInterpreter::Init();
}

void Pickler::InitExceptions() {
  Sited = UniqueConstructor::New(String::New("Component.Sited"))->ToWord();
  RootSet::Add(Sited);
}
