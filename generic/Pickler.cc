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
  enum { FILE_POS, EXCEPTION_POS, SIZE };
public:
  // FileOutputStream Accessors
  void InitFile(FILE *file) {
    InitArg(FILE_POS, Store::UnmanagedPointerToWord(file));
  }
  void InitException(u_int exception) {
    InitArg(EXCEPTION_POS, exception);
  }
  FILE *GetFile() {
    return static_cast<FILE *>
      (Store::DirectWordToUnmanagedPointer(GetArg(FILE_POS)));
  }
  u_int GetException() {
    return Store::DirectWordToInt(GetArg(EXCEPTION_POS));
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
  enum { POS_POS, SIZE_POS, STRING_POS, SIZE };

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
    static_cast<FileOutputStream *>(this)->PutByte(byte); break;
  case STRING_OUTPUT_STREAM:
    static_cast<StringOutputStream *>(this)->PutByte(byte); break;
  }
}

void OutputStream::PutBytes(Chunk *c) {
  switch ((OUT_STREAM_TYPE) this->GetLabel()) {
  case FILE_OUTPUT_STREAM:
    static_cast<FileOutputStream *>(this)->PutBytes(c); break;
  case STRING_OUTPUT_STREAM:
    static_cast<StringOutputStream *>(this)->PutBytes(c); break;
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
  FileOutputStream *stream = static_cast<FileOutputStream *>
    (OutputStream::New(FILE_OUTPUT_STREAM, SIZE));
  FILE *f         = std::fopen(filename, "wb");
  u_int exception = (f == NULL);
  stream->InitFile(f);
  stream->InitException(exception);
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
  StringOutputStream *stream = static_cast<StringOutputStream *>
    (OutputStream::New(STRING_OUTPUT_STREAM, SIZE));
  stream->SetPos(0);
  stream->SetSize(INITIAL_SIZE);
  stream->SetString(String::New(INITIAL_SIZE));
  return stream;
}

class Seen : private Block {
private:
  static const BlockLabel SEEN_LABEL = MIN_DATA_LABEL;
  enum { COUNTER_POS, TABLE_POS, SIZE };
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
  enum { STREAM_POS, SEEN_POS, SIZE };
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
  static void PushFrame(word data);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

// PicklingInterpreter Frame
class PicklingFrame : private StackFrame {
private:
  enum { DATA_POS, SIZE };
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

void PicklingInterpreter::PushFrame(word data) {
  Scheduler::PushFrame(PicklingFrame::New(self, data)->ToWord());
}

#define CONTINUE()					\
  if (StatusWord::GetStatus(Store::GCStatus() | Scheduler::PreemptStatus())) \
    return Interpreter::PREEMPT;			\
  else							\
    return Interpreter::CONTINUE;

Interpreter::Result PicklingInterpreter::Run() {
  PicklingFrame *frame =
    PicklingFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  word x0 = frame->GetData();
  if (Store::WordToTransient(x0) != INVALID_POINTER) {
    Scheduler::currentData = x0;
    Scheduler::PushFrameNoCheck(frame->ToWord());
    return Interpreter::REQUEST;
  }
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
  case UNIQUESTRING_LABEL:
    {
      outputStream->PutByte(Pickle::UNIQUE);
      seen->Add(v);
      UniqueString *s = static_cast<UniqueString *>(v);
      PicklingInterpreter::PushFrame(s->ToString()->ToWord());
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
	PicklingInterpreter::PushFrame(v->GetArg(i));
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
	PicklingInterpreter::PushFrame(v->GetArg(i));
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
	PicklingInterpreter::PushFrame(block->ToWord());
	CONTINUE();
      }
    }
    break;
  case TRANSFORM_LABEL:
    {
      Transform *transform = reinterpret_cast<Transform *>(v);
      outputStream->PutByte(Pickle::TRANSFORM);
      seen->Add(v);
      PicklingInterpreter::PushFrame(transform->GetArgument());
      PicklingInterpreter::PushFrame(transform->GetName()->ToWord());
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
	PicklingInterpreter::PushFrame(v->GetArg(i));
      CONTINUE();
    }
  }
}

const char *PicklingInterpreter::Identify() {
  return "PicklingInterpreter";
}

void PicklingInterpreter::DumpFrame(word) {
  std::fprintf(stderr, "Pickling Task\n");
}

// PicklePackInterpreter Frame
class PicklePackFrame : private StackFrame {
private:
  enum { SIZE };
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
  static void PushFrame();
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// PicklePackInterpreter Functions
//
PicklePackInterpreter *PicklePackInterpreter::self;

void PicklePackInterpreter::PushFrame() {
  Scheduler::PushFrame(PicklePackFrame::New(self)->ToWord());
}

Interpreter::Result PicklePackInterpreter::Run() {
  Scheduler::PopFrame();
  StringOutputStream *os =
    static_cast<StringOutputStream *>(PickleArgs::GetOutputStream());
  Scheduler::nArgs = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = os->Close();
  return Interpreter::CONTINUE;
}

const char *PicklePackInterpreter::Identify() {
  return "PicklePackInterpreter";
}

void PicklePackInterpreter::DumpFrame(word) {
  std::fprintf(stderr, "Pickle Pack");
}

// PickleSaveInterpreter Frame
class PickleSaveFrame : private StackFrame {
private:
  enum { SIZE };
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
  static void PushFrame();
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// PickleSaveInterpreter Interpreter Functions
//
PickleSaveInterpreter *PickleSaveInterpreter::self;

void PickleSaveInterpreter::PushFrame() {
  Scheduler::PushFrame(PickleSaveFrame::New(self)->ToWord());
}

Interpreter::Result PickleSaveInterpreter::Run() {
  Scheduler::PopFrame();
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
  std::fprintf(stderr, "Pickle Save\n");
}

//
// Pickler Functions
//
word Pickler::Sited;

Interpreter::Result Pickler::Pack(word x) {
  StringOutputStream *os = StringOutputStream::New();
  Scheduler::PopFrame();
  PicklePackInterpreter::PushFrame();
  PicklingInterpreter::PushFrame(x);
  PickleArgs::New(os, Seen::New());
  return Interpreter::CONTINUE;
}

Interpreter::Result Pickler::Save(String *filename, word x) {
  char *szFileName     = filename->ExportC();
  FileOutputStream *os = FileOutputStream::New(szFileName);
  if (os->GetException()) {
    delete os;
    Scheduler::currentData = Store::IntToWord(0); // to be done: Io exn
    Scheduler::currentBacktrace = Backtrace::New(Scheduler::GetAndPopFrame());
    return Interpreter::RAISE;
  }
  Scheduler::PopFrame();
  PickleSaveInterpreter::PushFrame();
  PicklingInterpreter::PushFrame(x);
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
