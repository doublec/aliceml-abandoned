//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Contributors:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "generic/Unpickler.hh"
#endif

#include <cstdio>
#include <cstdlib>
#include "adt/Stack.hh"
#include "adt/HashTable.hh"
#include "generic/RootSet.hh"
#include "generic/Tuple.hh"
#include "generic/Closure.hh"
#include "generic/Backtrace.hh"
#include "generic/TaskStack.hh"
#include "generic/Transients.hh"
#include "generic/Interpreter.hh"
#include "generic/Scheduler.hh"
#include "generic/Transform.hh"
#include "generic/Unpickler.hh"

#include "alice/Data.hh" //--** should not be here

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

static word handlerTable;
static const u_int initialHandlerTableSize = 7;

//
// Stream Classes
//
typedef enum {
  FILE_INPUT_STREAM   = MIN_DATA_LABEL,
  STRING_INPUT_STREAM = (FILE_INPUT_STREAM + 1)
} IN_STREAM_TYPE;

class InputStream : private Block {
private:
  static const u_int HD_POS     = 0;
  static const u_int TL_POS     = 1;
  static const u_int RD_POS     = 2;
  static const u_int EOB_POS    = 3;
  static const u_int BUFFER_POS = 4;
  static const u_int SIZE       = 5;
public:
  using Block::ToWord;
  // InputStream Accessors
  u_int GetHd() {
    return Store::DirectWordToInt(Block::GetArg(HD_POS));
  }
  void SetHd(u_int hd) {
    Block::InitArg(HD_POS, hd);
  }
  u_int GetTl() {
    return Store::DirectWordToInt(Block::GetArg(TL_POS));
  }
  void SetTl(u_int tl) {
    Block::InitArg(TL_POS, tl);
  }
  u_int GetRd() {
    return Store::DirectWordToInt(Block::GetArg(RD_POS));
  }
  void SetRd(u_int rd) {
    Block::InitArg(RD_POS, rd);
  }
  u_int GetEOB() {
    return Store::DirectWordToInt(Block::GetArg(EOB_POS));
  }
  void SetEOB(u_int eob) {
    Block::InitArg(EOB_POS, eob);
  }
  Chunk *GetBuffer() {
    return Store::DirectWordToChunk(Block::GetArg(BUFFER_POS));
  }
  void SetBuffer(Chunk *buffer) {
    Block::ReplaceArg(BUFFER_POS, buffer->ToWord());
  }
  word GetArg(u_int index) {
    return Block::GetArg(SIZE + index);
  }
  void InitArg(u_int index, word value) {
    Block::InitArg(SIZE + index, value);
  }
  void ReplaceArg(u_int index, word value) {
    Block::ReplaceArg(SIZE + index, value);
  }
  // InputStream Methods
  u_int IsEOB() {
    if (GetEOB()) {
      SetEOB(0);
      SetRd(GetHd());
      return 1;
    }
    else {
      return 0;
    }
  }
  u_char GetByte() {
    u_int rd = GetRd();
    if (rd == GetTl()) {
      SetEOB(1);
      return (u_char) 0;
    }
    else {
      SetRd(rd + 1);
      u_char *buffer = (u_char*) GetBuffer()->GetBase();
      return buffer[rd];
    }
  }
  u_char *GetBytes(u_int n) {
    u_int rd      = GetRd();
    u_char *bytes = (u_char *) GetBuffer()->GetBase() + rd;
    // Seek bytes to make sure they are available
    if (rd + n >= GetTl()) {
      SetEOB(1);
    }
    else {
      SetRd(rd + n);
    }
    return bytes;
  }
  u_int GetUInt() {
    int shift = 0;
    int freeBits = sizeof(u_int) * 8 - 1;
    u_int value = 0;
    u_char b;
    do {
      b = GetByte(); if (GetEOB()) return 0;
      u_char c = b & 0x7F;
      if (c >= (u_char) (1 << freeBits))
	Error("Unpickler: integer out of range"); //--** raise exception
      value |= c << shift;
      shift += 7;
      freeBits -= 7;
    } while (b & 0x80);
    return value;
  }
  void Commit() {
    SetHd(GetRd());
  }
  void AppendToBuffer(u_char *src, int size) {
    // This has to be revisited: TOO NAIVE
    // Fresh Buffer
    if (GetTl() == 0) {
      Chunk *c       = Store::AllocChunk(size);
      u_char *buffer = (u_char *) c->GetBase();
      Assert(buffer != INVALID_POINTER);
      std::memcpy(buffer, src, size);
      SetTl(size);
      SetBuffer(c);
    }
    // Enlarge Buffer
    else {
      u_int tl       = GetTl();
      int newTl      = size + tl;
      u_char *old    = (u_char *) GetBuffer()->GetBase();
      Chunk *c       = Store::AllocChunk(newTl);
      u_char *buffer = (u_char *) c->GetBase();
      Assert(buffer != INVALID_POINTER);
      std::memcpy(buffer, old, tl);
      std::memcpy(buffer + tl, src, size);
      SetTl(newTl);
      SetBuffer(c);
    }
  }
  // Former virtual methods; need manual dispatch
  void Close();
  Interpreter::Result FillBuffer(TaskStack *taskStack);
  // InputStream Constructor
  static InputStream *New(IN_STREAM_TYPE type, u_int size) {
    InputStream *stream =
      (InputStream *) Store::AllocBlock((BlockLabel) type, SIZE + size);
    stream->SetHd(0);
    stream->SetTl(0);
    stream->SetRd(0);
    stream->SetEOB(0);
    return stream;
  }
  // InputStream Untagging
  static InputStream *FromWordDirect(word stream) {
    Block *p = Store::DirectWordToBlock(stream);
    Assert(p != INVALID_POINTER);
    Assert(p->GetLabel() == (BlockLabel) FILE_INPUT_STREAM ||
	   p->GetLabel() == (BlockLabel) STRING_INPUT_STREAM);
    return static_cast<InputStream *>(p);
  }
};

// FileInputStream
class FileInputStream : public InputStream { //--** finalization to be done
private:
  static const u_int RD_BUF_POS    = 0;
  static const u_int FILE_POS      = 1;
  static const u_int EXCEPTION_POS = 2;
  static const u_int SIZE          = 3;

  static const u_int RD_SIZE       = 8192;
public:
  // FileInputStream Accessors
  Chunk *GetRdBuf() {
    return Store::DirectWordToChunk(GetArg(RD_BUF_POS));
  }
  void SetRdBuf(Chunk *buf) {
    ReplaceArg(RD_BUF_POS, buf->ToWord());
  }
  FILE *GetFile() {
    return (FILE *) Store::DirectWordToUnmanagedPointer(GetArg(FILE_POS));
  } 
  void SetFile(FILE *file) {
    ReplaceArg(FILE_POS, Store::UnmanagedPointerToWord(file));
  }
  u_int GetException() {
    return Store::DirectWordToInt(GetArg(EXCEPTION_POS));
  }
  void SetException(u_int exception) {
    InitArg(EXCEPTION_POS, Store::IntToWord(exception));
  }
  // FileInputStream Constructor
  static FileInputStream *New(char *filename) {
    FileInputStream *is =
      (FileInputStream *) InputStream::New(FILE_INPUT_STREAM, SIZE);
    Chunk *rdBuf    = Store::AllocChunk(RD_SIZE);
    FILE *file      = fopen(filename, "rb");
    u_int exception = (file == NULL);
    is->SetRdBuf(rdBuf);
    is->SetFile(file);
    is->SetException(exception);
    return is;
  }
  // FileInputStream Functions
  bool GotException() {
    return GetException();
  }
  void Close() {
    std::fclose(GetFile());
  }
  Interpreter::Result FillBuffer(TaskStack *taskStack) {
    u_char *rdBuf = (u_char *) GetRdBuf()->GetBase();
    FILE *file    = GetFile();
    u_int nread   = (u_int) std::fread(rdBuf, sizeof(u_char), RD_SIZE, file);
    if (ferror(file)) {
      Error("FileInputStream::FillBuffer"); //--** raise Io exception
    } else if (nread == 0) {
      Scheduler::currentData = Unpickler::Corrupt;
      Scheduler::currentBacktrace = Backtrace::New(taskStack->GetFrame());
      taskStack->PopFrame();
      return Interpreter::RAISE;
    } else {
      AppendToBuffer(rdBuf, nread);
      taskStack->PopFrame();
      return Interpreter::CONTINUE;
    }
  }
};

// StringInputStream
class StringInputStream : public InputStream {
private:
  static const u_int STRING_POS = 0;
  static const u_int SIZE       = 1;
public:
  // StringInputStream Accessors
  word GetString() {
    return GetArg(STRING_POS);
  }
  void SetString(word string) {
    ReplaceArg(STRING_POS, string);
  }
  // StringInputStream Constructor
  static StringInputStream *New(Chunk *chunk) {
    StringInputStream *is =
      (StringInputStream *) InputStream::New(STRING_INPUT_STREAM, SIZE);
    is->SetString(chunk->ToWord());
    return is;
  }
  // StringInputStream Functions
  void Close() {
    return;
  }
  Interpreter::Result FillBuffer(TaskStack *taskStack) {
    taskStack->PopFrame();
    word string = GetString();
    if (string == Store::IntToWord(0)) {
      Scheduler::currentData = Unpickler::Corrupt;
      Scheduler::currentBacktrace = Backtrace::New(taskStack->GetFrame());
      taskStack->PopFrame();
      return Interpreter::RAISE;
    } else {
      Chunk *chunk = Store::DirectWordToChunk(string);
      AppendToBuffer(reinterpret_cast<u_char *>(chunk->GetBase()),
		     chunk->GetSize());
      SetString(Store::IntToWord(0));
      return Interpreter::CONTINUE;
    }
  }
};

// InputStream Methods
void InputStream::Close() {
  switch ((IN_STREAM_TYPE) this->GetLabel()) {
  case FILE_INPUT_STREAM:
    ((FileInputStream *) this)->Close(); break;
  case STRING_INPUT_STREAM:
    ((StringInputStream *) this)->Close(); break;
  default:
    Error("InputStream::Close: illegal node type");
  }
}

Interpreter::Result InputStream::FillBuffer(TaskStack *taskStack) {
  switch ((IN_STREAM_TYPE) this->GetLabel()) {
  case FILE_INPUT_STREAM:
    return ((FileInputStream *) this)->FillBuffer(taskStack);
  case STRING_INPUT_STREAM:
    return ((StringInputStream *) this)->FillBuffer(taskStack);
  default:
    Error("InputStream::FillBuffer: illegal node type");
  }
}

// Pickle Arguments
class UnpickleArgs {
private:
  static const u_int STREAM_POS = 0;
  static const u_int ENV_POS    = 1;
  static const u_int COUNT_POS  = 2;
  static const u_int SIZE       = 3;
public:
  static void New(InputStream *is, word env, int count) {
    Scheduler::nArgs = SIZE;
    Scheduler::currentArgs[STREAM_POS] = is->ToWord();
    Scheduler::currentArgs[ENV_POS] = env;
    Scheduler::currentArgs[COUNT_POS] = Store::IntToWord(count);
  }
  static InputStream *GetInputStream() {
    Assert(Scheduler::nArgs == SIZE);
    return InputStream::FromWordDirect(Scheduler::currentArgs[STREAM_POS]);
  }
  static word GetEnv() {
    Assert(Scheduler::nArgs == SIZE);
    return Scheduler::currentArgs[ENV_POS];
  }
  static u_int GetCount() {
    Assert(Scheduler::nArgs == SIZE);
    return Store::DirectWordToInt(Scheduler::currentArgs[COUNT_POS]);
  }
};

// InputInterpreter
class InputInterpreter : public Interpreter {
private:
  static InputInterpreter *self;
  // InputInterpreter Constructor
  InputInterpreter() : Interpreter() {}
public:
  // InputInterpreter Static Constructor
  static void Init() {
    self = new InputInterpreter();
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
// InputInterpreter Functions
//
InputInterpreter *InputInterpreter::self;

void InputInterpreter::PushFrame(TaskStack *taskStack) {
  taskStack->PushFrame(StackFrame::New(INPUT_FRAME, self, 0)->ToWord());
}

Interpreter::Result InputInterpreter::Run(TaskStack *taskStack) {
  InputStream *is = UnpickleArgs::GetInputStream();
  return is->FillBuffer(taskStack);
}

const char *InputInterpreter::Identify() {
  return "InputInterpreter";
}

void InputInterpreter::DumpFrame(word) {
  std::fprintf(stderr, "Fill Unpickling Buffer\n");
}

// TransformInterpreter Frame
class TransformFrame : private StackFrame {
private:
  static const u_int FUTURE_POS = 0;
  static const u_int TUPLE_POS  = 1;
  static const u_int SIZE       = 2;
public:
  using Block::ToWord;

  // TransformFrame Constructor
  static TransformFrame *New(Interpreter *interpreter,
			     Future *future, Tuple *tuple) {
    StackFrame *frame = StackFrame::New(TRANSFORM_FRAME, interpreter, SIZE);
    frame->InitArg(FUTURE_POS, future->ToWord());
    frame->InitArg(TUPLE_POS, tuple->ToWord());
    return static_cast<TransformFrame *>(frame);
  }
  // TransformFrame Untagging
  static TransformFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == TRANSFORM_FRAME);
    return static_cast<TransformFrame *>(p);
  }

  // TransformFrame Accessors
  Future *GetFuture() {
    return static_cast<Future *>
      (Store::WordToTransient(StackFrame::GetArg(FUTURE_POS)));
  }
  Tuple *GetTuple() {
    return Tuple::FromWordDirect(StackFrame::GetArg(TUPLE_POS));
  }
};

// TransformInterpreter
class TransformInterpreter : public Interpreter {
private:
  static TransformInterpreter *self;
  // TransformInterpreter Constructor
  TransformInterpreter() : Interpreter() {}
public:
  // TransformInterpreter Static Constructor
  static void Init() {
    self = new TransformInterpreter();
  }
  // Frame Handling
  static void TransformInterpreter::PushFrame(TaskStack *taskStack,
					      Future *future, Tuple *tuple);
  // Execution
  virtual Result Run(TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

// ApplyTransform Function
static inline
word ApplyTransform(Chunk *f, word x) {
  Assert(f != INVALID_POINTER);
  HashTable *table = HashTable::FromWordDirect(handlerTable);
  if (table->IsMember(f->ToWord())) {
    Unpickler::handler handler = (Unpickler::handler)
      Store::DirectWordToUnmanagedPointer(table->GetItem(f->ToWord()));
    return handler(x);
  } else {
    Error("ApplyTransform: unknown transform");
  }
}

//
// TransformInterpreter Functions
//
TransformInterpreter *TransformInterpreter::self;

void TransformInterpreter::PushFrame(TaskStack *taskStack,
				     Future *future, Tuple *tuple) {
  TransformFrame *frame = TransformFrame::New(self, future, tuple);
  taskStack->PushFrame(frame->ToWord());
}

Interpreter::Result TransformInterpreter::Run(TaskStack *taskStack) {
  TransformFrame *frame =
    TransformFrame::FromWordDirect(taskStack->GetFrame());
  Future *future = frame->GetFuture();
  Tuple *tuple   = frame->GetTuple();
  Chunk *f       = Chunk::FromWord(tuple->Sel(0));
  word x         = tuple->Sel(1);
  future->Become(REF_LABEL, ApplyTransform(f, x));
  taskStack->PopFrame(); // Discard Frame
  return Interpreter::CONTINUE;
}

const char *TransformInterpreter::Identify() {
  return "TransformInterpreter";
}

void TransformInterpreter::DumpFrame(word) {
  std::fprintf(stderr, "Apply Transform\n");
}

// UnpickleInterpreter Frame
class UnpickleFrame : private StackFrame {
private:
  static const u_int BLOCK_POS     = 0;
  static const u_int INDEX_POS     = 1;
  static const u_int NUM_ELEMS_POS = 2;
  static const u_int SIZE          = 3;
public:
  using Block::ToWord;

  // UnpickleFrame Constructor
  static UnpickleFrame *New(Interpreter *interpreter,
			    word x, u_int i, u_int n) {
    StackFrame *frame = StackFrame::New(UNPICKLE_FRAME, interpreter, SIZE);
    frame->InitArg(BLOCK_POS, x);
    frame->InitArg(INDEX_POS, Store::IntToWord(i));
    frame->InitArg(NUM_ELEMS_POS, Store::IntToWord(n));
    return static_cast<UnpickleFrame *>(frame);
  }
  // UnpickleFrame Untagging
  static UnpickleFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == UNPICKLE_FRAME);
    return static_cast<UnpickleFrame *>(p);
  }

  // UnpickleFrame Accessors
  word GetBlock() {
    return StackFrame::GetArg(BLOCK_POS);
  }
  u_int GetIndex() {
    return Store::DirectWordToInt(StackFrame::GetArg(INDEX_POS));
  }
  u_int GetNumberOfElements() {
    return Store::DirectWordToInt(StackFrame::GetArg(NUM_ELEMS_POS));
  }
};

// UnpickleInterpreter
class UnpickleInterpreter : public Interpreter {
private:
  static UnpickleInterpreter *self;
  // UnpickleInterpreter Constructor
  UnpickleInterpreter() : Interpreter() {}
public:
  // Pickle Tags //--** move to some other file Pickle.hh
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

  // UnpickleInterpreter Static Constructor
  static void Init() {
    self = new UnpickleInterpreter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack, word block, u_int i, u_int n);
  // Execution
  virtual Result Run(TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// UnpickleInterpreter Functions
//
UnpickleInterpreter *UnpickleInterpreter::self;

void UnpickleInterpreter::PushFrame(TaskStack *taskStack,
				    word block, u_int i, u_int n) {
  taskStack->PushFrame(UnpickleFrame::New(self, block, i, n)->ToWord());
}

// Unpickle Helpers
static inline
void PushUnpickleFrame(TaskStack *taskStack, word block, u_int i, u_int n) {
  if (i != n) {
    Assert(i < n);
    UnpickleInterpreter::PushFrame(taskStack, block, i, n);
  }
}

static inline
void Set(word block, u_int i, word y) {
  Store::DirectWordToBlock(block)->ReplaceArg(i, y);
}

static inline
void AddToEnv(word env, u_int count, word value) {
  Stack *stack = Stack::FromWordDirect(env);
  Assert(stack->GetStackSize() == count); count = count;
  stack->SlowPush(value);
}

static inline
word SelFromEnv(word env, u_int index) {
  Stack *stack = Stack::FromWordDirect(env);
  return stack->GetAbsoluteArg(index);
}

// End of Buffer requires rereading;
// therefore we reinstall the old task stack
//--** to be done: more efficient solution
#define CHECK_EOB()				\
  if (is->IsEOB()) {				\
    taskStack->PushFrame(frame->ToWord());	\
    InputInterpreter::PushFrame(taskStack);	\
    return Interpreter::CONTINUE;		\
  } else {}

#define CONTINUE()					\
  if (Scheduler::TestPreempt() || Store::NeedGC())	\
    return Interpreter::PREEMPT;			\
  else							\
    return Interpreter::CONTINUE;

// Core Unpickling Function
Interpreter::Result UnpickleInterpreter::Run(TaskStack *taskStack) {
  UnpickleFrame *frame = UnpickleFrame::FromWordDirect(taskStack->GetFrame());
  word x  = frame->GetBlock();
  u_int i = frame->GetIndex();
  u_int n = frame->GetNumberOfElements();
  // It is safe to pop the frame, since we remember it in variable `frame':
  taskStack->PopFrame();
  if (i == n) { // we are finished!
    CONTINUE();
  } else {
    InputStream *is = UnpickleArgs::GetInputStream();
    word env        = UnpickleArgs::GetEnv();
    u_int count     = UnpickleArgs::GetCount();
    u_char tag      = is->GetByte();
    CHECK_EOB();
    switch ((Tag::PickleTags) tag) {
    case Tag::POSINT:
      {
	u_int y = is->GetUInt(); CHECK_EOB();
	Set(x, i, Store::IntToWord(y));
	is->Commit();
	PushUnpickleFrame(taskStack, x, i + 1, n);
	CONTINUE();
      }
      break;
    case Tag::NEGINT:
      {
	u_int y = is->GetUInt(); CHECK_EOB();
	Set(x, i, Store::IntToWord(-(y + 1)));
	is->Commit();
	PushUnpickleFrame(taskStack, x, i + 1, n);
	CONTINUE();
      }
      break;
    case Tag::CHUNK:
      {
	u_int size    = is->GetUInt(); CHECK_EOB();
	u_char *bytes = is->GetBytes(size); CHECK_EOB();
	Chunk *y      = Store::AllocChunk(size);
	std::memcpy(y->GetBase(), bytes, size);
	Set(x, i, y->ToWord());
	AddToEnv(env, count, y->ToWord());
	is->Commit();
	PushUnpickleFrame(taskStack, x, i + 1, n);
	UnpickleArgs::New(is, env, count + 1);
	CONTINUE();
      }
      break;
    case Tag::BLOCK:
      {
	u_int label  = is->GetUInt(); CHECK_EOB();
	u_int size   = is->GetUInt(); CHECK_EOB();
	Block *block = Store::AllocBlock(static_cast<BlockLabel>(label), size);
	word y       = block->ToWord();
	Set(x, i, y);
	AddToEnv(env, count, y);
	is->Commit();
	PushUnpickleFrame(taskStack, x, i + 1, n);
	UnpickleInterpreter::PushFrame(taskStack, y, 0, size);
	UnpickleArgs::New(is, env, count + 1);
	CONTINUE();
      }
      break;
    case Tag::TUPLE:
      {
	u_int size = is->GetUInt(); CHECK_EOB();
	word y     = Tuple::New(size)->ToWord();
	Set(x, i, y);
	AddToEnv(env, count, y);
	is->Commit();
	PushUnpickleFrame(taskStack, x, i + 1, n);
	UnpickleInterpreter::PushFrame(taskStack, y, 0, size);
	UnpickleArgs::New(is, env, count + 1);
	CONTINUE();
      }
      break;
    case Tag::CLOSURE:
      {
	u_int size = is->GetUInt(); CHECK_EOB();
	word cc    = Store::IntToWord(0); // will be replaced by concrete code
	word y     = Closure::New(cc, size - 1)->ToWord();
	Set(x, i, y);
	AddToEnv(env, count, y);
	is->Commit();
	PushUnpickleFrame(taskStack, x, i + 1, n);
	UnpickleInterpreter::PushFrame(taskStack, y, 0, size);
	UnpickleArgs::New(is, env, count + 1);
	CONTINUE();
      }
      break;
    case Tag::TRANSFORM:
      {
	Future *future = Future::New();
	word y         = future->ToWord();
	Set(x, i, y);
	AddToEnv(env, count, y);
	is->Commit();
	Tuple *tuple = Tuple::New(2);
	PushUnpickleFrame(taskStack, x, i + 1, n);
	TransformInterpreter::PushFrame(taskStack, future, tuple);
	UnpickleInterpreter::PushFrame(taskStack, tuple->ToWord(), 0, 2);
	UnpickleArgs::New(is, env, count + 1);
	CONTINUE();
      }
      break;
    case Tag::REF:
      {
	u_int index = is->GetUInt(); CHECK_EOB();
	Set(x, i, SelFromEnv(env, index));
	is->Commit();
	PushUnpickleFrame(taskStack, x, i + 1, n);
	CONTINUE();
      }
    default:
      Scheduler::currentData = Unpickler::Corrupt;
      Scheduler::currentBacktrace = Backtrace::New(taskStack->GetFrame());
      return Interpreter::RAISE;
    }
  }
}

const char *UnpickleInterpreter::Identify() {
  return "UnpickleInterpreter";
}

void UnpickleInterpreter::DumpFrame(word frameWord) {
  UnpickleFrame *frame = UnpickleFrame::FromWordDirect(frameWord);
  std::fprintf(stderr, "Unpickling Task %d of %d\n",
	       frame->GetIndex(), frame->GetNumberOfElements());
}

// PickleUnpackInterpeter Frame
class PickleUnpackFrame : private StackFrame {
private:
  static const u_int TUPLE_POS = 0;
  static const u_int SIZE      = 1;
public:
  using Block::ToWord;

  // PickleUnpackFrame Constructor
  static PickleUnpackFrame *New(Interpreter *interpreter, Tuple *x) {
    StackFrame *frame =
      StackFrame::New(PICKLE_UNPACK_FRAME, interpreter, SIZE);
    frame->InitArg(TUPLE_POS, x->ToWord());
    return static_cast<PickleUnpackFrame *>(frame);
  }
  // PickleUnpackFrame Untagging
  static PickleUnpackFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == PICKLE_UNPACK_FRAME);
    return static_cast<PickleUnpackFrame *>(p);
  }

  // PickleUnpackFrame Accessors
  Tuple *GetTuple() {
    return Tuple::FromWordDirect(StackFrame::GetArg(TUPLE_POS));
  }
};

// PickleUnpackInterpeter
class PickleUnpackInterpeter : public Interpreter {
private:
  static PickleUnpackInterpeter *self;
  // PickleUnpackInterpeter Constructor
  PickleUnpackInterpeter() : Interpreter() {}
public:
  // PickleUnpackInterpeter Static Constructor
  static void Init() {
    self = new PickleUnpackInterpeter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack, Tuple *x);
  // Execution
  virtual Result Run(TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// PickleUnpackInterpeter Functions
//
PickleUnpackInterpeter *PickleUnpackInterpeter::self;

void PickleUnpackInterpeter::PushFrame(TaskStack *taskStack, Tuple *x) {
  taskStack->PushFrame(PickleUnpackFrame::New(self, x)->ToWord());
}

Interpreter::Result PickleUnpackInterpeter::Run(TaskStack *taskStack) {
  PickleUnpackFrame *frame =
    PickleUnpackFrame::FromWordDirect(taskStack->GetFrame());
  Tuple *x = frame->GetTuple();
  taskStack->PopFrame();
  Scheduler::nArgs = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = x->Sel(0);
  return Interpreter::CONTINUE;
}

const char *PickleUnpackInterpeter::Identify() {
  return "PickleUnpackInterpeter";
}

void PickleUnpackInterpeter::DumpFrame(word) {
  std::fprintf(stderr, "Pickle Unpack\n");
}

// PickleLoadInterpreter Frame
class PickleLoadFrame : private StackFrame {
private:
  static const u_int TUPLE_POS = 0;
  static const u_int SIZE      = 1;
public:
  using Block::ToWord;

  // PickleLoadFrame Constructor
  static PickleLoadFrame *New(Interpreter *interpreter, Tuple *x) {
    StackFrame *frame = StackFrame::New(PICKLE_LOAD_FRAME, interpreter, SIZE);
    frame->InitArg(TUPLE_POS, x->ToWord());
    return static_cast<PickleLoadFrame *>(frame);
  }
  // PickleLoadFrame Untagging
  static PickleLoadFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == PICKLE_LOAD_FRAME);
    return static_cast<PickleLoadFrame *>(p);
  }

  // PickleLoadFrame Accessors
  Tuple *GetTuple() {
    return Tuple::FromWordDirect(StackFrame::GetArg(TUPLE_POS));
  }
};

// PickleLoadInterpreter
class PickleLoadInterpreter : public Interpreter {
private:
  static PickleLoadInterpreter *self;
  // PickleLoadInterpreter Constructor
  PickleLoadInterpreter() : Interpreter() {}
public:
  // PickleLoadInterpreter Static Constructor
  static void Init() {
    self = new PickleLoadInterpreter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack, Tuple *x);
  // Execution
  virtual Result Run(TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// PickleLoadInterpreter Functions
//
PickleLoadInterpreter *PickleLoadInterpreter::self;

void PickleLoadInterpreter::PushFrame(TaskStack *taskStack, Tuple *x) {
  taskStack->PushFrame(PickleLoadFrame::New(self, x)->ToWord());
}

Interpreter::Result PickleLoadInterpreter::Run(TaskStack *taskStack) {
  PickleLoadFrame *frame =
    PickleLoadFrame::FromWordDirect(taskStack->GetFrame());
  InputStream *is = UnpickleArgs::GetInputStream();
  Tuple *x        = frame->GetTuple();
  is->Close();
  taskStack->PopFrame();
  Scheduler::nArgs = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = x->Sel(0);
  return Interpreter::CONTINUE;
}

const char *PickleLoadInterpreter::Identify() {
  return "PickleLoadInterpreter";
}

void PickleLoadInterpreter::DumpFrame(word) {
  std::fprintf(stderr, "Pickle Load\n");
}

//
// Unpickler Functions
//
static const u_int INITIAL_TABLE_SIZE = 16; // to be checked

// String Handling: to be done
static char *ExportCString(Chunk *s) {
  u_int sLen = s->GetSize();
  Chunk *e   = Store::AllocChunk(sLen + 1);
  char *eb   = e->GetBase();
  std::memcpy(eb, s->GetBase(), sLen);
  eb[sLen] = '\0';
  return eb;
}

Interpreter::Result Unpickler::Unpack(Chunk *s, TaskStack *taskStack) {
  Tuple *x = Tuple::New(1);
  InputStream *is = (InputStream *) StringInputStream::New(s);
  Stack *env = Stack::New(INITIAL_TABLE_SIZE);
  taskStack->PopFrame();
  PickleUnpackInterpeter::PushFrame(taskStack, x);
  UnpickleInterpreter::PushFrame(taskStack, x->ToWord(), 0, 1);
  UnpickleArgs::New(is, env->ToWord(), 0);
  return Interpreter::CONTINUE;
}

Interpreter::Result Unpickler::Load(Chunk *filename, TaskStack *taskStack) {
  char *szFileName    = ExportCString(filename);
  FileInputStream *is = FileInputStream::New(szFileName);
  if (is->GotException()) {
    delete is;
    Scheduler::currentData = Store::IntToWord(0); // to be done
    fprintf(stderr, "file '%s' not found\n", szFileName);
    Scheduler::currentBacktrace = Backtrace::New(taskStack->GetFrame());
    taskStack->PopFrame();
    return Interpreter::RAISE;
  }
  taskStack->PopFrame();
  Tuple *x = Tuple::New(1);
  Stack *env = Stack::New(INITIAL_TABLE_SIZE);
  PickleLoadInterpreter::PushFrame(taskStack, x);
  UnpickleInterpreter::PushFrame(taskStack, x->ToWord(), 0, 1);
  UnpickleArgs::New(is, env->ToWord(), 0);
  return Interpreter::CONTINUE;
}

word Unpickler::Corrupt;

void Unpickler::Init() {
  // Setup internal Interpreters
  InputInterpreter::Init();
  TransformInterpreter::Init();
  UnpickleInterpreter::Init();
  PickleUnpackInterpeter::Init();
  PickleLoadInterpreter::Init();
  handlerTable =
    HashTable::New(HashTable::BLOCK_KEY, initialHandlerTableSize)->ToWord();
  RootSet::Add(handlerTable);
}

void Unpickler::InitExceptions() {
  Corrupt = UniqueConstructor::New(String::New("Component.Corrupt"))->ToWord();
  RootSet::Add(Corrupt);
}

void Unpickler::RegisterHandler(Chunk *name, handler handler) {
  word x = Store::UnmanagedPointerToWord((void *) handler);
  HashTable::FromWordDirect(handlerTable)->InsertItem(name->ToWord(), x);
}
