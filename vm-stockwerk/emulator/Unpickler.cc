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
#pragma implementation "emulator/Unpickler.hh"
#endif

#include <cstdio>
#include <cstdlib>
#include "adt/Stack.hh"
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
#include "emulator/Unpickler.hh"

//--** these should be factored out:
#include "emulator/Alice.hh"
#include "emulator/PrimitiveTable.hh"
#include "emulator/AbstractCodeInterpreter.hh"

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
// Stream Classes
//
class InputStream {
private:
  u_int hd, tl, rd, eob;
  u_char *buffer;
public:
  // InputStream Constructor
  InputStream() : hd(0), tl(0), rd(0), eob(0) {
    buffer = INVALID_POINTER;
  }
  // InputStream Functions
  u_int IsEOB() {
    if (eob) {
      eob = 0;
      rd  = hd;
      return 1;
    }
    else {
      return 0;
    }
  }
  u_char GetByte() {
    if (rd == tl) {
      eob = 1;
      return (u_char) 0;
    }
    else {
       return buffer[rd++];
    }
  }
  u_char *GetBytes(u_int n) {
    u_char *bytes = buffer + rd;
    // Seek bytes to make sure they are available
    if (rd + n >= tl) {
      eob = 1;
    }
    else {
      rd += n;
    }
    return bytes;
  }
  u_int GetUInt() {
    int shift = 0;
    int freeBits = sizeof(u_int) * 8 - 1;
    u_int value = 0;
    u_char b;
    do {
      b = GetByte(); if (eob) return 0;
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
    hd = rd;
  }
  void AppendToBuffer(u_char *src, int size) {
    // This has to be revisited: TOO NAIVE
    // Fresh Buffer
    if (tl == 0) {
      buffer = reinterpret_cast<u_char *>(malloc(sizeof(u_char) * size));
      Assert(buffer != INVALID_POINTER);
      std::memcpy(buffer, src, size);
      tl = size;
    }
    // Enlarge Buffer
    else {
      int newTl   = size + tl;
      u_char *old = buffer;
      buffer = reinterpret_cast<u_char *>(malloc(sizeof(u_char) * newTl));
      Assert(buffer != INVALID_POINTER);
      std::memcpy(buffer, old, tl);
      std::memcpy(buffer + tl, src, size);
      tl = newTl;
    }
  }
  // Buffer Handling
  virtual void Close() {
    free(buffer);
  }
  virtual Interpreter::Result FillBuffer(TaskStack *taskStack) = 0;

  // Store Interface
  word ToWord() {
    return Store::UnmanagedPointerToWord(this);
  }
  static InputStream *FromWordDirect(word x) {
    void *p = Store::DirectWordToUnmanagedPointer(x);
    return static_cast<InputStream *>(p);
  }
};

// FileInputStream
class FileInputStream : public InputStream { //--** finalization to be done
private:
  static const u_int rdSize = 8192;
  u_char *rdBuf;
  std::FILE *file;
  bool exception;
public:
  // FileInputStream Constructor
  FileInputStream(char *filename) : InputStream() {
    rdBuf = reinterpret_cast<u_char *>(std::malloc(sizeof(u_char) * rdSize));
    file = std::fopen(filename, "r");
    exception = (file == NULL);
  }
  // FileInputStream Functions
  bool GotException() {
    return exception;
  }
  virtual void Close() {
    InputStream::Close();
    std::fclose(file);
    std::free(rdBuf);
  }
  virtual Interpreter::Result FillBuffer(TaskStack *taskStack) {
    u_int nread = (u_int) std::fread(rdBuf, sizeof(u_char), rdSize, file);
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
  word string;
public:
  // StringInputStream Constructor
  StringInputStream(Chunk *chunk) : InputStream() {
    string = chunk->ToWord();
    RootSet::Add(string);
  }
  // StringInputStream Functions
  virtual void Close() {
    InputStream::Close();
    RootSet::Remove(string);
  }
  virtual Interpreter::Result FillBuffer(TaskStack *taskStack) {
    taskStack->PopFrame();
    if (string == Store::IntToWord(0)) {
      Scheduler::currentData = Unpickler::Corrupt;
      Scheduler::currentBacktrace = Backtrace::New(taskStack->GetFrame());
      taskStack->PopFrame();
      return Interpreter::RAISE;
    } else {
      Chunk *chunk = Store::DirectWordToChunk(string);
      AppendToBuffer(reinterpret_cast<u_char *>(chunk->GetBase()),
		     chunk->GetSize());
      string = Store::IntToWord(0);
      return Interpreter::CONTINUE;
    }
  }
};

// Pickle Arguments
class UnpickleArgs {
private:
  static const u_int STREAM_POS = 0;
  static const u_int ENV_POS    = 1;
  static const u_int COUNT_POS  = 2;
public:
  static void New(InputStream *is, word env, int count) {
    Scheduler::currentArgs[STREAM_POS] = is->ToWord();
    Scheduler::currentArgs[ENV_POS] = env;
    Scheduler::currentArgs[COUNT_POS] = Store::IntToWord(count);
  }
  static InputStream *GetInputStream() {
    return InputStream::FromWordDirect(Scheduler::currentArgs[STREAM_POS]);
  }
  static word GetEnv() {
    return Scheduler::currentArgs[ENV_POS];
  }
  static u_int GetCount() {
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
  char *fs = f->GetBase();
  u_int len = f->GetSize();
  if ((len == sizeof("Alice.primitive.value") - 1) &&
      !std::memcmp(fs, "Alice.primitive.value", len)) {
    Block *xp = Store::WordToBlock(x);
    return PrimitiveTable::LookupValue(Chunk::FromWord(xp->GetArg(0)));
  } else if ((len == sizeof("Alice.primitive.function") - 1) &&
	     !std::memcmp(fs, "Alice.primitive.function", len)) {
    Block *xp = Store::WordToBlock(x);
    return PrimitiveTable::LookupFunction(Chunk::FromWord(xp->GetArg(0)));
  } else if ((len == sizeof("Alice.function") - 1) &&
	     !std::memcmp(fs, "Alice.function", len)) {
    ConcreteCode *concreteCode =
      ConcreteCode::New(AbstractCodeInterpreter::self, 2);
    Chunk *name =
      Store::DirectWordToChunk(Unpickler::aliceFunctionTransformName);
    Transform *transform = Transform::New(name, x);
    concreteCode->Init(0, x);
    concreteCode->Init(1, transform->ToWord());
    return concreteCode->ToWord();
  }
  Error("ApplyTransform: unknown transform");
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

// Core Unpickling Function
Interpreter::Result UnpickleInterpreter::Run(TaskStack *taskStack) {
  UnpickleFrame *frame = UnpickleFrame::FromWordDirect(taskStack->GetFrame());
  word x  = frame->GetBlock();
  u_int i = frame->GetIndex();
  u_int n = frame->GetNumberOfElements();
  // It is safe to pop the frame, since we remember it in variable `frame':
  taskStack->PopFrame();
  if (i == n) { // we are finished!
    return Interpreter::CONTINUE;
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
	return Interpreter::CONTINUE;
      }
      break;
    case Tag::NEGINT:
      {
	u_int y = is->GetUInt(); CHECK_EOB();
	Set(x, i, Store::IntToWord(-(y + 1)));
	is->Commit();
	PushUnpickleFrame(taskStack, x, i + 1, n);
	return Interpreter::CONTINUE;
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
	return Interpreter::CONTINUE;
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
	return Interpreter::CONTINUE;
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
	return Interpreter::CONTINUE;
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
	return Interpreter::CONTINUE;
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
	return Interpreter::CONTINUE;
      }
      break;
    case Tag::REF:
      {
	u_int index = is->GetUInt(); CHECK_EOB();
	Set(x, i, SelFromEnv(env, index));
	is->Commit();
	PushUnpickleFrame(taskStack, x, i + 1, n);
	return Interpreter::CONTINUE;
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
  InputStream *is = new StringInputStream(s);
  Stack *env = Stack::New(INITIAL_TABLE_SIZE);
  taskStack->PopFrame();
  PickleUnpackInterpeter::PushFrame(taskStack, x);
  UnpickleInterpreter::PushFrame(taskStack, x->ToWord(), 0, 1);
  UnpickleArgs::New(is, env->ToWord(), 0);
  return Interpreter::CONTINUE;
}

Interpreter::Result Unpickler::Load(Chunk *filename, TaskStack *taskStack) {
  char *szFileName    = ExportCString(filename);
  FileInputStream *is = new FileInputStream(szFileName);
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
word Unpickler::aliceFunctionTransformName;

void Unpickler::Init() {
  // Setup internal Interpreters
  InputInterpreter::Init();
  TransformInterpreter::Init();
  UnpickleInterpreter::Init();
  PickleUnpackInterpeter::Init();
  PickleLoadInterpreter::Init();
  Corrupt = UniqueConstructor::New(String::New("Component.Corrupt"))->ToWord();
  RootSet::Add(Corrupt);
  aliceFunctionTransformName = String::New("Alice.function")->ToWord();
  RootSet::Add(aliceFunctionTransformName);
}
