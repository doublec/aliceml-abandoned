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
#include "generic/Transients.hh"
#include "generic/Interpreter.hh"
#include "generic/Scheduler.hh"
#include "generic/Transform.hh"
#include "generic/Unpickler.hh"
#include "generic/Pickle.hh"

#include "alice/Data.hh" //--** should not be here

static word handlerTable;
static const u_int initialHandlerTableSize = 7;

//
// InputStream Class
//
static const u_int READ_BUFFER_SIZE = 8192; // to be checked
static const u_int READ_BUFFER_OVERSHOOT = 20; // to be checked

class InputStream: private Block { //--** finalization to be done
private:
  enum IN_STREAM_TYPE {
    FILE_INPUT_STREAM = MIN_DATA_LABEL, STRING_INPUT_STREAM
  };

  enum { HD_POS, TL_POS, RD_POS, EOB_POS, BUFFER_POS, BASE_SIZE };
  enum { RD_BUF_POS = BASE_SIZE, FILE_POS, FILE_INPUT_STREAM_SIZE };
  enum { STRING_INPUT_STREAM_SIZE = BASE_SIZE };

  u_int GetHd() {
    return Store::DirectWordToInt(GetArg(HD_POS));
  }
  void SetHd(u_int hd) {
    ReplaceArg(HD_POS, hd);
  }
  u_int GetTl() {
    return Store::DirectWordToInt(GetArg(TL_POS));
  }
  void SetTl(u_int tl) {
    ReplaceArg(TL_POS, tl);
  }
  u_int GetRd() {
    return Store::DirectWordToInt(GetArg(RD_POS));
  }
  void SetRd(u_int rd) {
    ReplaceArg(RD_POS, rd);
  }
  void SetEOB(bool eob) {
    ReplaceArg(EOB_POS, eob);
  }
  String *GetBuffer() {
    return String::FromWordDirect(GetArg(BUFFER_POS));
  }
  void SetBuffer(String *buffer) {
    ReplaceArg(BUFFER_POS, buffer->ToWord());
  }

  static InputStream *New(IN_STREAM_TYPE type, u_int size) {
    InputStream *is =
      (InputStream *) Store::AllocBlock((BlockLabel) type, size);
    is->InitArg(HD_POS, 0);
    is->InitArg(RD_POS, 0);
    is->InitArg(EOB_POS, false);
    return is;
  }
public:
  using Block::ToWord;

  // InputStream Constructors
  static InputStream *NewFromFile(const char *filename) {
    InputStream *is = New(FILE_INPUT_STREAM, FILE_INPUT_STREAM_SIZE);
    FILE *file = std::fopen(filename, "rb");
    is->InitArg(FILE_POS, Store::UnmanagedPointerToWord(file));
    if (file != NULL) {
      Chunk *buffer =
	Store::AllocChunk(READ_BUFFER_SIZE + READ_BUFFER_OVERSHOOT);
      is->InitArg(BUFFER_POS, buffer->ToWord());
    }
    is->InitArg(TL_POS, 0);
    return is;
  }
  static InputStream *NewFromString(String *string) {
    InputStream *is = New(STRING_INPUT_STREAM, STRING_INPUT_STREAM_SIZE);
    is->InitArg(BUFFER_POS, string->ToWord());
    is->InitArg(TL_POS, string->GetSize());
    return is;
  }

  // InputStream Untagging
  static InputStream *FromWordDirect(word stream) {
    Block *p = Store::DirectWordToBlock(stream);
    Assert(p != INVALID_POINTER);
    Assert(p->GetLabel() == (BlockLabel) FILE_INPUT_STREAM ||
	   p->GetLabel() == (BlockLabel) STRING_INPUT_STREAM);
    return static_cast<InputStream *>(p);
  }

  // InputStream Methods
  bool IsEOB() {
    return Store::DirectWordToInt(Block::GetArg(EOB_POS));
  }
  u_char GetByte() {
    u_int rd = GetRd();
    if (rd >= GetTl()) {
      SetEOB(true);
      return (u_char) 0;
    } else {
      SetRd(rd + 1);
      u_char *buffer = GetBuffer()->GetValue();
      return buffer[rd];
    }
  }
  u_char *GetBytes(u_int n) {
    u_int rd      = GetRd();
    u_char *bytes = GetBuffer()->GetValue() + rd;
    // Seek bytes to make sure they are available
    if (rd + n >= GetTl())
      SetEOB(true);
    else
      SetRd(rd + n);
    return bytes;
  }
  u_int GetUInt() {
    int shift = 0;
    int freeBits = sizeof(u_int) * 8 - 1;
    u_int value = 0;
    u_char b;
    do {
      b = GetByte(); if (IsEOB()) return 0;
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
  void Close() {
    switch ((IN_STREAM_TYPE) GetLabel()) {
    case FILE_INPUT_STREAM:
      {
	FILE *file = static_cast<FILE *>
	  (Store::DirectWordToUnmanagedPointer(GetArg(FILE_POS)));
	std::fclose(file);
      }
      break;
    case STRING_INPUT_STREAM:
      break;
    default:
      Error("InputStream::Close: illegal node type");
    }
  }
  Interpreter::Result FillBuffer() {
    switch ((IN_STREAM_TYPE) GetLabel()) {
    case FILE_INPUT_STREAM:
      {
	u_int hd = GetHd(), tl = GetTl();
	String *buffer = GetBuffer();
	u_char *bytes = buffer->GetValue();
	u_int size = buffer->GetSize();
	if (hd > 0) { // move data to beginning of buffer, then fill rest
	  tl -= hd;
	  std::memmove(bytes, bytes + hd, tl);
	  SetHd(0);
	  bytes += tl;
	  size -= tl;
	} else if (tl == size) { // enlarge buffer by READ_BUFFER_SIZE
	  u_int newSize = size + READ_BUFFER_SIZE;
	  String *newBuffer = String::New(newSize);
	  SetBuffer(newBuffer);
	  u_char *newBytes = newBuffer->GetValue();
	  std::memcpy(newBytes, bytes, size);
	  bytes = newBytes + size;
	  size = READ_BUFFER_SIZE;
	} else { // free space at end of buffer: fill it
	  bytes += tl;
	  size -= tl;
	}
	SetRd(GetHd()); // undo non-committed reads
	SetEOB(false);
	// here bytes and size indicate the buffer to fill;
	// tl still needs to be written back
	FILE *file = static_cast<FILE *>
	  (Store::DirectWordToUnmanagedPointer(GetArg(FILE_POS)));
	u_int nread = std::fread(bytes, sizeof(u_char), size, file);
	if (ferror(file)) {
	  Error("InputStream::FillBuffer"); //--** raise Io exception
	} else if (nread == 0) { // at end of file: raise Corrupt exception
	  Scheduler::currentData = Unpickler::Corrupt;
	  Scheduler::currentBacktrace =
	    Backtrace::New(Scheduler::GetAndPopFrame());
	  return Interpreter::RAISE;
	} else {
	  tl += nread;
	  SetTl(tl);
	  Scheduler::PopFrame();
	  return Interpreter::CONTINUE;
	}
      }
    case STRING_INPUT_STREAM:
      // there is no more data: raise Corrupt exception
      Scheduler::currentData = Unpickler::Corrupt;
      Scheduler::currentBacktrace =
	Backtrace::New(Scheduler::GetAndPopFrame());
      return Interpreter::RAISE;
    default:
      Error("InputStream::FillBuffer: illegal node type");
    }
  }
  bool HasException() {
    FILE *file = static_cast<FILE *>
      (Store::DirectWordToUnmanagedPointer(GetArg(FILE_POS)));
    return file == NULL;
  }
};

// Pickle Arguments
class UnpickleArgs {
private:
  enum { STREAM_POS, ENV_POS, SIZE };
public:
  static void New(InputStream *is, word env) {
    Scheduler::nArgs = SIZE;
    Scheduler::currentArgs[STREAM_POS] = is->ToWord();
    Scheduler::currentArgs[ENV_POS] = env;
  }
  static InputStream *GetInputStream() {
    Assert(Scheduler::nArgs == SIZE);
    return InputStream::FromWordDirect(Scheduler::currentArgs[STREAM_POS]);
  }
  static word GetEnv() {
    Assert(Scheduler::nArgs == SIZE);
    return Scheduler::currentArgs[ENV_POS];
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
  static void PushFrame();
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// InputInterpreter Functions
//
InputInterpreter *InputInterpreter::self;

void InputInterpreter::PushFrame() {
  Scheduler::PushFrame(StackFrame::New(INPUT_FRAME, self, 0)->ToWord());
}

Interpreter::Result InputInterpreter::Run() {
  return UnpickleArgs::GetInputStream()->FillBuffer();
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
  enum { FUTURE_POS, TUPLE_POS, SIZE };
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
  static void TransformInterpreter::PushFrame(Future *future, Tuple *tuple);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

// ApplyTransform Function
static inline
word ApplyTransform(String *name, word argument) {
  Assert(name != INVALID_POINTER);
  HashTable *table = HashTable::FromWordDirect(handlerTable);
  if (table->IsMember(name->ToWord())) {
    Unpickler::handler handler = (Unpickler::handler)
      Store::DirectWordToUnmanagedPointer(table->GetItem(name->ToWord()));
    return handler(argument);
  } else {
    Error("ApplyTransform: unknown transform");
  }
}

//
// TransformInterpreter Functions
//
TransformInterpreter *TransformInterpreter::self;

void TransformInterpreter::PushFrame(Future *future, Tuple *tuple) {
  Scheduler::PushFrame(TransformFrame::New(self, future, tuple)->ToWord());
}

Interpreter::Result TransformInterpreter::Run() {
  TransformFrame *frame =
    TransformFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  Future *future = frame->GetFuture();
  Tuple *tuple   = frame->GetTuple();
  String *name   = String::FromWordDirect(tuple->Sel(0));
  word argument  = tuple->Sel(1);
  future->Become(REF_LABEL, ApplyTransform(name, argument));
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
  enum { BLOCK_POS, INDEX_POS, NUM_ELEMS_POS, SIZE };
public:
  using Block::ToWord;

  // UnpickleFrame Constructor
  static UnpickleFrame *New(Interpreter *interpreter,
			    word x, u_int i, u_int n) {
    StackFrame *frame = StackFrame::New(UNPICKLING_FRAME, interpreter, SIZE);
    frame->InitArg(BLOCK_POS, x);
    frame->InitArg(INDEX_POS, Store::IntToWord(i));
    frame->InitArg(NUM_ELEMS_POS, Store::IntToWord(n));
    return static_cast<UnpickleFrame *>(frame);
  }
  // UnpickleFrame Untagging
  static UnpickleFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == UNPICKLING_FRAME);
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
  // UnpickleInterpreter Static Constructor
  static void Init() {
    self = new UnpickleInterpreter();
  }
  // Frame Handling
  static void PushFrame(word block, u_int i, u_int n);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// UnpickleInterpreter Functions
//
UnpickleInterpreter *UnpickleInterpreter::self;

void UnpickleInterpreter::PushFrame(word block, u_int i, u_int n) {
  Scheduler::PushFrame(UnpickleFrame::New(self, block, i, n)->ToWord());
}

// Unpickle Helpers
static inline
void PushUnpickleFrame(word block, u_int i, u_int n) {
  if (i != n) {
    Assert(i < n);
    UnpickleInterpreter::PushFrame(block, i, n);
  }
}

static inline
void Set(word block, u_int i, word y) {
  Store::DirectWordToBlock(block)->ReplaceArg(i, y);
}

static inline
void AddToEnv(word env, word value) {
  Stack::FromWordDirect(env)->SlowPush(value);
}

static inline
word SelFromEnv(word env, u_int index) {
  return Stack::FromWordDirect(env)->GetAbsoluteArg(index);
}

// End of Buffer requires rereading;
// therefore we reinstall the old task stack
//--** to be done: more efficient solution
#define CHECK_EOB()				\
  if (is->IsEOB()) {				\
    Scheduler::PushFrame(frame->ToWord());	\
    InputInterpreter::PushFrame();		\
    return Interpreter::CONTINUE;		\
  } else {}

#define CONTINUE()							     \
  if (StatusWord::GetStatus(Store::GCStatus() | Scheduler::PreemptStatus())) \
    return Interpreter::PREEMPT;					     \
  else									     \
    return Interpreter::CONTINUE;

// Core Unpickling Function
Interpreter::Result UnpickleInterpreter::Run() {
  UnpickleFrame *frame =
    UnpickleFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  word x = frame->GetBlock();
  u_int i = frame->GetIndex();
  u_int n = frame->GetNumberOfElements();
  if (i == n) { // we are finished!
    CONTINUE();
  } else {
    InputStream *is = UnpickleArgs::GetInputStream();
    word env        = UnpickleArgs::GetEnv();
    u_char tag      = is->GetByte(); CHECK_EOB();
    switch (static_cast<Pickle::Tag>(tag)) {
    case Pickle::POSINT:
      {
	u_int y = is->GetUInt(); CHECK_EOB();
	Set(x, i, Store::IntToWord(y));
	is->Commit();
	PushUnpickleFrame(x, i + 1, n);
	CONTINUE();
      }
      break;
    case Pickle::NEGINT:
      {
	u_int y = is->GetUInt(); CHECK_EOB();
	Set(x, i, Store::IntToWord(-(y + 1)));
	is->Commit();
	PushUnpickleFrame(x, i + 1, n);
	CONTINUE();
      }
      break;
    case Pickle::CHUNK:
      {
	u_int size    = is->GetUInt(); CHECK_EOB();
	u_char *bytes = is->GetBytes(size); CHECK_EOB();
	Chunk *y      = Store::AllocChunk(size);
	std::memcpy(y->GetBase(), bytes, size);
	Set(x, i, y->ToWord());
	AddToEnv(env, y->ToWord());
	is->Commit();
	PushUnpickleFrame(x, i + 1, n);
	CONTINUE();
      }
      break;
    case Pickle::UNIQUE:
      {
	String *s;
	Chunk *y;
	u_char tag = is->GetByte(); CHECK_EOB();
	switch (static_cast<Pickle::Tag>(tag)) {
	case Pickle::CHUNK:
	  {
	    u_int size = is->GetUInt(); CHECK_EOB();
	    u_char *bytes = is->GetBytes(size); CHECK_EOB();
	    y = Store::AllocChunk(size);
	    std::memcpy(y->GetBase(), bytes, size);
	    s = static_cast<String *>(y);
	  }
	  break;
	case Pickle::REF:
	  {
	    u_int index = is->GetUInt(); CHECK_EOB();
	    s = String::FromWordDirect(SelFromEnv(env, index));
	    y = INVALID_POINTER;
	  }
	  break;
	default:
	  Scheduler::currentData = Unpickler::Corrupt;
	  Scheduler::currentBacktrace =
	    Backtrace::New(Scheduler::GetAndPopFrame());
	  return Interpreter::RAISE;
	}
	is->Commit();
	word wUnique = UniqueString::New(s)->ToWord();
	Set(x, i, wUnique);
	AddToEnv(env, wUnique);
	if (y != INVALID_POINTER) AddToEnv(env, y->ToWord());
	PushUnpickleFrame(x, i + 1, n);
	CONTINUE();
      }
      break;
    case Pickle::BLOCK:
      {
	u_int label  = is->GetUInt(); CHECK_EOB();
	u_int size   = is->GetUInt(); CHECK_EOB();
	Block *block = Store::AllocBlock(static_cast<BlockLabel>(label), size);
	word y       = block->ToWord();
	Set(x, i, y);
	AddToEnv(env, y);
	is->Commit();
	PushUnpickleFrame(x, i + 1, n);
	UnpickleInterpreter::PushFrame(y, 0, size);
	CONTINUE();
      }
      break;
    case Pickle::TUPLE:
      {
	u_int size = is->GetUInt(); CHECK_EOB();
	word y     = Tuple::New(size)->ToWord();
	Set(x, i, y);
	AddToEnv(env, y);
	is->Commit();
	PushUnpickleFrame(x, i + 1, n);
	UnpickleInterpreter::PushFrame(y, 0, size);
	CONTINUE();
      }
      break;
    case Pickle::CLOSURE:
      {
	u_int size = is->GetUInt(); CHECK_EOB();
	word cc    = Store::IntToWord(0); // will be replaced by concrete code
	word y     = Closure::New(cc, size - 1)->ToWord();
	Set(x, i, y);
	AddToEnv(env, y);
	is->Commit();
	PushUnpickleFrame(x, i + 1, n);
	UnpickleInterpreter::PushFrame(y, 0, size);
	CONTINUE();
      }
      break;
    case Pickle::TRANSFORM:
      {
	Future *future = Future::New();
	word y         = future->ToWord();
	Set(x, i, y);
	AddToEnv(env, y);
	is->Commit();
	Tuple *tuple = Tuple::New(2);
	PushUnpickleFrame(x, i + 1, n);
	TransformInterpreter::PushFrame(future, tuple);
	UnpickleInterpreter::PushFrame(tuple->ToWord(), 0, 2);
	CONTINUE();
      }
      break;
    case Pickle::REF:
      {
	u_int index = is->GetUInt(); CHECK_EOB();
	Set(x, i, SelFromEnv(env, index));
	is->Commit();
	PushUnpickleFrame(x, i + 1, n);
	CONTINUE();
      }
    default:
      Scheduler::currentData = Unpickler::Corrupt;
      Scheduler::currentBacktrace =
	Backtrace::New(Scheduler::GetAndPopFrame());
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
  enum { TUPLE_POS, SIZE };
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
  static void PushFrame(Tuple *x);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// PickleUnpackInterpeter Functions
//
PickleUnpackInterpeter *PickleUnpackInterpeter::self;

void PickleUnpackInterpeter::PushFrame(Tuple *x) {
  Scheduler::PushFrame(PickleUnpackFrame::New(self, x)->ToWord());
}

Interpreter::Result PickleUnpackInterpeter::Run() {
  PickleUnpackFrame *frame =
    PickleUnpackFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  Tuple *x = frame->GetTuple();
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
  enum { TUPLE_POS, SIZE };
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
  static void PushFrame(Tuple *x);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// PickleLoadInterpreter Functions
//
PickleLoadInterpreter *PickleLoadInterpreter::self;

void PickleLoadInterpreter::PushFrame(Tuple *x) {
  Scheduler::PushFrame(PickleLoadFrame::New(self, x)->ToWord());
}

Interpreter::Result PickleLoadInterpreter::Run() {
  PickleLoadFrame *frame =
    PickleLoadFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  InputStream *is = UnpickleArgs::GetInputStream();
  Tuple *x = frame->GetTuple();
  is->Close();
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

Interpreter::Result Unpickler::Unpack(String *s) {
  Tuple *x = Tuple::New(1);
  InputStream *is = InputStream::NewFromString(s);
  Stack *env = Stack::New(INITIAL_TABLE_SIZE);
  Scheduler::PopFrame();
  PickleUnpackInterpeter::PushFrame(x);
  UnpickleInterpreter::PushFrame(x->ToWord(), 0, 1);
  UnpickleArgs::New(is, env->ToWord());
  return Interpreter::CONTINUE;
}

Interpreter::Result Unpickler::Load(String *filename) {
  char *szFileName = filename->ExportC();
  InputStream *is = InputStream::NewFromFile(szFileName);
  if (is->HasException()) {
    delete is;
    Scheduler::currentData = Store::IntToWord(0); // to be done
    fprintf(stderr, "file '%s' not found\n", szFileName);
    Scheduler::currentBacktrace = Backtrace::New(Scheduler::GetAndPopFrame());
    return Interpreter::RAISE;
  }
  Scheduler::PopFrame();
  Tuple *x = Tuple::New(1);
  Stack *env = Stack::New(INITIAL_TABLE_SIZE);
  PickleLoadInterpreter::PushFrame(x);
  UnpickleInterpreter::PushFrame(x->ToWord(), 0, 1);
  UnpickleArgs::New(is, env->ToWord());
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

void Unpickler::RegisterHandler(String *name, handler handler) {
  word x = Store::UnmanagedPointerToWord((void *) handler);
  HashTable::FromWordDirect(handlerTable)->InsertItem(name->ToWord(), x);
}
