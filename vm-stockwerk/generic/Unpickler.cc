//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Contributor:
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
#include <zlib.h>
#include "adt/Stack.hh"
#include "adt/ChunkMap.hh"
#include "generic/RootSet.hh"
#include "generic/FinalizationSet.hh"
#include "generic/Tuple.hh"
#include "generic/Closure.hh"
#include "generic/Backtrace.hh"
#include "generic/Transients.hh"
#include "generic/Worker.hh"
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

class InputStreamFinalizationSet: public FinalizationSet {
public:
  virtual void Finalize(word value);
};

class InputStream: private Block {
private:
  enum IN_STREAM_TYPE {
    FILE_INPUT_STREAM = MIN_DATA_LABEL, STRING_INPUT_STREAM
  };

  static InputStreamFinalizationSet *finalizationSet;

  enum { HD_POS, TL_POS, RD_POS, EOB_POS, BUFFER_POS, BASE_SIZE };
  enum { RD_BUF_POS = BASE_SIZE, FILE_POS, FINALIZATION_KEY_POS,
	 FILE_INPUT_STREAM_SIZE };
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

  static void Init() {
    finalizationSet = new InputStreamFinalizationSet();
  }

  // InputStream Constructors
  static InputStream *NewFromFile(const char *filename) {
    InputStream *is = New(FILE_INPUT_STREAM, FILE_INPUT_STREAM_SIZE);
    gzFile file = gzopen(filename, "rb");
    is->InitArg(FILE_POS, Store::UnmanagedPointerToWord(file));
    if (file != NULL) {
      Chunk *buffer =
	Store::AllocChunk(READ_BUFFER_SIZE + READ_BUFFER_OVERSHOOT);
      is->InitArg(BUFFER_POS, buffer->ToWord());
    }
    is->InitArg(TL_POS, 0);
    is->InitArg(FINALIZATION_KEY_POS, finalizationSet->Register(is->ToWord()));
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
  gzFile GetFile() {
    Assert(GetLabel() == (BlockLabel) FILE_INPUT_STREAM);
    return static_cast<gzFile>
      (Store::DirectWordToUnmanagedPointer(GetArg(FILE_POS)));
  }
  bool IsEOB() {
    return Store::DirectWordToInt(Block::GetArg(EOB_POS));
  }
  bool IsEOF() {
    switch ((IN_STREAM_TYPE) GetLabel()) {
    case FILE_INPUT_STREAM:
      if (GetRd() >= GetTl() &&
	  gzgetc(GetFile()) < 0) {
	gzseek(GetFile(), -1L, SEEK_CUR);
	return true;
      } else {
	return false;
      }
      break;
    case STRING_INPUT_STREAM:
      return (GetRd() >= GetTl());
      break;
    default:
      Error("InputStream::IsEOF: illegal node type");
    }
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
    int freeBits = INT_PRECISION;
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
	gzclose(GetFile());
	u_int key = Store::DirectWordToInt(GetArg(FINALIZATION_KEY_POS));
	finalizationSet->Unregister(key);
      }
      break;
    case STRING_INPUT_STREAM:
      break;
    default:
      Error("InputStream::Close: illegal node type");
    }
  }
  void Rewind() {
    switch ((IN_STREAM_TYPE) GetLabel()) {
    case FILE_INPUT_STREAM:
      {
	gzrewind(GetFile());
	SetTl(0);
      }
      break;
    case STRING_INPUT_STREAM:
      {
	SetTl(GetBuffer()->GetSize());
      }
      break;
    default:
      Error("InputStream::Close: illegal node type");
    }
    SetHd(0);
    SetRd(0);
    SetEOB(false);
  }
  Worker::Result FillBuffer() {
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
	gzFile file = GetFile();
	int nread = gzread(file, bytes, size);
	if (nread < 0) {
	  Error("InputStream::FillBuffer"); //--** raise Io exception
	} else if (nread == 0) { // at end of file: raise Corrupt exception
	  Scheduler::currentData = Unpickler::Corrupt;
	  Scheduler::currentBacktrace =
	    Backtrace::New(Scheduler::GetAndPopFrame());
	  return Worker::RAISE;
	} else {
	  tl += nread;
	  SetTl(tl);
	  Scheduler::PopFrame();
	  return Worker::CONTINUE;
	}
      }
    case STRING_INPUT_STREAM:
      // there is no more data: raise Corrupt exception
      Scheduler::currentData = Unpickler::Corrupt;
      Scheduler::currentBacktrace =
	Backtrace::New(Scheduler::GetAndPopFrame());
      return Worker::RAISE;
    default:
      Error("InputStream::FillBuffer: illegal node type");
    }
  }
  bool HasException() {
    return GetFile() == NULL;
  }
};

InputStreamFinalizationSet *InputStream::finalizationSet;

void InputStreamFinalizationSet::Finalize(word value) {
  gzclose(InputStream::FromWordDirect(value)->GetFile());
}

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

// InputWorker
class InputWorker: public Worker {
private:
  static InputWorker *self;
  // InputWorker Constructor
  InputWorker(): Worker() {}
public:
  // InputWorker Static Constructor
  static void Init() {
    self = new InputWorker();
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
// InputWorker Functions
//
InputWorker *InputWorker::self;

void InputWorker::PushFrame() {
  Scheduler::PushFrame(StackFrame::New(INPUT_FRAME, self, 0)->ToWord());
}

Worker::Result InputWorker::Run() {
  return UnpickleArgs::GetInputStream()->FillBuffer();
}

const char *InputWorker::Identify() {
  return "InputWorker";
}

void InputWorker::DumpFrame(word) {
  std::fprintf(stderr, "Fill Unpickling Buffer\n");
}

// TransformWorker Frame
class TransformFrame: private StackFrame {
private:
  enum { FUTURE_POS, TUPLE_POS, SIZE };
public:
  using Block::ToWord;

  // TransformFrame Constructor
  static TransformFrame *New(Worker *worker, Future *future, Tuple *tuple) {
    StackFrame *frame = StackFrame::New(TRANSFORM_FRAME, worker, SIZE);
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

// TransformWorker
class TransformWorker: public Worker {
private:
  static TransformWorker *self;
  // TransformWorker Constructor
  TransformWorker(): Worker() {}
public:
  // TransformWorker Static Constructor
  static void Init() {
    self = new TransformWorker();
  }
  // Frame Handling
  static void TransformWorker::PushFrame(Future *future, Tuple *tuple);
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
  ChunkMap *map = ChunkMap::FromWordDirect(handlerTable);
  if (map->IsMember(name->ToWord())) {
    Unpickler::handler handler = (Unpickler::handler)
      Store::DirectWordToUnmanagedPointer(map->Get(name->ToWord()));
    return handler(argument);
  } else {
    Error("ApplyTransform: unknown transform");
  }
}

//
// TransformWorker Functions
//
TransformWorker *TransformWorker::self;

void TransformWorker::PushFrame(Future *future, Tuple *tuple) {
  Scheduler::PushFrame(TransformFrame::New(self, future, tuple)->ToWord());
}

Worker::Result TransformWorker::Run() {
  TransformFrame *frame =
    TransformFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  Future *future = frame->GetFuture();
  Tuple *tuple   = frame->GetTuple();
  String *name   = String::FromWordDirect(tuple->Sel(0));
  word argument  = tuple->Sel(1);
  future->Become(REF_LABEL, ApplyTransform(name, argument));
  return Worker::CONTINUE;
}

const char *TransformWorker::Identify() {
  return "TransformWorker";
}

void TransformWorker::DumpFrame(word) {
  std::fprintf(stderr, "Apply Transform\n");
}

// UnpickleWorker Frame
class UnpickleFrame: private StackFrame {
private:
  enum { BLOCK_POS, INDEX_POS, NUM_ELEMS_POS, SIZE };
public:
  using Block::ToWord;

  // UnpickleFrame Constructor
  static UnpickleFrame *New(Worker *worker, word x, u_int i, u_int n) {
    StackFrame *frame = StackFrame::New(UNPICKLING_FRAME, worker, SIZE);
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

// UnpickleWorker
class UnpickleWorker: public Worker {
private:
  static UnpickleWorker *self;
  // UnpickleWorker Constructor
  UnpickleWorker(): Worker() {}
public:
  // UnpickleWorker Static Constructor
  static void Init() {
    self = new UnpickleWorker();
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
// UnpickleWorker Functions
//
UnpickleWorker *UnpickleWorker::self;

void UnpickleWorker::PushFrame(word block, u_int i, u_int n) {
  Scheduler::PushFrame(UnpickleFrame::New(self, block, i, n)->ToWord());
}

// Unpickle Helpers
static inline
void PushUnpickleFrame(word block, u_int i, u_int n) {
  if (i != n) {
    Assert(i < n);
    UnpickleWorker::PushFrame(block, i, n);
  }
}

static inline void Set(word block, u_int i, word y) {
  Store::DirectWordToBlock(block)->ReplaceArg(i, y);
}

static inline void AddToEnv(word env, word value) {
  Stack::FromWordDirect(env)->SlowPush(value);
}

static inline word SelFromEnv(word env, u_int index) {
  Stack *stack = Stack::FromWordDirect(env);
  if (index < stack->GetStackSize())
    return stack->GetAbsoluteArg(index);
  else
    return 0;
}

// End of Buffer requires rereading;
// therefore we reinstall the old task stack
//--** to be done: more efficient solution
#define CHECK_EOB() {				\
  if (is->IsEOB()) {				\
    Scheduler::PushFrame(frame->ToWord());	\
    InputWorker::PushFrame();			\
    return Worker::CONTINUE;			\
  }						\
}

#define CONTINUE() {				\
  if (StatusWord::GetStatus() != 0)		\
    return Worker::PREEMPT;			\
  else						\
    return Worker::CONTINUE;			\
}

#define CORRUPT() {					\
  Scheduler::currentData = Unpickler::Corrupt;		\
  Scheduler::currentBacktrace =				\
    Backtrace::New(Scheduler::GetAndPopFrame());	\
  return Worker::RAISE;					\
}

// Core Unpickling Function
Worker::Result UnpickleWorker::Run() {
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
	if (y > MAX_VALID_INT) CORRUPT();
	Set(x, i, Store::IntToWord(y));
	is->Commit();
	PushUnpickleFrame(x, i + 1, n);
	CONTINUE();
      }
      break;
    case Pickle::NEGINT:
      {
	u_int y = is->GetUInt(); CHECK_EOB();
	if (y > MAX_VALID_INT) CORRUPT();
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
	    word w = SelFromEnv(env, index);
	    if (w == 0) CORRUPT();
	    s = String::FromWordDirect(w);
	    y = INVALID_POINTER;
	  }
	  break;
	default:
	  CORRUPT();
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
	UnpickleWorker::PushFrame(y, 0, size);
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
	UnpickleWorker::PushFrame(y, 0, size);
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
	UnpickleWorker::PushFrame(y, 0, size);
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
	TransformWorker::PushFrame(future, tuple);
	UnpickleWorker::PushFrame(tuple->ToWord(), 0, 2);
	CONTINUE();
      }
      break;
    case Pickle::REF:
      {
	u_int index = is->GetUInt(); CHECK_EOB();
	word w = SelFromEnv(env, index);
	if (w == 0) CORRUPT();
	Set(x, i, w);
	is->Commit();
	PushUnpickleFrame(x, i + 1, n);
	CONTINUE();
      }
    default:
      CORRUPT();
    }
  }
}

const char *UnpickleWorker::Identify() {
  return "UnpickleWorker";
}

void UnpickleWorker::DumpFrame(word frameWord) {
  UnpickleFrame *frame = UnpickleFrame::FromWordDirect(frameWord);
  std::fprintf(stderr, "Unpickle subnode %d of %d\n",
	       frame->GetIndex(), frame->GetNumberOfElements());
}

// PickleUnpackWorker Frame
class PickleUnpackFrame: private StackFrame {
private:
  enum { TUPLE_POS, SIZE };
public:
  using Block::ToWord;

  // PickleUnpackFrame Constructor
  static PickleUnpackFrame *New(Worker *worker, Tuple *x) {
    StackFrame *frame = StackFrame::New(PICKLE_UNPACK_FRAME, worker, SIZE);
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

// PickleUnpackWorker
class PickleUnpackWorker: public Worker {
private:
  static PickleUnpackWorker *self;
  // PickleUnpackWorker Constructor
  PickleUnpackWorker(): Worker() {}
public:
  // PickleUnpackWorker Static Constructor
  static void Init() {
    self = new PickleUnpackWorker();
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
// PickleUnpackWorker Functions
//
PickleUnpackWorker *PickleUnpackWorker::self;

void PickleUnpackWorker::PushFrame(Tuple *x) {
  Scheduler::PushFrame(PickleUnpackFrame::New(self, x)->ToWord());
}

Worker::Result PickleUnpackWorker::Run() {
  PickleUnpackFrame *frame =
    PickleUnpackFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  Tuple *x = frame->GetTuple();
  Scheduler::nArgs = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = x->Sel(0);
  return Worker::CONTINUE;
}

const char *PickleUnpackWorker::Identify() {
  return "PickleUnpackWorker";
}

void PickleUnpackWorker::DumpFrame(word) {
  std::fprintf(stderr, "Pickle Unpack\n");
}


// PickleLoadWorker Frame
class PickleLoadFrame: private StackFrame {
private:
  enum { TUPLE_POS, SIZE };
public:
  using Block::ToWord;

  // PickleLoadFrame Constructor
  static PickleLoadFrame *New(Worker *worker, Tuple *x) {
    StackFrame *frame = StackFrame::New(PICKLE_LOAD_FRAME, worker, SIZE);
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

// PickleLoadWorker
class PickleLoadWorker: public Worker {
private:
  static PickleLoadWorker *self;
  // PickleLoadWorker Constructor
  PickleLoadWorker(): Worker() {}
public:
  // PickleLoadWorker Static Constructor
  static void Init() {
    self = new PickleLoadWorker();
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
// PickleLoadWorker Functions
//
PickleLoadWorker *PickleLoadWorker::self;

void PickleLoadWorker::PushFrame(Tuple *x) {
  Scheduler::PushFrame(PickleLoadFrame::New(self, x)->ToWord());
}

Worker::Result PickleLoadWorker::Run() {
  PickleLoadFrame *frame =
    PickleLoadFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  InputStream *is = UnpickleArgs::GetInputStream();
  Tuple *x = frame->GetTuple();
  is->Close();
  Scheduler::nArgs = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = x->Sel(0);
  return Worker::CONTINUE;
}

const char *PickleLoadWorker::Identify() {
  return "PickleLoadWorker";
}

void PickleLoadWorker::DumpFrame(word) {
  std::fprintf(stderr, "Pickle Load\n");
}

static const u_int INITIAL_TABLE_SIZE = 16; // to be checked

#if BOTTOM_UP_PICKLER
#include "NewUnpickler.cc"

// PickleCheckWorker
// checks whether the pickle is old or new style

class PickleCheckWorker: public Worker {
private:
  static PickleCheckWorker *self;
  // PickleCheckWorker Constructor
  PickleCheckWorker(): Worker() {}
public:
  // PickleCheckWorker Static Constructor
  static void Init() {
    self = new PickleCheckWorker();
  }
  // Frame Handling
  static void PushFrame();
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

PickleCheckWorker *PickleCheckWorker::self;

void PickleCheckWorker::PushFrame() {
  Scheduler::PushFrame(StackFrame::New(MIN_STACK_FRAME, self)->ToWord());
}

Worker::Result PickleCheckWorker::Run() {
  StackFrame *frame = StackFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  InputStream *is = UnpickleArgs::GetInputStream();

  u_char tag = is->GetByte(); CHECK_EOB();
  bool flag = false;
  u_int stackSize = 0;
  u_int localsSize = 0;

  if (static_cast<Pickle::Tag>(tag) == Pickle::POSINT) {
    is->Commit();
    stackSize = is->GetUInt(); CHECK_EOB();
    is->Commit();
    if (stackSize > MAX_VALID_INT) CORRUPT();
    if (!is->IsEOF()) {
      localsSize = is->GetUInt(); CHECK_EOB();
      is->Commit();
      if (localsSize > MAX_VALID_INT) CORRUPT();
      flag = true;
    }
  }

  if (flag) {
    NewPickleLoadWorker::PushFrame();
    NewUnpickleWorker::PushFrame(stackSize, localsSize);
    NewUnpickleArgs::New(is);
  } else {
    is->Rewind();
    Tuple *x = Tuple::New(1);
    Stack *env = Stack::New(INITIAL_TABLE_SIZE);
    PickleLoadWorker::PushFrame(x);
    UnpickleWorker::PushFrame(x->ToWord(), 0, 1);
    UnpickleArgs::New(is, env->ToWord());
  }

  return Worker::CONTINUE;
}

const char *PickleCheckWorker::Identify() {
  return "PickleCheckWorker";
}

void PickleCheckWorker::DumpFrame(word) {
  std::fprintf(stderr, "Pickle Check\n");
}

#endif

//
// Unpickler Functions
//


Worker::Result Unpickler::Unpack(String *s) {
  InputStream *is = InputStream::NewFromString(s);
  Scheduler::PopFrame();
  Stack *env = Stack::New(INITIAL_TABLE_SIZE);
#if BOTTOM_UP_PICKLER
  PickleCheckWorker::PushFrame();
  UnpickleArgs::New(is, env->ToWord());
#else
  Tuple *x = Tuple::New(1);
  PickleUnpackWorker::PushFrame(x);
  UnpickleWorker::PushFrame(x->ToWord(), 0, 1);
  UnpickleArgs::New(is, env->ToWord());
#endif
  return Worker::CONTINUE;
}

Worker::Result Unpickler::Load(String *filename) {
  char *szFileName = filename->ExportC();
  InputStream *is = InputStream::NewFromFile(szFileName);
  if (is->HasException()) {
    Scheduler::currentData = Store::IntToWord(0); // to be done
    fprintf(stderr, "file '%s' not found\n", szFileName);
    Scheduler::currentBacktrace = Backtrace::New(Scheduler::GetAndPopFrame());
    return Worker::RAISE;
  }

  Scheduler::PopFrame();
  Stack *env = Stack::New(INITIAL_TABLE_SIZE);
#if BOTTOM_UP_PICKLER
  PickleCheckWorker::PushFrame();
  UnpickleArgs::New(is, env->ToWord());
  return Worker::CONTINUE;
#else
  Tuple *x = Tuple::New(1);
  PickleLoadWorker::PushFrame(x);
  UnpickleWorker::PushFrame(x->ToWord(), 0, 1);
  UnpickleArgs::New(is, env->ToWord());
  return Worker::CONTINUE;
#endif
}

word Unpickler::Corrupt;

void Unpickler::Init() {
  // Setup internal Workers
#if BOTTOM_UP_PICKLER
  PickleCheckWorker::Init();
  NewInputWorker::Init();
  NewUnpickleWorker::Init();
  NewPickleUnpackWorker::Init();
  NewPickleLoadWorker::Init();
#endif
  InputStream::Init();
  InputWorker::Init();
  TransformWorker::Init();
  UnpickleWorker::Init();
  PickleUnpackWorker::Init();
  PickleLoadWorker::Init();
  handlerTable = ChunkMap::New(initialHandlerTableSize)->ToWord();
  RootSet::Add(handlerTable);
}

void Unpickler::InitExceptions() {
  Corrupt = UniqueConstructor::New(String::New("Component.Corrupt"))->ToWord();
  RootSet::Add(Corrupt);
}

void Unpickler::RegisterHandler(String *name, handler handler) {
  word x = Store::UnmanagedPointerToWord((void *) handler);
  ChunkMap::FromWordDirect(handlerTable)->Put(name->ToWord(), x);
}
