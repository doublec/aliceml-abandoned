//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//   Guido Tack <tack@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//   Guido Tack, 2003
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
#include "generic/UniqueString.hh"
#include "generic/Broker.hh"
#include "generic/ZLib.hh"

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
    return STATIC_CAST(InputStream *, p);
  }

  // InputStream Methods
  gzFile GetFile() {
    Assert(GetLabel() == (BlockLabel) FILE_INPUT_STREAM);
    return STATIC_CAST(gzFile, Store::DirectWordToUnmanagedPointer(GetArg(FILE_POS)));
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
	  StackFrame *frame = Scheduler::GetFrame();
	  Scheduler::currentBacktrace = Backtrace::New(frame->Clone());
	  Scheduler::PopFrame();
	  return Worker::RAISE;
	} else {
	  tl += nread;
	  SetTl(tl);
	  Scheduler::PopFrame();
	  return Worker::CONTINUE;
	}
      }
    case STRING_INPUT_STREAM:
      {
	// there is no more data: raise Corrupt exception
	Scheduler::currentData = Unpickler::Corrupt;
	StackFrame *frame = Scheduler::GetFrame();
	Scheduler::currentBacktrace = Backtrace::New(frame->Clone());
	Scheduler::PopFrame();
	return Worker::RAISE;
      }
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

static const u_int INITIAL_TABLE_SIZE = 16; // to be checked

// Pickle Arguments
class UnpickleArgs {
private:
  enum { STREAM_POS, RESULT_POS, SIZE };
public:
  static void New(InputStream *is) {
    Scheduler::nArgs = SIZE;
    Scheduler::currentArgs[STREAM_POS] = is->ToWord();
    Scheduler::currentArgs[RESULT_POS] = Store::IntToWord(0);
  }
  static InputStream *GetInputStream() {
    Assert(Scheduler::nArgs == SIZE);
    return InputStream::FromWordDirect(Scheduler::currentArgs[STREAM_POS]);
  }
  static void SetResult(word result) {
    Assert(Scheduler::nArgs == SIZE);
    Scheduler::currentArgs[RESULT_POS] = result;
  }
  static word GetResult() {
    return Scheduler::currentArgs[RESULT_POS];
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
  virtual u_int GetFrameSize(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *sFrame);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

//
// InputWorker Functions
//
InputWorker *InputWorker::self;

void InputWorker::PushFrame() {
  NEW_STACK_FRAME(frame, self, 0);
  frame = frame; // Ignored
}

u_int InputWorker::GetFrameSize(StackFrame *sFrame) {
  Assert(sFrame->GetWorker() == this);
  return sFrame->GetSize();
}

Worker::Result InputWorker::Run(StackFrame *) {
  return UnpickleArgs::GetInputStream()->FillBuffer();
}

const char *InputWorker::Identify() {
  return "InputWorker";
}

void InputWorker::DumpFrame(StackFrame *) {
  std::fprintf(stderr, "Fill Unpickling Buffer\n");
}

// TransformWorker
class TransformFrame : private StackFrame {
private:
  enum { FUTURE_POS, NAME_POS, ARG_POS, SIZE };
public:
  static TransformFrame *New(Worker *worker,
			     Future *future,
			     String *name,
			     word arg) {
    NEW_STACK_FRAME(frame, worker, SIZE);
    frame->InitArg(FUTURE_POS, future->ToWord());
    frame->InitArg(NAME_POS, name->ToWord());
    frame->InitArg(ARG_POS, arg);
    return STATIC_CAST(TransformFrame *, frame);
  }
  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  Future *GetFuture() {
    return
      STATIC_CAST(Future *, Store::DirectWordToTransient(GetArg(FUTURE_POS)));
  }
  String *GetName() {
    return String::FromWordDirect(GetArg(NAME_POS));
  }
  word GetArgument() {
    return GetArg(ARG_POS);
  }
};

class TransformWorker : public Worker {
private:
  static TransformWorker *self;
  TransformWorker(): Worker() {}
public:
  static void Init() {
    self = new TransformWorker();
  }

  static void PushFrame(Future *future, String *name, word arg);
  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Result Run(StackFrame *sFrame);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

TransformWorker *TransformWorker::self;

void TransformWorker::PushFrame(Future *future,
				String *name,
				word arg) {
  TransformFrame::New(self, future, name, arg);
}

u_int TransformWorker::GetFrameSize(StackFrame *sFrame) {
  TransformFrame *frame = STATIC_CAST(TransformFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

const char *TransformWorker::Identify() {
  return "TransformWorker";
}

void TransformWorker::DumpFrame(StackFrame *) {
  std::fprintf(stderr, "TransformWorker Frame\n");
}

Worker::Result TransformWorker::Run(StackFrame *sFrame) {
  TransformFrame *frame = STATIC_CAST(TransformFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);

  Future *f = frame->GetFuture();
  String *name = frame->GetName();
  word argument = frame->GetArgument();



  ChunkMap *map = ChunkMap::FromWordDirect(handlerTable);
  
  if (map->IsMember(name->ToWord())) {
    Unpickler::handler handler = (Unpickler::handler)
      Store::DirectWordToUnmanagedPointer(map->Get(name->ToWord()));
    f->Become(REF_LABEL, handler(argument));
  } else {
    Error("TransformWorker: unknown transform");
//     Scheduler::currentData = Unpickler::Corrupt;
//     Scheduler::currentBacktrace =
//       Backtrace::New(STATIC_CAST(StackFrame *, frame)->Clone());
//     Scheduler::PopFrame();
//     return Worker::RAISE;
  }

  Scheduler::PopFrame(frame->GetSize());
  if (StatusWord::GetStatus() != 0)
    return Worker::PREEMPT;
  else
    return Worker::CONTINUE;  
}

// ApplyTransform Function
static inline
bool ApplyTransform(String *name, word argument, word *newBlock,
		    Future *f) {
  Assert(name != INVALID_POINTER);
  ChunkMap *map = ChunkMap::FromWordDirect(handlerTable);

  if (map->IsMember(name->ToWord())) {
    Unpickler::handler handler = (Unpickler::handler)
      Store::DirectWordToUnmanagedPointer(map->Get(name->ToWord()));
    *newBlock = handler(argument);
    return false;
  } else {
    u_char *p = name->GetValue();
    u_int i = 0;
    while (p[i] != '.') {
      Assert(i < name->GetSize());
      i++;
    }
    String *languageId = String::New(reinterpret_cast<char *>(p), i);
    TransformWorker::PushFrame(f, name, argument);
    Broker::Load(languageId);	  
    *newBlock = f->ToWord();
    return true;
  }
}

// PickleUnpackWorker Frame
class PickleUnpackFrame: private StackFrame {
private:
  enum { SIZE };
public:
  // PickleUnpackFrame Constructor
  static PickleUnpackFrame *New(Worker *worker) {
    NEW_STACK_FRAME(frame, worker, SIZE);
    return STATIC_CAST(PickleUnpackFrame *, frame);
  }
  // PickleUnpackFrame Accessors
  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
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
  static void PushFrame();
  virtual u_int GetFrameSize(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *sFrame);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

//
// PickleUnpackWorker Functions
//
PickleUnpackWorker *PickleUnpackWorker::self;

void PickleUnpackWorker::PushFrame() {
  PickleUnpackFrame::New(self);
}

u_int PickleUnpackWorker::GetFrameSize(StackFrame *sFrame) {
  PickleUnpackFrame *frame = STATIC_CAST(PickleUnpackFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result PickleUnpackWorker::Run(StackFrame *sFrame) {
  PickleUnpackFrame *frame = STATIC_CAST(PickleUnpackFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  Scheduler::PopFrame(frame->GetSize());
  word result = UnpickleArgs::GetResult();

  Scheduler::nArgs = 1;
  Scheduler::currentArgs[0] = result;
  return Worker::CONTINUE;
}

const char *PickleUnpackWorker::Identify() {
  return "PickleUnpackWorker";
}

void PickleUnpackWorker::DumpFrame(StackFrame *) {
  std::fprintf(stderr, "Pickle Unpack\n");
}

// PickleLoadWorker Frame
class PickleLoadFrame: private StackFrame {
private:
  enum { SIZE };
public:
  // PickleLoadFrame Constructor
  static PickleLoadFrame *New(Worker *worker) {
    NEW_STACK_FRAME(frame, worker, SIZE);
    return STATIC_CAST(PickleLoadFrame *, frame);
  }
  // PickleLoadFrame Accessors
  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
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
  static void PushFrame();
  virtual u_int GetFrameSize(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *sFrame);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

//
// PickleLoadWorker Functions
//
PickleLoadWorker *PickleLoadWorker::self;

void PickleLoadWorker::PushFrame() {
  PickleLoadFrame::New(self);
}

u_int PickleLoadWorker::GetFrameSize(StackFrame *sFrame) {
  PickleLoadFrame *frame = STATIC_CAST(PickleLoadFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result PickleLoadWorker::Run(StackFrame *sFrame) {
  PickleLoadFrame *frame = STATIC_CAST(PickleLoadFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  Scheduler::PopFrame(frame->GetSize());
  InputStream *is = UnpickleArgs::GetInputStream();
  is->Close();
  
  word result = UnpickleArgs::GetResult();

  Scheduler::nArgs = 1;
  Scheduler::currentArgs[0] = result;
  return Worker::CONTINUE;
}

const char *PickleLoadWorker::Identify() {
  return "PickleLoadWorker";
}

void PickleLoadWorker::DumpFrame(StackFrame *) {
  std::fprintf(stderr, "Pickle Load\n");
}


// UnpickleWorker Frame
class UnpickleFrame: private StackFrame {
private:
  enum { SIZE_POS, TOP_POS, LOCALS_POS, SIZE };
public:
  // UnpickleFrame Constructor
  static UnpickleFrame *New(Worker *worker, int stackSize, int locals) {
    locals = locals + 100;
    u_int frSize = SIZE+stackSize+locals+2000;
    NEW_STACK_FRAME(frame, worker, frSize);
    frame->InitArg(SIZE_POS, frame->GetSize() + frSize);
    frame->InitArg(TOP_POS, SIZE+locals+1);
    frame->InitArg(LOCALS_POS, locals+1);
    for (u_int i=locals+2; i--;) {
      frame->InitArg(SIZE+i, Store::IntToWord(0));
    }
    return STATIC_CAST(UnpickleFrame *, frame);
  }

  u_int GetSize() {
    return Store::DirectWordToInt(StackFrame::GetArg(SIZE_POS));
  }
  void Push(word value);
  word Pop();
  word Top();
  void Store(u_int i, word value);
  word Load(u_int i);
  void PushStore(u_int i, word value);
  void PushAddr(u_int i);

};

void UnpickleFrame::Push(word value) {
  u_int tp = Store::DirectWordToInt(GetArg(TOP_POS));
  tp++;
  Assert(tp>=Store::DirectWordToInt(GetArg(LOCALS_POS)));
  Assert(tp<GetSize());
  ReplaceArg(tp, value);
  ReplaceArg(TOP_POS, Store::IntToWord(tp));
}

word UnpickleFrame::Pop() {
  u_int tp = Store::DirectWordToInt(GetArg(TOP_POS));
  Assert(tp >= (u_int) Store::DirectWordToInt(GetArg(LOCALS_POS)));
  word result = GetArg(tp);
  tp--;
  ReplaceArg(TOP_POS, Store::IntToWord(tp));
  return result;
}

word UnpickleFrame::Top() {
  u_int tp = Store::DirectWordToInt(GetArg(TOP_POS));
  Assert(tp >= (u_int) Store::DirectWordToInt(GetArg(LOCALS_POS)));
  return GetArg(tp);
}

void UnpickleFrame::Store(u_int i, word value) {
  Assert(i < (u_int) Store::DirectWordToInt(GetArg(LOCALS_POS)));
  Assert( Store::WordToInt(GetArg(SIZE+i)) == 0);
  ReplaceArg(SIZE+i, value);
}

word UnpickleFrame::Load(u_int i) {
  Assert(i < (u_int) Store::DirectWordToInt(GetArg(LOCALS_POS)));
  Assert( Store::WordToInt(GetArg(SIZE+i)) == INVALID_INT);
  return GetArg(SIZE+i);
}

void UnpickleFrame::PushAddr(u_int i) {
  Assert(i < (u_int) Store::DirectWordToInt(GetArg(LOCALS_POS)));
  Assert( Store::WordToInt(GetArg(SIZE+i)) == INVALID_INT);
  u_int tp = Store::DirectWordToInt(GetArg(TOP_POS));
  tp++;
  Assert(tp>=Store::DirectWordToInt(GetArg(LOCALS_POS)));
  Assert(tp<GetSize());
  ReplaceArg(tp, GetArg(SIZE+i));
  ReplaceArg(TOP_POS, Store::IntToWord(tp));
}

void UnpickleFrame::PushStore(u_int i, word value) {
  Assert(i < (u_int) Store::DirectWordToInt(GetArg(LOCALS_POS)));
  Assert( Store::WordToInt(GetArg(SIZE+i)) == 0);
  ReplaceArg(SIZE+i, value);

  u_int tp = Store::DirectWordToInt(GetArg(TOP_POS));
  tp++;
  Assert(tp>=Store::DirectWordToInt(GetArg(LOCALS_POS)));
  Assert(tp<GetSize());
  ReplaceArg(tp, value);
  ReplaceArg(TOP_POS, Store::IntToWord(tp));
}


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
  static void PushFrame(int, int);
  virtual u_int GetFrameSize(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *sFrame);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

// The StoreAbstraction makes the business of transforming CONCRETE and 
// TRANSFORM nodes invisible to the unpickler

class StoreAbstraction {
public:
  static bool AllocBlock(BlockLabel label,
			 u_int size,
			 UnpickleFrame *frame,
			 word *newBlock) {
    switch(label) {
    case TRANSFORM_LABEL:
      {
	Assert(size == 2);

	String *name   = String::FromWordDirect(frame->Pop());
	word argument  = frame->Pop();
	return ApplyTransform(name, argument, newBlock, Future::New());
      }
      break;
    case CLOSURE_LABEL:
      {
	word cc    = Store::IntToWord(0); // will be replaced by concrete code
	*newBlock   = Closure::New(cc, size - 1)->ToWord();
	Block *b = Store::DirectWordToBlock(*newBlock);
	for (u_int i=0; i<size; i++) {
	  b->InitArg(i, frame->Pop());
	}
	return false;
      }
      break;
    default:
      {
	Block *b = Store::AllocBlock(label, size);
	for (u_int i=0; i<size; i++) {
	  b->InitArg(i, frame->Pop());
	}
	*newBlock = b->ToWord();
	return false;
      }      
      break;
    }
  }


  static word AnnounceBlock(BlockLabel label, u_int size) {
    switch(label) {
    case TRANSFORM_LABEL:
      {
	Future *future = Future::New();
	return future->ToWord();
      }
      break;
    case CLOSURE_LABEL:
      {
	word cc    = Store::IntToWord(0); // will be replaced by concrete code
	word y     = Closure::New(cc, size - 1)->ToWord();
	return y;
      }
      break;
    default:
      {
	return Store::AllocBlock(label, size)->ToWord();
      }
      break;
    }
  }

  
  static bool MakeFulfilledBlock(word future,
				 UnpickleFrame *frame,
				 word *newBlock) {
    if (Store::WordToTransient(future) == INVALID_POINTER) {
      Block *b = Store::DirectWordToBlock(future);
      u_int size = b->GetSize();
      for (u_int i=0; i<size; i++) {
	b->ReplaceArg(i, frame->Pop());
      }
      *newBlock = b->ToWord();
      return false;
    } else {
      Future *f = STATIC_CAST(Future *,	Store::DirectWordToTransient(future));

      word argument  = frame->Pop();
      String *name   = String::FromWordDirect(frame->Pop());
	
      return ApplyTransform(name, argument, newBlock, f);
    }
  }  

};


//
// UnpickleWorker Functions
//
UnpickleWorker *UnpickleWorker::self;

void UnpickleWorker::PushFrame(int stackSize, int localsSize) {
  UnpickleFrame::New(self, stackSize, localsSize);
}

// Unpickle Helpers

// End of Buffer requires rereading;
// therefore we reinstall the old task stack
//--** to be done: more efficient solution
#define NCHECK_EOB() {				\
  if (is->IsEOB()) {				\
    InputWorker::PushFrame();		\
    return Worker::CONTINUE;			\
  }						\
}

#define NCONTINUE() {				\
  if (StatusWord::GetStatus() != 0)		\
    return Worker::PREEMPT;			\
  else						\
    return Worker::CONTINUE;			\
}

#define NCORRUPT() {					\
  Scheduler::currentData = Unpickler::Corrupt;		\
  StackFrame *frame = Scheduler::GetFrame();            \
  Scheduler::currentBacktrace =				\
    Backtrace::New(frame->Clone());	                \
  Scheduler::PopFrame();                                \
  return Worker::RAISE;					\
}


u_int UnpickleWorker::GetFrameSize(StackFrame *sFrame) {
  UnpickleFrame *frame = STATIC_CAST(UnpickleFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

// Core Unpickling Function
Worker::Result UnpickleWorker::Run(StackFrame *sFrame) {
  UnpickleFrame *frame = STATIC_CAST(UnpickleFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);

  InputStream *is = UnpickleArgs::GetInputStream();

  //  NUStack *st = UnpickleArgs::GetStack();

  for(;;) {
    word newBlock;
    bool mustContinue = false;
    u_char tag = is->GetByte(); NCHECK_EOB();
    switch (STATIC_CAST(Pickle::Tag, tag)) {
    case Pickle::INIT:
      {
	NCORRUPT();
	// May not occur except at the beginning
      }
      break;
    case Pickle::STORE:
      {
	u_int addr = is->GetUInt(); NCHECK_EOB();
	is->Commit();

	if (addr > MAX_VALID_INT) NCORRUPT();
	word top = frame->Top();
	frame->Store(addr, top);
      }
      break;
    case Pickle::LOAD:
      {
	u_int addr = is->GetUInt(); NCHECK_EOB();
	is->Commit();

	if (addr > MAX_VALID_INT) NCORRUPT();
	frame->PushAddr(addr);
      }
      break;
    case Pickle::POSINT:
      {
	u_int y = is->GetUInt(); NCHECK_EOB();
	is->Commit();

	if (y > MAX_VALID_INT) NCORRUPT();
	
	frame->Push(Store::IntToWord(y));
      }
      break;
    case Pickle::NEGINT:
      {
	u_int y = is->GetUInt(); NCHECK_EOB();
	is->Commit();

	if (y > MAX_VALID_INT) NCORRUPT();
	
	frame->Push(Store::IntToWord(-(y + 1)));
      }
      break;
    case Pickle::CHUNK: 
      {
	u_int size    = is->GetUInt(); NCHECK_EOB();

	u_char *bytes = is->GetBytes(size); NCHECK_EOB();
	is->Commit();
	Chunk *y      = Store::AllocChunk(size);
	std::memcpy(y->GetBase(), bytes, size);
	frame->Push(y->ToWord());
      }
      break;
    case Pickle::UNIQUE: 
      {
	is->Commit();
	word top = frame->Pop();
	Assert(Store::WordToChunk(top) != INVALID_POINTER);
	String *s = String::FromWordDirect(top);
	word wUnique = UniqueString::New(s)->ToWord();
	frame->Push(wUnique);
      }
      break;
    case Pickle::BLOCK: 
      {
	u_int label  = is->GetUInt(); NCHECK_EOB();
	u_int size   = is->GetUInt(); NCHECK_EOB();
	is->Commit();

	mustContinue =
	  StoreAbstraction::AllocBlock(STATIC_CAST(BlockLabel, label),
				       size, frame, &newBlock);
	frame->Push(newBlock);
      }
      break;
    case Pickle::TUPLE:
      {
	u_int size = is->GetUInt(); NCHECK_EOB();
	is->Commit();

	mustContinue = 
	  StoreAbstraction::AllocBlock(TUPLE_LABEL,
				       size, frame, &newBlock);
	frame->Push(newBlock);
      }
      break;
    case Pickle::CLOSURE:
      {
	u_int size = is->GetUInt(); NCHECK_EOB();
	is->Commit();

	mustContinue = 
	  StoreAbstraction::AllocBlock(CLOSURE_LABEL,
				       size, frame, &newBlock);
	frame->Push(newBlock);
      }
      break;
    case Pickle::TRANSFORM:
      {
	is->Commit();
	mustContinue =
	  StoreAbstraction::AllocBlock(TRANSFORM_LABEL,
				       2, frame, &newBlock);
	frame->Push(newBlock);
      }
      break;
    case Pickle::aBLOCK:
      {
	u_int label  = is->GetUInt(); NCHECK_EOB();
	u_int size   = is->GetUInt(); NCHECK_EOB();
	u_int addr   = is->GetUInt(); NCHECK_EOB();
	is->Commit();

	word block =
	  StoreAbstraction::AnnounceBlock(STATIC_CAST(BlockLabel, label),
					  size);
	frame->PushStore(addr, block);
      }
      break;
    case Pickle::aTUPLE:
      {
	u_int size   = is->GetUInt(); NCHECK_EOB();
	u_int addr   = is->GetUInt(); NCHECK_EOB();
	is->Commit();

	word block =
	  StoreAbstraction::AnnounceBlock(TUPLE_LABEL,
					  size);
	frame->PushStore(addr, block);
      }
      break;
    case Pickle::aCLOSURE:
      {
	u_int size   = is->GetUInt(); NCHECK_EOB();
	u_int addr   = is->GetUInt(); NCHECK_EOB();
	is->Commit();

	word block =
	  StoreAbstraction::AnnounceBlock(CLOSURE_LABEL,
					  size);
	frame->PushStore(addr, block);
      }
      break;
    case Pickle::aTRANSFORM:
      {
	u_int addr   = is->GetUInt(); NCHECK_EOB();
	is->Commit();

	word block =
	  StoreAbstraction::AnnounceBlock(TRANSFORM_LABEL,
					  2);
	frame->PushStore(addr, block);	
      }
      break;
    case Pickle::FULFILL:
      {
	u_int addr   = is->GetUInt(); NCHECK_EOB();
	is->Commit();
	if (addr > MAX_VALID_INT) NCORRUPT();

	word future = frame->Load(addr);
	mustContinue = StoreAbstraction::MakeFulfilledBlock(future,
							    frame,
							    &newBlock);
	frame->Push(newBlock);
      }
      break;
    case Pickle::ENDOFSTREAM:
      {
	UnpickleArgs::SetResult(frame->Load(0));
	Scheduler::PopFrame(frame->GetSize());
	NCONTINUE();
      }
      break;
    default:
      {
	NCORRUPT();
      }
      break;
    }
    if (StatusWord::GetStatus() != 0) {
      return Worker::PREEMPT;
    } else if (mustContinue) {
      return Worker::CONTINUE;
    }
  }

  Scheduler::PopFrame(frame->GetSize());
  NCONTINUE();
}

const char *UnpickleWorker::Identify() {
  return "UnpickleWorker";
}

void UnpickleWorker::DumpFrame(StackFrame *) {
  std::fprintf(stderr, "Unpickling\n");
}

// PickleCheckWorker
// checks whether the pickle is old or new style

#define PCHECK_EOB() {				\
   if (is->IsEOB()) {				\
     InputWorker::PushFrame();			\
     return Worker::CONTINUE;			\
   }						\
 }

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
  virtual u_int GetFrameSize(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *sFrame);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

PickleCheckWorker *PickleCheckWorker::self;

void PickleCheckWorker::PushFrame() {
  NEW_STACK_FRAME(frame, self, 0);
  frame = frame; // Ignored
}

u_int PickleCheckWorker::GetFrameSize(StackFrame *sFrame) {
  Assert(sFrame->GetWorker() == this);
  return sFrame->GetSize();
}

Worker::Result PickleCheckWorker::Run(StackFrame *sFrame) {
  InputStream *is = UnpickleArgs::GetInputStream();

  u_char tag = is->GetByte(); PCHECK_EOB();
  bool flag = false;
  u_int stackSize = 0;
  u_int localsSize = 0;

  if (STATIC_CAST(Pickle::Tag, tag) == Pickle::INIT) {
    is->Commit();
    stackSize = is->GetUInt(); PCHECK_EOB();
    is->Commit();
    if (stackSize > MAX_VALID_INT) NCORRUPT();
    if (!is->IsEOF()) {
      localsSize = is->GetUInt(); PCHECK_EOB();
      is->Commit();
      if (localsSize > MAX_VALID_INT) NCORRUPT();
      flag = true;
    }
  }

  if (flag) {
    Scheduler::PopFrame(sFrame->GetSize());
    PickleLoadWorker::PushFrame();
    UnpickleWorker::PushFrame(stackSize, localsSize);
    return Worker::CONTINUE;
  } else {
    NCORRUPT();
  }

}

const char *PickleCheckWorker::Identify() {
  return "PickleCheckWorker";
}

void PickleCheckWorker::DumpFrame(StackFrame *) {
  std::fprintf(stderr, "Pickle Check\n");
}

//
// Unpickler Functions
//


Worker::Result Unpickler::Unpack(String *s) {
  InputStream *is = InputStream::NewFromString(s);
  Scheduler::PopFrame();

  PickleCheckWorker::PushFrame();
  UnpickleArgs::New(is);

  return Worker::CONTINUE;
}

Worker::Result Unpickler::Load(String *filename) {
  char *szFileName = filename->ExportC();
  InputStream *is = InputStream::NewFromFile(szFileName);
  if (is->HasException()) {
    Scheduler::currentData = Store::IntToWord(0); // to be done
    fprintf(stderr, "file '%s' not found\n", szFileName);
    StackFrame *frame = Scheduler::GetFrame();
    Scheduler::currentBacktrace = Backtrace::New(frame->Clone());
    Scheduler::PopFrame();
    return Worker::RAISE;
  }

  Scheduler::PopFrame();

  PickleCheckWorker::PushFrame();
  UnpickleArgs::New(is);

  return Worker::CONTINUE;
}

word Unpickler::Corrupt;

void Unpickler::Init() {
  // Setup internal Workers
  PickleCheckWorker::Init();
  InputWorker::Init();
  UnpickleWorker::Init();
  PickleUnpackWorker::Init();
  PickleLoadWorker::Init();
  TransformWorker::Init();
  InputStream::Init();
  handlerTable = ChunkMap::New(initialHandlerTableSize)->ToWord();
  RootSet::Add(handlerTable);
  Corrupt = UniqueString::New(String::New("Component.Corrupt"))->ToWord();
  RootSet::Add(Corrupt);
}

void Unpickler::RegisterHandler(String *name, handler handler) {
  word x = Store::UnmanagedPointerToWord((void *) handler);
  ChunkMap::FromWordDirect(handlerTable)->Put(name->ToWord(), x);
}
