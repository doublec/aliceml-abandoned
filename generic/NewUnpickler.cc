//
// Author:
//   Guido Tack <tack@ps.uni-sb.de>
//
//
// Copyright:
//   Guido Tack, 2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "NewPickle.hh"

// Pickle Arguments
class NewUnpickleArgs {
private:
  enum { STREAM_POS, RESULT_POS, SIZE };
public:
  static void New(InputStream *is) {
    Scheduler::nArgs = SIZE;
    Scheduler::currentArgs[STREAM_POS] = is->ToWord();
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

// NewInputWorker
class NewInputWorker: public Worker {
private:
  static NewInputWorker *self;
  // NewInputWorker Constructor
  NewInputWorker(): Worker() {}
public:
  // NewInputWorker Static Constructor
  static void Init() {
    self = new NewInputWorker();
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
// NewInputWorker Functions
//
NewInputWorker *NewInputWorker::self;

void NewInputWorker::PushFrame() {
  Scheduler::PushFrame(StackFrame::New(INPUT_FRAME, self, 0)->ToWord());
}

Worker::Result NewInputWorker::Run() {
  return NewUnpickleArgs::GetInputStream()->FillBuffer();
}

const char *NewInputWorker::Identify() {
  return "NewInputWorker";
}

void NewInputWorker::DumpFrame(word) {
  std::fprintf(stderr, "Fill Unpickling Buffer\n");
}


// NewPickleUnpackWorker Frame
class NewPickleUnpackFrame: private StackFrame {
private:
  enum { SIZE };
public:
  using Block::ToWord;

  // NewPickleUnpackFrame Constructor
  static NewPickleUnpackFrame *New(Worker *worker) {
    StackFrame *frame = StackFrame::New(PICKLE_UNPACK_FRAME, worker, SIZE);
    return static_cast<NewPickleUnpackFrame *>(frame);
  }
  // NewPickleUnpackFrame Untagging
  static NewPickleUnpackFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == PICKLE_UNPACK_FRAME);
    return static_cast<NewPickleUnpackFrame *>(p);
  }
};

// NewPickleUnpackWorker
class NewPickleUnpackWorker: public Worker {
private:
  static NewPickleUnpackWorker *self;
  // NewPickleUnpackWorker Constructor
  NewPickleUnpackWorker(): Worker() {}
public:
  // NewPickleUnpackWorker Static Constructor
  static void Init() {
    self = new NewPickleUnpackWorker();
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
// NewPickleUnpackWorker Functions
//
NewPickleUnpackWorker *NewPickleUnpackWorker::self;

void NewPickleUnpackWorker::PushFrame() {
  Scheduler::PushFrame(NewPickleUnpackFrame::New(self)->ToWord());
}

Worker::Result NewPickleUnpackWorker::Run() {
  Scheduler::GetAndPopFrame();
  word result = NewUnpickleArgs::GetResult();

  Scheduler::nArgs = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = result;
  return Worker::CONTINUE;
}

const char *NewPickleUnpackWorker::Identify() {
  return "NewPickleUnpackWorker";
}

void NewPickleUnpackWorker::DumpFrame(word) {
  std::fprintf(stderr, "Pickle Unpack\n");
}

// NewPickleLoadWorker Frame
class NewPickleLoadFrame: private StackFrame {
private:
  enum { SIZE };
public:
  using Block::ToWord;

  // NewPickleLoadFrame Constructor
  static NewPickleLoadFrame *New(Worker *worker) {
    StackFrame *frame = StackFrame::New(PICKLE_LOAD_FRAME, worker, SIZE);
    return static_cast<NewPickleLoadFrame *>(frame);
  }
  // NewPickleLoadFrame Untagging
  static NewPickleLoadFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == PICKLE_LOAD_FRAME);
    return static_cast<NewPickleLoadFrame *>(p);
  }
};

// NewPickleLoadWorker
class NewPickleLoadWorker: public Worker {
private:
  static NewPickleLoadWorker *self;
  // NewPickleLoadWorker Constructor
  NewPickleLoadWorker(): Worker() {}
public:
  // NewPickleLoadWorker Static Constructor
  static void Init() {
    self = new NewPickleLoadWorker();
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
// NewPickleLoadWorker Functions
//
NewPickleLoadWorker *NewPickleLoadWorker::self;

void NewPickleLoadWorker::PushFrame() {
  Scheduler::PushFrame(NewPickleLoadFrame::New(self)->ToWord());
}

Worker::Result NewPickleLoadWorker::Run() {
  Scheduler::GetAndPopFrame();
  InputStream *is = NewUnpickleArgs::GetInputStream();
  is->Close();
  
  word result = NewUnpickleArgs::GetResult();

  Scheduler::nArgs = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = result;
  return Worker::CONTINUE;
}

const char *NewPickleLoadWorker::Identify() {
  return "NewPickleLoadWorker";
}

void NewPickleLoadWorker::DumpFrame(word) {
  std::fprintf(stderr, "Pickle Load\n");
}


// UnpickleWorker Frame
class NewUnpickleFrame: private StackFrame {
private:
  enum { TOP_POS, LOCALS_POS, SIZE };
public:
  using Block::ToWord;

  // NewUnpickleFrame Constructor
  static NewUnpickleFrame *New(Worker *worker, int stackSize, int locals) {
    StackFrame *frame = StackFrame::New(UNPICKLING_FRAME,
					worker, SIZE+stackSize+locals+1);
    frame->InitArg(TOP_POS, SIZE+locals);
    frame->InitArg(LOCALS_POS, locals);
    for (u_int i=locals; i--;) {
      frame->InitArg(SIZE+i, Store::IntToWord(0));
    }
    return static_cast<NewUnpickleFrame *>(frame);
  }
  // NewUnpickleFrame Untagging
  static NewUnpickleFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == UNPICKLING_FRAME);
    return static_cast<NewUnpickleFrame *>(p);
  }

  void Push(word value);
  word Pop();
  word Top();
  void Store(u_int i, word value);
  word Load(u_int i);
  void PushStore(u_int i, word value);
  void PushAddr(u_int i);

};

void NewUnpickleFrame::Push(word value) {
  u_int tp = Store::DirectWordToInt(GetArg(TOP_POS));
  tp++;
  ReplaceArg(tp, value);
  ReplaceArg(TOP_POS, Store::IntToWord(tp));
}

word NewUnpickleFrame::Pop() {
  u_int tp = Store::DirectWordToInt(GetArg(TOP_POS));
  Assert(tp >= (u_int) Store::DirectWordToInt(GetArg(LOCALS_POS)));
  word result = GetArg(tp);
  tp--;
  ReplaceArg(TOP_POS, Store::IntToWord(tp));
  return result;
}

word NewUnpickleFrame::Top() {
  u_int tp = Store::DirectWordToInt(GetArg(TOP_POS));
  Assert(tp >= (u_int) Store::DirectWordToInt(GetArg(LOCALS_POS)));
  return GetArg(tp);
}

void NewUnpickleFrame::Store(u_int i, word value) {
  Assert(i < (u_int) Store::DirectWordToInt(GetArg(LOCALS_POS)));
  Assert( Store::WordToInt(GetArg(SIZE+i)) == 0);
  ReplaceArg(SIZE+i, value);
}

word NewUnpickleFrame::Load(u_int i) {
  Assert(i < (u_int) Store::DirectWordToInt(GetArg(LOCALS_POS)));
  Assert( Store::WordToInt(GetArg(SIZE+i)) == INVALID_INT);
  return GetArg(SIZE+i);
}

void NewUnpickleFrame::PushAddr(u_int i) {
  Assert(i < (u_int) Store::DirectWordToInt(GetArg(LOCALS_POS)));
  Assert( Store::WordToInt(GetArg(SIZE+i)) == INVALID_INT);
  u_int tp = Store::DirectWordToInt(GetArg(TOP_POS));
  tp++;
  ReplaceArg(tp, GetArg(SIZE+i));
  ReplaceArg(TOP_POS, Store::IntToWord(tp));
}

void NewUnpickleFrame::PushStore(u_int i, word value) {
  Assert(i < (u_int) Store::DirectWordToInt(GetArg(LOCALS_POS)));
  Assert( Store::WordToInt(GetArg(SIZE+i)) == 0);
  ReplaceArg(SIZE+i, value);

  u_int tp = Store::DirectWordToInt(GetArg(TOP_POS));
  tp++;
  ReplaceArg(tp, value);
  ReplaceArg(TOP_POS, Store::IntToWord(tp));
}


// NewUnpickleWorker
class NewUnpickleWorker: public Worker {
private:
  static NewUnpickleWorker *self;
  // NewUnpickleWorker Constructor
  NewUnpickleWorker(): Worker() {}
public:
  // NewUnpickleWorker Static Constructor
  static void Init() {
    self = new NewUnpickleWorker();
  }
  // Frame Handling
  static void PushFrame(int, int);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

// The StoreAbstraction makes the business of transforming CONCRETE and 
// TRANSFORM nodes invisible to the unpickler

class StoreAbstraction {
public:
  static word AllocBlock(BlockLabel label,
			 u_int size,
			 NewUnpickleFrame *frame) {
    switch(label) {
    case TRANSFORM_LABEL:
      {
	//	fprintf(stderr, "transform found!\n");
	Assert(size == 2);

	word argument  = frame->Pop();
	String *name   = String::FromWordDirect(frame->Pop());
	
	word result = ApplyTransform(name, argument);
	return result;
      }
      break;
    case CLOSURE_LABEL:
      {
	word cc    = Store::IntToWord(0); // will be replaced by concrete code
	word y     = Closure::New(cc, size - 1)->ToWord();
	Block *b = Store::DirectWordToBlock(y);
	for (u_int i=0; i<size; i++) {
	  b->InitArg(i, frame->Pop());
	}
	return y;
      }
      break;
    default:
      {
	Block *b = Store::AllocBlock(label, size);
	for (u_int i=0; i<size; i++) {
	  b->InitArg(i, frame->Pop());
	}
	return b->ToWord();
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

  
  static Block *MakeFulfilledBlock(word future, NewUnpickleFrame *frame) {
    if (Store::WordToTransient(future) == INVALID_POINTER) {
      Block *b = Store::DirectWordToBlock(future);
      u_int size = b->GetSize();
      for (u_int i=0; i<size; i++) {
	b->ReplaceArg(i, frame->Pop());
      }
      return b;
    } else {
      Future *f = static_cast<Future *>
	(Store::DirectWordToTransient(future));

      word argument  = frame->Pop();
      String *name   = String::FromWordDirect(frame->Pop());
	
      f->Become(REF_LABEL, ApplyTransform(name, argument));
      return Store::DirectWordToBlock(f->ToWord());
    }
  }  

};


//
// NewUnpickleWorker Functions
//
NewUnpickleWorker *NewUnpickleWorker::self;

void NewUnpickleWorker::PushFrame(int stackSize, int localsSize) {
  Scheduler::PushFrame(NewUnpickleFrame::New(self, 
					     stackSize, 
					     localsSize)->ToWord());
}

// Unpickle Helpers

// End of Buffer requires rereading;
// therefore we reinstall the old task stack
//--** to be done: more efficient solution
#define NCHECK_EOB() {				\
  if (is->IsEOB()) {				\
    NewInputWorker::PushFrame();		\
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
  Scheduler::currentBacktrace =				\
    Backtrace::New(Scheduler::GetAndPopFrame());	\
  return Worker::RAISE;					\
}

// Core Unpickling Function
Worker::Result NewUnpickleWorker::Run() {
  NewUnpickleFrame *frame =
    NewUnpickleFrame::FromWordDirect(Scheduler::GetFrame());

  InputStream *is = NewUnpickleArgs::GetInputStream();

  //  NUStack *st = NewUnpickleArgs::GetStack();

  for(;;) {
    u_char tag = is->GetByte(); NCHECK_EOB();
    switch (static_cast<NewPickle::Tag>(tag)) {
    case NewPickle::INIT:
      {
	NCORRUPT();
	// May not occur except at the beginning
      }
      break;
    case NewPickle::STORE:
      {
	u_int addr = is->GetUInt(); NCHECK_EOB();
	is->Commit();

	if (addr > MAX_VALID_INT) NCORRUPT();
	word top = frame->Top();
	frame->Store(addr, top);
      }
      break;
    case NewPickle::LOAD:
      {
	u_int addr = is->GetUInt(); NCHECK_EOB();
	is->Commit();

	if (addr > MAX_VALID_INT) NCORRUPT();
	frame->PushAddr(addr);
      }
      break;
    case NewPickle::POSINT:
      {
	u_int y = is->GetUInt(); NCHECK_EOB();
	is->Commit();

	if (y > MAX_VALID_INT) NCORRUPT();
	
	frame->Push(Store::IntToWord(y));
      }
      break;
    case NewPickle::NEGINT:
      {
	u_int y = is->GetUInt(); NCHECK_EOB();
	is->Commit();

	if (y > MAX_VALID_INT) NCORRUPT();
	
	frame->Push(Store::IntToWord(-(y + 1)));
      }
      break;
    case NewPickle::CHUNK: 
      {
	u_int size    = is->GetUInt(); NCHECK_EOB();

	u_char *bytes = is->GetBytes(size); NCHECK_EOB();
	is->Commit();
	Chunk *y      = Store::AllocChunk(size);
	std::memcpy(y->GetBase(), bytes, size);
	frame->Push(y->ToWord());
      }
      break;
      
    case NewPickle::UNIQUE: 
      {
	is->Commit();
	word top = frame->Pop();
	Assert(Store::WordToChunk(top) != INVALID_POINTER);
	String *s = String::FromWordDirect(top);
	word wUnique = UniqueString::New(s)->ToWord();
	frame->Push(wUnique);
      }
      break;
    case NewPickle::BLOCK: 
      {
	u_int label  = is->GetUInt(); NCHECK_EOB();
	u_int size   = is->GetUInt(); NCHECK_EOB();
	is->Commit();

	word block =
	  StoreAbstraction::AllocBlock(static_cast<BlockLabel>(label),
				       size, frame);
	frame->Push(block);
      }
      break;
    case NewPickle::aBLOCK:
      {
	u_int label  = is->GetUInt(); NCHECK_EOB();
	u_int size   = is->GetUInt(); NCHECK_EOB();
	u_int addr   = is->GetUInt(); NCHECK_EOB();
	is->Commit();

	word block =
	  StoreAbstraction::AnnounceBlock(static_cast<BlockLabel>(label),
					  size);
	frame->PushStore(addr, block);
      }
      break;
    case NewPickle::FULFILL:
      {
	u_int addr   = is->GetUInt(); NCHECK_EOB();
	is->Commit();

	if (addr > MAX_VALID_INT) NCORRUPT();

	word future = frame->Load(addr);
	Block *block = StoreAbstraction::MakeFulfilledBlock(future, frame);
	frame->Push(block->ToWord());
      }
      break;
    case NewPickle::ENDOFSTREAM:
      {
	NewUnpickleArgs::SetResult(frame->Load(0));
	Scheduler::PopFrame();
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
      //      Scheduler::PushFrame(frame->ToWord());
      return Worker::PREEMPT;
    }
  }

  Scheduler::PopFrame();
  NCONTINUE();
}

const char *NewUnpickleWorker::Identify() {
  return "NewUnpickleWorker";
}

void NewUnpickleWorker::DumpFrame(word frameWord) {
  frameWord = frameWord;
  std::fprintf(stderr, "Unpickling\n");
}
