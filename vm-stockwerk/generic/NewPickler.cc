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

#include <sys/time.h>
#include <time.h>

// The OutputBuffer is used to buffer the byte code as well as
// keep track of local variables

class OutputBuffer : private Block {
  static const BlockLabel OBUF_LABEL = MIN_DATA_LABEL;
  enum { BUFFER_POS, LOCALS_POS, POS_POS, SIZE_POS, 
	 LOC_COUNT_POS, LOC_SIZE_POS, SIZE };
  static const u_int initialSize = 8192; //--** to be checked
  static const u_int locInitialSize = 64; //--** to be checked

  void ResizeBuffer();
  void ResizeLocals();

  // Quicksort implementation needed for
  // local variable tracking
  static void swap(Block *b, int i, int j);
  static int partition(Block *b, int l, int r);
  static void sort(Block *b, int l, int r);
  
public:
  using Block::ToWord;

  static OutputBuffer *New() {
    Block *b = Store::AllocBlock(OBUF_LABEL, SIZE);
    b->InitArg(BUFFER_POS, Store::AllocChunk(initialSize)->ToWord());

    Block *locBuf = Store::AllocBlock(MIN_DATA_LABEL, locInitialSize*2);
    locBuf->InitArg(0,Store::IntToWord(0));
    locBuf->InitArg(1,Store::IntToWord(0));

    b->InitArg(LOCALS_POS, locBuf->ToWord());
    b->InitArg(POS_POS, Store::IntToWord(0));
    b->InitArg(SIZE_POS, Store::IntToWord(initialSize));
    b->InitArg(LOC_COUNT_POS, Store::IntToWord(0));
    b->InitArg(LOC_SIZE_POS, Store::IntToWord(locInitialSize*2));
    return static_cast<OutputBuffer *>(b);
  }
  static OutputBuffer *FromWordDirect(word w) {
    Block *b = Store::DirectWordToBlock(w);
    Assert(b->GetLabel() == OBUF_LABEL);
    return static_cast<OutputBuffer *>(b);
  }

  u_int GetSize();

  void PutByte(u_char byte);
  void PutBytes(Chunk *c);
  void PutUInt(u_int i);
  u_int PutLocal(u_int ref);

  u_int GetPos();
  Chunk *GetBuffer();
  Block *GetLocals();
  u_int GetNoOfLocals();

};


void OutputBuffer::swap(Block *b, int i, int j) {
  word v = b->GetArg(i*2);
  word l = b->GetArg(i*2+1);
  b->ReplaceArg(i*2, b->GetArg(j*2));
  b->ReplaceArg(i*2+1, b->GetArg(j*2+1));
  b->ReplaceArg(j*2, v);
  b->ReplaceArg(j*2+1, l);
}
  
int OutputBuffer::partition(Block *b, int l, int r) {
  int i = l-1, j = r;
  
  int v = Store::DirectWordToInt(b->GetArg(r*2));
  
  for(;;) {
    while (Store::DirectWordToInt(b->GetArg((++i)*2)) < v);
    while (v < Store::DirectWordToInt(b->GetArg((--j)*2)))
      if (j==l) break;
    if (i >= j) break;
    swap(b, i, j);
  }
  swap(b, i, r);
  return i;
  
}

void OutputBuffer::sort(Block *b, int l, int r) {
  if (r <= l) return;
  int i = partition(b, l, r);
  sort(b, l, i-1);
  sort(b, i+1, r);
}

void OutputBuffer::ResizeBuffer() {
  // Adjusts the output buffer's size if it has become too small
  u_int size = Store::DirectWordToInt(GetArg(SIZE_POS));
  u_int newSize = (size * 3) >> 1; // Why that?

  Chunk *buf = Store::DirectWordToChunk(GetArg(BUFFER_POS));
  Chunk *newBuf = Store::AllocChunk(newSize);
  std::memcpy(newBuf->GetBase(), buf->GetBase(), GetPos());
  
  ReplaceArg(SIZE_POS, Store::IntToWord(newSize));
  ReplaceArg(BUFFER_POS, newBuf->ToWord());
}

void OutputBuffer::ResizeLocals() {
  // Adjusts the size of the local variable buffer if it has
  // become too small
  u_int size = Store::DirectWordToInt(GetArg(LOC_SIZE_POS));
  u_int newSize = (size * 3 * 2) >> 1;

  u_int pos  = Store::DirectWordToInt(GetArg(LOC_COUNT_POS));
  Block *buf = Store::DirectWordToBlock(GetArg(LOCALS_POS));
  Block *newBuf = Store::AllocBlock(MIN_DATA_LABEL, newSize);
  for (int i=pos+1; i--;) {
    newBuf->InitArg(i*2, buf->GetArg(i*2));
    newBuf->InitArg(i*2+1, buf->GetArg(i*2+1));
  }
  ReplaceArg(LOC_SIZE_POS, Store::IntToWord(newSize));
  ReplaceArg(LOCALS_POS, newBuf->ToWord());
}

u_int OutputBuffer::GetSize() {
  return Store::DirectWordToInt(GetArg(SIZE_POS));
}

Block *OutputBuffer::GetLocals() {
  // Returns the local variable vector, sorted
  // by the index into the output buffer
  
  Block *b = Store::DirectWordToBlock(GetArg(LOCALS_POS));
  u_int n = GetNoOfLocals();

  if (n>0) {
    sort(b, 0, GetNoOfLocals());
  }
  return b;
}

void OutputBuffer::PutByte(u_char byte) {
  u_int pos = Store::DirectWordToInt(GetArg(POS_POS));
  u_int size = Store::DirectWordToInt(GetArg(SIZE_POS));
  if (pos >= size) ResizeBuffer();
  Chunk *buf = Store::DirectWordToChunk(GetArg(BUFFER_POS));

  u_char *c = (u_char *)buf->GetBase();
  c[pos] = byte;
  ReplaceArg(POS_POS, Store::IntToWord(pos+1));
}

void OutputBuffer::PutBytes(Chunk *c) {
  u_int pos = Store::DirectWordToInt(GetArg(POS_POS));
  u_int size = c->GetSize();

  while (pos + size >= GetSize())
    ResizeBuffer();

  Chunk *buf = Store::DirectWordToChunk(GetArg(BUFFER_POS));
  std::memcpy(buf->GetBase() + pos, c->GetBase(), size);

  ReplaceArg(POS_POS, Store::IntToWord(pos+size));
}

void OutputBuffer::PutUInt(u_int i) {
  u_int pos = Store::DirectWordToInt(GetArg(POS_POS));
  u_int size = Store::DirectWordToInt(GetArg(SIZE_POS));

  // HACK: u_int of 32 bit can be 5 bytes at most!
  if (pos + 5 >= size) ResizeBuffer();

  Chunk *buf = Store::DirectWordToChunk(GetArg(BUFFER_POS));
  u_char *c = (u_char *)buf->GetBase();

  while (i >= 0x80) {
    c[pos] = (i & 0x7F | 0x80);
    pos++;
    i >>= 7;
  }
  c[pos] = i;
  ReplaceArg(POS_POS, Store::IntToWord(pos+1));

}

u_int OutputBuffer::PutLocal(u_int ref) {
  // creates a new local variable and returns its number
  // the reference into the output buffer is stored

  u_int lpos = Store::DirectWordToInt(GetArg(LOC_COUNT_POS));
  u_int lsize = Store::DirectWordToInt(GetArg(LOC_SIZE_POS));
  lpos++;
  if (lpos*2+1 >= lsize) ResizeLocals();

  Block *buf = Store::DirectWordToBlock(GetArg(LOCALS_POS));
  buf->ReplaceArg(lpos*2, Store::IntToWord(ref));
  buf->ReplaceArg(lpos*2+1, Store::IntToWord(lpos));
  PutUInt(lpos);
  ReplaceArg(LOC_COUNT_POS, Store::IntToWord(lpos));
  return lpos;
}

u_int OutputBuffer::GetPos() {
  return Store::DirectWordToInt(GetArg(POS_POS));
}

Chunk *OutputBuffer::GetBuffer() {
  return Store::DirectWordToChunk(GetArg(BUFFER_POS));
}

u_int OutputBuffer::GetNoOfLocals() {
  return Store::DirectWordToInt(GetArg(LOC_COUNT_POS));
}



// Sharing Detector
class NewSeen: private Block {
private:
  static const BlockLabel SEEN_LABEL = MIN_DATA_LABEL;
  enum { TABLE_POS, SIZE };
  static const u_int initialSize = 8; //--** to be checked
public:
  static const u_int NOT_WRITTEN = static_cast<u_int>(-1);
  static const u_int NOT_FOUND = static_cast<u_int>(-2);

  using Block::ToWord;

  static NewSeen *New() {
    Block *p = Store::AllocBlock(SEEN_LABEL, SIZE);
    p->InitArg(TABLE_POS, Map::New(initialSize)->ToWord());
    return static_cast<NewSeen *>(p);
  }
  static NewSeen *FromWordDirect(word w) {
    Block *b = Store::DirectWordToBlock(w);
    Assert(b->GetLabel() == SEEN_LABEL);
    return static_cast<NewSeen *>(b);
  }

  void Add(Block *v, int index) {
    Map *map     = Map::FromWordDirect(GetArg(TABLE_POS));
    Block *b = Store::AllocBlock(MIN_DATA_LABEL, 2);
    b->InitArg(0, Store::IntToWord(index));
    b->InitArg(1, Store::IntToWord(0));
    map->Put(v->ToWord(), b->ToWord());
  }

  void SetIndex(Block *v, int index) {
    Map *map     = Map::FromWordDirect(GetArg(TABLE_POS));
    Block *b = Store::DirectWordToBlock(map->Get(v->ToWord()));
    b->ReplaceArg(0, Store::IntToWord(index));
  }

  u_int Find(Block *v) {
    word vw  = v->ToWord();
    Map *map = Map::FromWordDirect(GetArg(TABLE_POS));
    if (map->IsMember(vw)) {
      Block *b = Store::DirectWordToBlock(map->Get(vw));
      return Store::DirectWordToInt(b->GetArg(0));
    } else {
      return NOT_FOUND;
    }
  }

  void SetVarNo(Block *v, int varNo) {
    Assert(Find(v) != NOT_FOUND);
    Map *map = Map::FromWordDirect(GetArg(TABLE_POS));
    word vw  = v->ToWord();
    Block *b = Store::DirectWordToBlock(map->Get(vw));
    b->ReplaceArg(1, Store::IntToWord(varNo));
  }
  int GetVarNo(Block *v) {
    Assert(Find(v) != NOT_FOUND);
    word vw  = v->ToWord();
    Map *map = Map::FromWordDirect(GetArg(TABLE_POS));
    Block *b = Store::DirectWordToBlock(map->Get(vw));
    return Store::DirectWordToInt(b->GetArg(1));
  }

};

// NewPickleArgs
class NewPickleArgs {
private:
  enum { STREAM_POS, BUFFER_POS, SEEN_POS,
	 CUR_STACK_POS, SIZE };
public:
  static void New(OutputStream *stream, OutputBuffer *buffer,
		  NewSeen *seen) {
    Scheduler::nArgs = SIZE;
    Scheduler::currentArgs[STREAM_POS] = stream->ToWord();
    Scheduler::currentArgs[BUFFER_POS] = buffer->ToWord();
    Scheduler::currentArgs[SEEN_POS] = seen->ToWord();
    Scheduler::currentArgs[CUR_STACK_POS] = Store::IntToWord(0);
  }
  static OutputStream *GetOutputStream() {
    Assert(Scheduler::nArgs == SIZE);
    return OutputStream::FromWordDirect(Scheduler::currentArgs[STREAM_POS]);
  }
  static OutputBuffer *GetOutputBuffer() {
    Assert(Scheduler::nArgs == SIZE);
    return OutputBuffer::FromWordDirect(Scheduler::currentArgs[BUFFER_POS]);
  }
  static NewSeen *GetSeen() {
    Assert(Scheduler::nArgs == SIZE);
    return NewSeen::FromWordDirect(Scheduler::currentArgs[SEEN_POS]);
  }
  static u_int GetCurStackHeight() {
    return Store::DirectWordToInt(Scheduler::currentArgs[CUR_STACK_POS]);
  }
  static void SetCurStackHeight(u_int i) {
    Scheduler::currentArgs[CUR_STACK_POS] = Store::IntToWord(i);
  }
};

class NewPickleStack : private Block {
private:
  enum { DATA_POS, BACK_POS, CHILD_NO_POS, MOTHER_POS,
	 SUM_POS, FRAME_SIZE };
  enum { ARRAY_POS, SIZE_POS, TOP_POS, SIZE };

  static const u_int initialSize = 128*FRAME_SIZE;

  void Resize();
  u_int GetTop();
  u_int GetSize();
public:
  using Block::ToWord;

  static NewPickleStack *New() {
    Block *b = Store::AllocBlock(MIN_DATA_LABEL, SIZE);
    b->InitArg(ARRAY_POS, Store::AllocBlock(MIN_DATA_LABEL, initialSize)->ToWord());
    b->InitArg(SIZE_POS, Store::IntToWord(initialSize));
    b->InitArg(TOP_POS, Store::IntToWord(0));
    return static_cast<NewPickleStack *>(b);
  }

  static NewPickleStack *FromWordDirect(word w) {
    Block *b = Store::DirectWordToBlock(w);
    Assert(b->GetLabel() == MIN_DATA_LABEL);
    return static_cast<NewPickleStack *>(b);
  }


  void Push(word data, u_int childNo, int mother);
  word GetData(u_int frame);
  void SetBack(u_int frame);
  bool GetBack(u_int frame);
  u_int GetChildNo(u_int frame);
  void SetSum(u_int frame, u_int sum);
  u_int GetSum(u_int frame);
  void AdjustMotherStack(u_int frame, u_int myStack);
  int GetTopFrame();
  void PopTopFrame();
};

void NewPickleStack::Resize() {
  // Adjusts the output buffer's size if it has become too small
  u_int size = Store::DirectWordToInt(GetArg(SIZE_POS));
  u_int newSize = (size * 3) >> 1;

  Block *buf = Store::DirectWordToBlock(GetArg(ARRAY_POS));
  Block *newBuf = Store::AllocBlock(MIN_DATA_LABEL, newSize);
  for (int i=size; i--;) {
    newBuf->InitArg(i, buf->GetArg(i));
  }
  ReplaceArg(SIZE_POS, Store::IntToWord(newSize));
  ReplaceArg(ARRAY_POS, newBuf->ToWord());
}

u_int NewPickleStack::GetTop() {
  return Store::DirectWordToInt(GetArg(TOP_POS));
}

u_int NewPickleStack::GetSize() {
  return Store::DirectWordToInt(GetArg(SIZE_POS));
}

void NewPickleStack::Push(word data, u_int childNo, int mother) {
  u_int top = GetTop();
  u_int size = GetSize();
  if (top+FRAME_SIZE >= size)
    Resize();
  
  Block *b = Store::DirectWordToBlock(GetArg(ARRAY_POS));
  b->ReplaceArg(top+DATA_POS, data);
  b->ReplaceArg(top+BACK_POS, Store::IntToWord(0));
  b->ReplaceArg(top+CHILD_NO_POS, Store::IntToWord(childNo));
  b->ReplaceArg(top+MOTHER_POS, Store::IntToWord(mother));
  b->ReplaceArg(top+SUM_POS, Store::IntToWord(0));

  top += FRAME_SIZE;
  ReplaceArg(TOP_POS, Store::IntToWord(top));
}

word NewPickleStack::GetData(u_int frame) {
  Block *b = Store::DirectWordToBlock(GetArg(ARRAY_POS));
  return b->GetArg(frame*FRAME_SIZE+DATA_POS);
}

void NewPickleStack::SetBack(u_int frame) {
  Block *b = Store::DirectWordToBlock(GetArg(ARRAY_POS));
  b->ReplaceArg(frame*FRAME_SIZE+BACK_POS, Store::IntToWord(1));
}

bool NewPickleStack::GetBack(u_int frame) {
  Block *b = Store::DirectWordToBlock(GetArg(ARRAY_POS));
  return (Store::DirectWordToInt(b->GetArg(frame*FRAME_SIZE+BACK_POS)) == 1);
}

u_int NewPickleStack::GetChildNo(u_int frame) {
  Block *b = Store::DirectWordToBlock(GetArg(ARRAY_POS));
  return (Store::DirectWordToInt(b->GetArg(frame*FRAME_SIZE+CHILD_NO_POS)));
}

void NewPickleStack::SetSum(u_int frame, u_int sum) {
  Block *b = Store::DirectWordToBlock(GetArg(ARRAY_POS));
  b->ReplaceArg(frame*FRAME_SIZE+SUM_POS, Store::IntToWord(sum));
}

u_int NewPickleStack::GetSum(u_int frame) {
  Block *b = Store::DirectWordToBlock(GetArg(ARRAY_POS));
  return (Store::DirectWordToInt(b->GetArg(frame*FRAME_SIZE+SUM_POS)));
}

void NewPickleStack::AdjustMotherStack(u_int frame, u_int myStack) {
  Block *b = Store::DirectWordToBlock(GetArg(ARRAY_POS));
  int m = Store::DirectWordToInt(b->GetArg(frame*FRAME_SIZE+MOTHER_POS));

  if (m == -1) {
    u_int sh = NewPickleArgs::GetCurStackHeight();
    if (myStack > sh)
      NewPickleArgs::SetCurStackHeight(myStack);
  } else {
    u_int sh = GetSum(m);
    if (myStack > sh)
      SetSum(m, myStack);
  }
}

int NewPickleStack::GetTopFrame() {
  u_int top = GetTop();
  if (top==0) {
    return -1;
  } else {
    return (top-FRAME_SIZE)/FRAME_SIZE;
  }
}

void NewPickleStack::PopTopFrame() {
  u_int top = GetTop();
  //  Assert(top > 0);
  top -= FRAME_SIZE;
  ReplaceArg(TOP_POS, Store::IntToWord(top));
}

// NewPickleWorker Frame
class NewPickleFrame: private StackFrame {
private:
  enum { STACK_POS, SIZE };
public:
  using Block::ToWord;

  // NewPickleFrame Constructor
  static NewPickleFrame *New(Worker *worker,
			     NewPickleStack *pstack) {
    StackFrame *frame = StackFrame::New(PICKLING_FRAME, worker, SIZE);
    frame->InitArg(STACK_POS, pstack->ToWord());
    return static_cast<NewPickleFrame *>(frame);
  }
  // NewPickleFrame Untagging
  static NewPickleFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == PICKLING_FRAME);
    return static_cast<NewPickleFrame *>(p);
  }

  // NewPickleFrame Accessors
  NewPickleStack *GetStack() {
    return NewPickleStack::FromWordDirect(StackFrame::GetArg(STACK_POS));
  }
};


// NewPickleWorker
class NewPickleWorker: public Worker {
private:
  static NewPickleWorker *self;
  // NewPickleWorker Constructor
  NewPickleWorker(): Worker() {}
public:
  // NewPickleWorker Static Constructor
  static void Init() {
    self = new NewPickleWorker();
  }
  // Frame Handling
  static void PushFrame(word data);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};


//
// NewPickleWorker Functions
//
NewPickleWorker *NewPickleWorker::self;

void NewPickleWorker::PushFrame(word data) {
  NewPickleStack *nps = NewPickleStack::New();
  nps->Push(data, 0, -1);

  Scheduler::PushFrame(NewPickleFrame::New(self, nps)->ToWord());
}

#define NCONTINUE() {				\
  if (StatusWord::GetStatus() != 0)		\
    return Worker::PREEMPT;			\
  else						\
    return Worker::CONTINUE;			\
}

Worker::Result NewPickleWorker::Run() {
  NewPickleFrame *frame =
    NewPickleFrame::FromWordDirect(Scheduler::GetFrame());

  NewPickleStack *nps = frame->GetStack();
  OutputBuffer *outputBuffer = NewPickleArgs::GetOutputBuffer();
  NewSeen *seen = NewPickleArgs::GetSeen();
    
  for(;;) {
    if (StatusWord::GetStatus() != 0) {
      //      Scheduler::PushFrame(frame->ToWord());
      return Worker::PREEMPT;
    }
  

    int fr = nps->GetTopFrame();

    if (fr < 0) {
      Scheduler::PopFrame();
      NCONTINUE();
    }
  
    word x0 = nps->GetData(fr);
    bool back = nps->GetBack(fr);


    if (back) { // We're back from recursion
      // Stack height computation
      u_int cNo = nps->GetChildNo(fr);
      u_int myStack = cNo + nps->GetSum(fr);
      nps->AdjustMotherStack(fr, myStack);

      Block *v = Store::WordToBlock(x0);
      u_int var = seen->GetVarNo(v);

      if (var!=0) { // This is a cycle
	// The block has already been announced, so this
	// announcement only has to be fulfilled
	outputBuffer->PutByte(NewPickle::FULFILL);
	outputBuffer->PutUInt(var);
	nps->PopTopFrame();
	continue;
      }

      BlockLabel l = v->GetLabel();
      u_int size = v->GetSize();

      switch(l) {
      case UNIQUESTRING_LABEL:
	{
	  outputBuffer->PutByte(NewPickle::UNIQUE);
	  seen->SetIndex(v, outputBuffer->GetPos());
	}
	break;
      case CONCRETE_LABEL:
	{
	  outputBuffer->PutByte(NewPickle::BLOCK);
	  outputBuffer->PutUInt((u_int) TRANSFORM_LABEL);
	  outputBuffer->PutUInt(2);
	  seen->SetIndex(v, outputBuffer->GetPos());
	}
	break;
      default:
	{
	  outputBuffer->PutByte(NewPickle::BLOCK);
	  outputBuffer->PutUInt((u_int) l);
	  outputBuffer->PutUInt(size);
	  seen->SetIndex(v, outputBuffer->GetPos());
	}
	break;
      }

      nps->PopTopFrame();
      continue;
    }

    if (Store::WordToTransient(x0) != INVALID_POINTER) {
      Scheduler::currentData = x0;
      //      Scheduler::PushFrameNoCheck(frame->ToWord());
      return Worker::REQUEST;
    }


    // Check for integer
    s_int i;
    if ((i = Store::WordToInt(x0)) != INVALID_INT) {
      if (i >= 0) {
	outputBuffer->PutByte(NewPickle::POSINT);
	outputBuffer->PutUInt(i);
      } else {
	outputBuffer->PutByte(NewPickle::NEGINT);
	outputBuffer->PutUInt(-(i + 1));
      }
      // integers occupy 1 stack cell
      u_int cNo = nps->GetChildNo(fr);
      nps->AdjustMotherStack(fr, cNo + 1);
      nps->PopTopFrame();
      continue;
    }

    // Search for already known value

    Block *v   = Store::WordToBlock(x0);
    u_int ref  = seen->Find(v);

    if (ref != NewSeen::NOT_FOUND) {
      if (ref==NewSeen::NOT_WRITTEN) {
	u_int vn = seen->GetVarNo(v);
	if (vn>0) {
	  // this node has already been announced -> just load it
	  outputBuffer->PutByte(NewPickle::LOAD);
	  outputBuffer->PutUInt(vn);
	} else {
	  // this is a cycle, we have to announce
	  switch(v->GetLabel()) {
	  case CONCRETE_LABEL:
	    {
	      outputBuffer->PutByte(NewPickle::aBLOCK);
	      outputBuffer->PutUInt((u_int) TRANSFORM_LABEL);
	      outputBuffer->PutUInt(2);
	      u_int ann = outputBuffer->PutLocal(0); // reference doesn't matter
	      // but ref=0 keeps SaveWorker from placing a STORE instruction
	      // which is not necessary for announces
	      seen->SetVarNo(v, ann);
	    }
	    break;
	  default:
	    {
	      outputBuffer->PutByte(NewPickle::aBLOCK);
	      outputBuffer->PutUInt((u_int) v->GetLabel());
	      outputBuffer->PutUInt(v->GetSize());	
	      u_int ann = outputBuffer->PutLocal(0); // reference doesn't matter
	      // same as for CONCRETE_LABEL
	      seen->SetVarNo(v, ann);
	    }
	    break;
	  }
	}
      } else {
	// this is sharing, we can load the reference
	outputBuffer->PutByte(NewPickle::LOAD);
	u_int vn = seen->GetVarNo(v);
	if (vn>0) {
	  // the variable is already known, just load it
	  outputBuffer->PutUInt(vn);
	} else {
	  // a new local variable for v has to be created
	  u_int ann = outputBuffer->PutLocal(ref);
	
	  // We have to remember its number so that we can
	  // simply load it next time
	  seen->SetVarNo(v, ann);
	}
      }
      u_int cNo = nps->GetChildNo(fr);
      nps->AdjustMotherStack(fr, cNo + 1);
      nps->PopTopFrame();
      continue;
    }

    // Handle new Block Value (non-abstract use)
    BlockLabel l = v->GetLabel();
    switch (l) {
    case WEAK_MAP_LABEL:
    case MAP_LABEL:
    case INT_MAP_LABEL:
    case CHUNK_MAP_LABEL:
    case THREAD_LABEL:
    case TASKSTACK_LABEL:
    case IODESC_LABEL:
      Scheduler::currentData      = Pickler::Sited;
      Scheduler::currentBacktrace = Backtrace::New(frame->ToWord());
      return Worker::RAISE;
    case CHUNK_LABEL:
      {
	Chunk *c = static_cast<Chunk *>(v);
	outputBuffer->PutByte(NewPickle::CHUNK);
	outputBuffer->PutUInt(c->GetSize());
	outputBuffer->PutBytes(c);
	u_int idx = outputBuffer->GetPos();

	seen->Add(v, idx);
	u_int cNo = nps->GetChildNo(fr);
	nps->AdjustMotherStack(fr, cNo + 1);
	nps->PopTopFrame();
	continue;
      }
      break;
    case UNIQUESTRING_LABEL:
      {
	nps->SetBack(fr);

	seen->Add(v, NewSeen::NOT_WRITTEN);
	UniqueString *s = static_cast<UniqueString *>(v);

	nps->Push(s->ToString()->ToWord(), 0, fr);
	continue;
      }
      break;
    case CONCRETE_LABEL:
      {
	nps->SetBack(fr);

	seen->Add(v, NewSeen::NOT_WRITTEN);
	ConcreteRepresentation *concrete =
	  static_cast<ConcreteRepresentation *>(v);
	Transform *abstract =
	  concrete->GetHandler()->GetAbstractRepresentation(concrete);
	Block *ablock = static_cast<Block *>(abstract);
	if (abstract == INVALID_POINTER) {
	  Scheduler::currentData      = Pickler::Sited;
	  Scheduler::currentBacktrace = Backtrace::New(frame->ToWord());
	  return Worker::RAISE;
	} else {
	  seen->Add(ablock, NewSeen::NOT_WRITTEN);

	  nps->Push(abstract->GetArgument(), 1, fr);
	  nps->Push(abstract->GetName()->ToWord(), 0, fr);
	  continue;
	}
      }
      break;
    case TRANSFORM_LABEL:
      {
	// must not occur anywhere but under a CONCRETE which is handled above
	Scheduler::currentData      = Pickler::Sited;
	Scheduler::currentBacktrace = Backtrace::New(frame->ToWord());
	return Worker::RAISE;
      
      }
      break;
    default:
      {
	nps->SetBack(fr);

	seen->Add(v, NewSeen::NOT_WRITTEN);
	u_int size = v->GetSize(); // to be done
	for (u_int i = 0; i<size; i++ )
	  nps->Push(v->GetArg(i), size-i-1, fr);
	continue;
      }
    }
  }
}

const char *NewPickleWorker::Identify() {
  return "NewPickleWorker";
}

void NewPickleWorker::DumpFrame(word) {
  std::fprintf(stderr, "Pickle Task\n");
}

// NewPickleSaveWorker Frame
class NewPickleSaveFrame: private StackFrame {
private:
  enum { SIZE };
public:
  using Block::ToWord;

  // NewPickleSaveFrame Constructor
  static NewPickleSaveFrame *New(Worker *worker) {
    StackFrame *frame = StackFrame::New(PICKLE_SAVE_FRAME, worker, SIZE);
    return static_cast<NewPickleSaveFrame *>(frame);
  }
  // NewPickleSaveFrame Untagging
  static NewPickleSaveFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == PICKLE_SAVE_FRAME);
    return static_cast<NewPickleSaveFrame *>(p);
  }
};

// NewPickleSaveWorker
class NewPickleSaveWorker: public Worker {
private:
  static NewPickleSaveWorker *self;
  // NewPickleSaveWorker Constructor
  NewPickleSaveWorker(): Worker() {}
public:
  // NewPickleSaveWorker Static Constructor

  static void WriteToStream(OutputBuffer *obf,
			    OutputStream *outputStream);

  static void Init() {
    self = new NewPickleSaveWorker();
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
// NewPickleSaveWorker Functions
//
NewPickleSaveWorker *NewPickleSaveWorker::self;

void NewPickleSaveWorker::PushFrame() {
  Scheduler::PushFrame(NewPickleSaveFrame::New(self)->ToWord());
}

void NewPickleSaveWorker::WriteToStream(OutputBuffer *obf,
					 OutputStream *outputStream) {

  Chunk *c = obf->GetBuffer();
  Block *vars = obf->GetLocals();
  u_int pos = obf->GetPos();
  u_int noOfLoc = obf->GetNoOfLocals();
  u_int stackHeight = NewPickleArgs::GetCurStackHeight();

  // Write the magic header = stack info
  outputStream->PutByte(NewPickle::INIT);
  outputStream->PutUInt(stackHeight);
  outputStream->PutUInt(noOfLoc+1);

  u_int curPos = 0;
  u_char *ibf = (u_char *)c->GetBase();

  // write the output buffer to file, filling in the missing
  // STOREs where indicated by the locals array
  for(u_int i=1;i<=noOfLoc;i++) {
    u_int nextLoc = Store::DirectWordToInt(vars->GetArg(i*2));
    u_int nextVar = Store::DirectWordToInt(vars->GetArg(i*2+1));

    if (nextLoc>0) {
      Assert ( nextLoc > curPos );
      outputStream->PutBytes(ibf+curPos, nextLoc-curPos);
      curPos = nextLoc;
      outputStream->PutByte(NewPickle::STORE);
      outputStream->PutUInt(nextVar);
    }
  }

  outputStream->PutBytes(ibf+curPos, pos-curPos);

  // store topmost node to variable 0
  // this becomes the root node on unpickling
  outputStream->PutByte(NewPickle::STORE);
  outputStream->PutUInt(0);
  outputStream->PutByte(NewPickle::ENDOFSTREAM);
}

Worker::Result NewPickleSaveWorker::Run() {
  Scheduler::PopFrame();

  OutputBuffer *obf = NewPickleArgs::GetOutputBuffer();

  OutputStream *os = NewPickleArgs::GetOutputStream();
  NewPickleSaveWorker::WriteToStream(obf, os);

  FileOutputStream *outputStream =
    static_cast<FileOutputStream *>(os);
  
  outputStream->Close();
  Scheduler::nArgs = 0;

  return Worker::CONTINUE;
}

const char *NewPickleSaveWorker::Identify() {
  return "NewPickleSaveWorker";
}

void NewPickleSaveWorker::DumpFrame(word) {
  std::fprintf(stderr, "Pickle Save\n");
}

// NewPicklePackWorker Frame
class NewPicklePackFrame: private StackFrame {
private:
  enum { SIZE };
public:
  using Block::ToWord;

  // NewPickleSaveFrame Constructor
  static NewPicklePackFrame *New(Worker *worker) {
    StackFrame *frame = StackFrame::New(PICKLE_PACK_FRAME, worker, SIZE);
    return static_cast<NewPicklePackFrame *>(frame);
  }
  // NewPickleSaveFrame Untagging
  static NewPicklePackFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == PICKLE_PACK_FRAME);
    return static_cast<NewPicklePackFrame *>(p);
  }
};

// NewPicklePackWorker
class NewPicklePackWorker: public Worker {
private:
  static NewPicklePackWorker *self;
  // NewPicklePackWorker Constructor
  NewPicklePackWorker(): Worker() {}
public:
  // NewPicklePackWorker Static Constructor
  static void Init() {
    self = new NewPicklePackWorker();
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
// NewPicklePackWorker Functions
//
NewPicklePackWorker *NewPicklePackWorker::self;

void NewPicklePackWorker::PushFrame() {
  Scheduler::PushFrame(NewPicklePackFrame::New(self)->ToWord());
}

Worker::Result NewPicklePackWorker::Run() {
  Scheduler::PopFrame();

  OutputBuffer *obf = NewPickleArgs::GetOutputBuffer();

  OutputStream *os = NewPickleArgs::GetOutputStream();
  NewPickleSaveWorker::WriteToStream(obf, os);

  StringOutputStream *outputStream =
    static_cast<StringOutputStream *>(os);
  
  Scheduler::nArgs = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = outputStream->Close();

  return Worker::CONTINUE;
}

const char *NewPicklePackWorker::Identify() {
  return "NewPicklePackWorker";
}

void NewPicklePackWorker::DumpFrame(word) {
  std::fprintf(stderr, "Pickle Pack\n");
}

