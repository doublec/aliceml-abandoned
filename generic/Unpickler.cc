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
#include <limits.h>
#include "generic/ZLib.hh"
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
#include "generic/Pickler.hh"
#include "generic/UniqueString.hh"
#include "generic/Broker.hh"


namespace {

  word handlerTable;
  const u_int initialHandlerTableSize = 7;

  //
  // InputStream Class
  //
  const u_int READ_BUFFER_SIZE = 8192; // to be checked
  const u_int READ_BUFFER_OVERSHOOT = 20; // to be checked

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
	static_cast<InputStream *>(Store::AllocMutableBlock(static_cast<BlockLabel>(type), size));
      is->InitArg(HD_POS, static_cast<s_int>(0));
      is->InitArg(RD_POS, static_cast<s_int>(0));
      is->InitArg(EOB_POS, static_cast<s_int>(false));
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
	  Store::AllocMutableChunk(READ_BUFFER_SIZE + READ_BUFFER_OVERSHOOT);
	is->InitArg(BUFFER_POS, buffer->ToWord());
      }
      is->InitArg(TL_POS, static_cast<s_int>(0));
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
      Assert(p->GetLabel() == static_cast<BlockLabel>(FILE_INPUT_STREAM) ||
	    p->GetLabel() == static_cast<BlockLabel>(STRING_INPUT_STREAM));
      return static_cast<InputStream *>(p);
    }

    // InputStream Methods
    gzFile GetFile() {
      Assert(GetLabel() == static_cast<BlockLabel>(FILE_INPUT_STREAM));
      return static_cast<gzFile>(Store::DirectWordToUnmanagedPointer(GetArg(FILE_POS)));
    }
    bool IsEOB() {
      return Store::DirectWordToInt(Block::GetArg(EOB_POS));
    }
    bool IsEOF() {
      switch (static_cast<IN_STREAM_TYPE>(GetLabel())) {
      case FILE_INPUT_STREAM:
	if (GetRd() >= GetTl() &&
	    gzgetc(GetFile()) < 0) {
	  gzseek(GetFile(), -1L, SEEK_CUR);
	  return true;
	} else {
	  return false;
	}
      case STRING_INPUT_STREAM:
	return (GetRd() >= GetTl());
      default:
	Error("InputStream::IsEOF: illegal node type");
      }
    }
    u_char GetByte() {
      u_int rd = GetRd();
      if (rd >= GetTl()) {
	SetEOB(true);
	return static_cast<u_char>(0);
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
	if (static_cast<u_int>(c) >= static_cast<u_int>(1 << freeBits)) {
	  Error("Unpickler: integer out of range"); //--** raise exception
	}
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
      switch (static_cast<IN_STREAM_TYPE>(GetLabel())) {
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
      switch (static_cast<IN_STREAM_TYPE>(GetLabel())) {
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
      switch (static_cast<IN_STREAM_TYPE>(GetLabel())) {
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
      if (size > UINT_MAX)
	Error("InputStream::FillBuffer");
	  int nread = gzread(file, bytes, static_cast<unsigned int>(size));
	  if (nread < 0) {
	    Scheduler::SetCurrentData(Pickler::IOError);
	    StackFrame *frame = Scheduler::GetFrame();
	    Scheduler::SetCurrentBacktrace(Backtrace::New(frame->Clone()));
	    Scheduler::PopFrame();
	    return Worker::RAISE;
	  } else if (nread == 0) { // at end of file: raise Corrupt exception
	    Scheduler::SetCurrentData(Unpickler::Corrupt);
	    StackFrame *frame = Scheduler::GetFrame();
	    Scheduler::SetCurrentBacktrace(Backtrace::New(frame->Clone()));
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
	  Scheduler::SetCurrentData(Unpickler::Corrupt);
	  StackFrame *frame = Scheduler::GetFrame();
	  Scheduler::SetCurrentBacktrace(Backtrace::New(frame->Clone()));
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
      Scheduler::SetNArgs(SIZE);
      Scheduler::SetCurrentArg(STREAM_POS, is->ToWord());
      Scheduler::SetCurrentArg(RESULT_POS, Store::IntToWord(0));
    }
    static InputStream *GetInputStream() {
      Assert(Scheduler::GetNArgs() == SIZE);
      return InputStream::FromWordDirect(Scheduler::GetCurrentArg(STREAM_POS));
    }
    static void SetResult(word result) {
      Assert(Scheduler::GetNArgs() == SIZE);
      Scheduler::SetCurrentArg(RESULT_POS, result);
    }
    static word GetResult() {
      return Scheduler::GetCurrentArg(RESULT_POS);
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
    virtual void DumpFrame(StackFrame *sFrame, std::ostream& out);
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

  void InputWorker::DumpFrame(StackFrame *, std::ostream& out) {
    out << "[Unpickler::Input]" << std::endl;
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
      return static_cast<TransformFrame *>(frame);
    }
    u_int GetSize() {
      return StackFrame::GetSize() + SIZE;
    }
    Future *GetFuture() {
      return
	static_cast<Future *>(Store::DirectWordToTransient(GetArg(FUTURE_POS)));
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
    virtual void DumpFrame(StackFrame *sFrame, std::ostream& out);
  };

  TransformWorker *TransformWorker::self;

  void TransformWorker::PushFrame(Future *future,
				  String *name,
				  word arg) {
    TransformFrame::New(self, future, name, arg);
  }

  u_int TransformWorker::GetFrameSize(StackFrame *sFrame) {
    TransformFrame *frame = reinterpret_cast<TransformFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);
    return frame->GetSize();
  }

  const char *TransformWorker::Identify() {
    return "TransformWorker";
  }

  void TransformWorker::DumpFrame(StackFrame *, std::ostream& out) {
    out << "[Unpickler::Transform]" << std::endl;
  }

  Worker::Result TransformWorker::Run(StackFrame *sFrame) {
    TransformFrame *frame = reinterpret_cast<TransformFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);

    Future *f = frame->GetFuture();
    String *name = frame->GetName();
    word argument = frame->GetArgument();

    ChunkMap *map = ChunkMap::FromWordDirect(handlerTable);
    word wHandler = map->CondGet(name->ToWord());
    if (wHandler != INVALID_POINTER) {
      Unpickler::handler handler = reinterpret_cast<Unpickler::handler>
	(Store::DirectWordToUnmanagedPointer(wHandler));
      f->Become(REF_LABEL, handler(argument));
    } else {
      Scheduler::SetCurrentData(Unpickler::Corrupt);
      Scheduler::SetCurrentBacktrace(
	Backtrace::New(reinterpret_cast<StackFrame *>(frame)->Clone()));
      Scheduler::PopFrame();
      return Worker::RAISE;
    }

    Scheduler::PopFrame(frame->GetSize());
    if (StatusWord::GetStatus() != 0)
      return Worker::PREEMPT;
    else
      return Worker::CONTINUE;  
  }

  // ApplyTransform Function
  bool ApplyTransform(String *name, word argument, word *newBlock,
		      Future *f) {
    Assert(name != INVALID_POINTER);
    
    ChunkMap *map = ChunkMap::FromWordDirect(handlerTable);
    word wHandler = map->CondGet(name->ToWord());

    if (wHandler != INVALID_POINTER) {
      Unpickler::handler handler = reinterpret_cast<Unpickler::handler>
	(Store::DirectWordToUnmanagedPointer(wHandler));
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
    virtual void DumpFrame(StackFrame *sFrame, std::ostream& out);
  };

  //
  // PickleUnpackWorker Functions
  //
  PickleUnpackWorker *PickleUnpackWorker::self;

  void PickleUnpackWorker::PushFrame() {
    NEW_STACK_FRAME(frame, self, 0);
  }

  u_int PickleUnpackWorker::GetFrameSize(StackFrame *sFrame) {
    Assert(sFrame->GetWorker() == this);
    return sFrame->GetSize();
  }

  Worker::Result PickleUnpackWorker::Run(StackFrame *sFrame) {
    Assert(sFrame->GetWorker() == this);
    Scheduler::PopFrame(sFrame->GetSize());
    word result = UnpickleArgs::GetResult();

    Scheduler::SetNArgs(1);
    Scheduler::SetCurrentArg(0, result);
    return Worker::CONTINUE;
  }

  const char *PickleUnpackWorker::Identify() {
    return "PickleUnpackWorker";
  }

  void PickleUnpackWorker::DumpFrame(StackFrame *, std::ostream& out) {
    out << "[Unpickler::Unpack]" << std::endl;
  }

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
    virtual void DumpFrame(StackFrame *sFrame, std::ostream& out);
  };

  //
  // PickleLoadWorker Functions
  //
  PickleLoadWorker *PickleLoadWorker::self;

  void PickleLoadWorker::PushFrame() {
    NEW_STACK_FRAME(frame, self, 0);
  }

  u_int PickleLoadWorker::GetFrameSize(StackFrame *sFrame) {
    Assert(sFrame->GetWorker() == this);
    return sFrame->GetSize();
  }

  Worker::Result PickleLoadWorker::Run(StackFrame *sFrame) {
    Assert(sFrame->GetWorker() == this);
    Scheduler::PopFrame(sFrame->GetSize());
    InputStream *is = UnpickleArgs::GetInputStream();
    is->Close();
    
    word result = UnpickleArgs::GetResult();

    Scheduler::SetNArgs(1);
    Scheduler::SetCurrentArg(0, result);
    return Worker::CONTINUE;
  }

  const char *PickleLoadWorker::Identify() {
    return "PickleLoadWorker";
  }

  void PickleLoadWorker::DumpFrame(StackFrame *, std::ostream& out) {
    out << "[Unpickler::Load]" << std::endl;
  }


  // UnpickleWorker Frame
  class UnpickleFrame: private StackFrame {
  private:
    enum { SIZE_POS, TOP_POS, 
  #ifdef DEBUG_CHECK
	  LOCALS_POS,
  #endif
	  SIZE };
  public:
    // UnpickleFrame Constructor
    static UnpickleFrame *New(Worker *worker, u_int stackSize, u_int locals) {
      u_int frSize = SIZE+stackSize+locals;
      NEW_STACK_FRAME(frame, worker, frSize);
      frame->InitArg(SIZE_POS, frame->GetSize() + frSize);
      frame->InitArg(TOP_POS, SIZE+locals);
  #ifdef DEBUG_CHECK
      frame->InitArg(LOCALS_POS, locals);
  #endif
      frame->InitArgs(SIZE, stackSize + locals, Store::IntToWord(0));
      return static_cast<UnpickleFrame *>(frame);
    }

    u_int GetSize() {
      return Store::DirectWordToInt(StackFrame::GetArg(SIZE_POS));
    }

    void Push(word value) {
      u_int tp = Store::DirectWordToInt(GetArg(TOP_POS));
      Assert(tp>=Store::DirectWordToInt(GetArg(LOCALS_POS)));
      Assert(tp<GetSize());
      ReplaceArg(tp, value);
      tp++;
      ReplaceArg(TOP_POS, Store::IntToWord(tp));
    }
    word Pop() {
      u_int tp = Store::DirectWordToInt(GetArg(TOP_POS));
      Assert(tp > static_cast<u_int>(Store::DirectWordToInt(GetArg(LOCALS_POS))));
      tp--;
      word result = GetArg(tp);
      ReplaceArg(TOP_POS, Store::IntToWord(tp));
      return result;
    }
    word Top() {
      u_int tp = Store::DirectWordToInt(GetArg(TOP_POS));
      Assert(tp > static_cast<u_int>(Store::DirectWordToInt(GetArg(LOCALS_POS))));
      return GetArg(tp-1);
    }
    void Store(u_int i, word value) {
      Assert(i < static_cast<u_int>(Store::DirectWordToInt(GetArg(LOCALS_POS))));
      Assert( Store::WordToInt(GetArg(SIZE+i)) == 0);
      ReplaceArg(SIZE+i, value);
    }
    word Load(u_int i) {
      Assert(i < static_cast<u_int>(Store::DirectWordToInt(GetArg(LOCALS_POS))));
      Assert(i == 0 || Store::WordToInt(GetArg(SIZE+i)) == INVALID_INT);
      return GetArg(SIZE+i);
    }
    void PushStore(u_int i, word value) {
      Store(i, value);
      Push(value);
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
    static void PushFrame(u_int, u_int);
    virtual u_int GetFrameSize(StackFrame *sFrame);
    // Execution
    virtual Result Run(StackFrame *sFrame);
    // Debugging
    virtual const char *Identify();
    virtual void DumpFrame(StackFrame *sFrame, std::ostream& out);
  };

  // The StoreAbstraction makes the business of transforming CONCRETE and 
  // TRANSFORM nodes invisible to the unpickler

  class StoreAbstraction {
  public:
    static bool AllocBlock(BlockLabel label,
			  u_int size,
			  UnpickleFrame *frame,
			  word *newBlock,
			  bool allocMutable = false) {
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
	  Block *b = allocMutable ?
	    Store::AllocMutableBlock(label, size) :
	    Store::AllocBlock(label, size);
	  for (u_int i=0; i<size; i++) {
	    b->InitArg(i, frame->Pop());
	  }
	  *newBlock = b->ToWord();
	  return false;
	}      
	break;
      }
    }

    static word AnnounceBlock(BlockLabel label, u_int size,
			      bool allocMutable=false) {
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
	  if (allocMutable)
	    return Store::AllocMutableBlock(label, size)->ToWord();
	  else
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
	  b->ReplaceArgUnchecked(i, frame->Pop());
	}
	*newBlock = b->ToWord();
	return false;
      } else {
	Future *f = static_cast<Future *>(Store::DirectWordToTransient(future));

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

  void UnpickleWorker::PushFrame(u_int stackSize, u_int localsSize) {
    UnpickleFrame::New(self, stackSize, localsSize);
  }

  // Unpickle Helpers

// End of Buffer requires rereading;
// therefore we reinstall the old task stack
//--** to be done: more efficient solution
#define NCHECK_EOB() {				\
  if (is->IsEOB()) {				\
    InputWorker::PushFrame();			\
    return Worker::CONTINUE;			\
  }						\
}

#define NCONTINUE() {				\
  if (StatusWord::GetStatus() != 0)		\
    return Worker::PREEMPT;			\
  else						\
    return Worker::CONTINUE;			\
}

#define NCORRUPT() {							\
  Scheduler::SetCurrentData(Unpickler::Corrupt);			\
  StackFrame *frame = Scheduler::GetFrame();				\
  Scheduler::SetCurrentBacktrace(Backtrace::New(frame->Clone()));	\
  Scheduler::PopFrame();						\
  return Worker::RAISE;							\
}


  u_int UnpickleWorker::GetFrameSize(StackFrame *sFrame) {
    UnpickleFrame *frame = reinterpret_cast<UnpickleFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);
    return frame->GetSize();
  }

  // Core Unpickling Function
  Worker::Result UnpickleWorker::Run(StackFrame *sFrame) {
    UnpickleFrame *frame = reinterpret_cast<UnpickleFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);

    InputStream *is = UnpickleArgs::GetInputStream();

    //  NUStack *st = UnpickleArgs::GetStack();

    for(;;) {
      word newBlock;
      bool mustContinue = false;
      u_char tag = is->GetByte(); NCHECK_EOB();
      switch (static_cast<Pickle::Tag>(tag)) {
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
	  frame->Push(frame->Load(addr));
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
      case Pickle::MCHUNK: 
	{
	  u_int size    = is->GetUInt(); NCHECK_EOB();

	  u_char *bytes = is->GetBytes(size); NCHECK_EOB();
	  is->Commit();
	  Chunk *y      = Store::AllocMutableChunk(size);
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
      case Pickle::MBLOCK: 
	{
	  u_int label  = is->GetUInt(); NCHECK_EOB();
	  u_int size   = is->GetUInt(); NCHECK_EOB();
	  is->Commit();

	  mustContinue =
	    StoreAbstraction::AllocBlock(static_cast<BlockLabel>(label),
					size, frame, &newBlock, true);
	  frame->Push(newBlock);
	}
	break;
      case Pickle::BLOCK: 
	{
	  u_int label  = is->GetUInt(); NCHECK_EOB();
	  u_int size   = is->GetUInt(); NCHECK_EOB();
	  is->Commit();

	  mustContinue =
	    StoreAbstraction::AllocBlock(static_cast<BlockLabel>(label),
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
	    StoreAbstraction::AnnounceBlock(static_cast<BlockLabel>(label),
					    size);
	  frame->PushStore(addr, block);
	}
	break;
      case Pickle::aMBLOCK:
	{
	  u_int label  = is->GetUInt(); NCHECK_EOB();
	  u_int size   = is->GetUInt(); NCHECK_EOB();
	  u_int addr   = is->GetUInt(); NCHECK_EOB();
	  is->Commit();

	  word block =
	    StoreAbstraction::AnnounceBlock
	    (static_cast<BlockLabel>(label), size, true);
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

  void UnpickleWorker::DumpFrame(StackFrame *, std::ostream& out) {
    out << "[Unpickler::Unpickle]" << std::endl;
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
    virtual void DumpFrame(StackFrame *sFrame, std::ostream& out);
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

    u_char* magic = is->GetBytes(4); PCHECK_EOB();
    bool flag = false;
    u_int stackSize = 0;
    u_int localsSize = 0;

    if (!strncmp(reinterpret_cast<char*>(magic), "seam", 4)) {
      is->Commit();
      u_int major = is->GetUInt(); PCHECK_EOB();
      is->Commit();
      if (major==Pickle::majorVersion) {
	u_int minor = is->GetUInt(); PCHECK_EOB();
	is->Commit();
	if (minor<=Pickle::minorVersion) {
	  is->Commit();
	  u_char tag = is->GetByte(); PCHECK_EOB();

	  if (static_cast<Pickle::Tag>(tag) == Pickle::INIT) {
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
	}
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

  void PickleCheckWorker::DumpFrame(StackFrame *, std::ostream& out) {
    out << "[Unpickler::Check]" << std::endl;
  }

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
    Scheduler::SetCurrentData(Pickler::IOError); // to be done
    StackFrame *frame = Scheduler::GetFrame();
    Scheduler::SetCurrentBacktrace(Backtrace::New(frame->Clone()));
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
  Corrupt = UniqueString::New(String::New("@Pickle.Corrupt"))->ToWord();
  RootSet::Add(Corrupt);
}

void Unpickler::RegisterHandler(String *name, handler handler) {
  word x = Store::UnmanagedPointerToWord(reinterpret_cast<void *>(handler));
  ChunkMap::FromWordDirect(handlerTable)->Put(name->ToWord(), x);
}


namespace gzip {
    typedef unsigned char ubyte_t;

    struct header {
        ubyte_t id1;
        ubyte_t id2;
        ubyte_t cm;
        ubyte_t flags;
        ubyte_t mtime[4];
        ubyte_t extra_flags;   
        ubyte_t os;
    };

    struct trailer {
        ubyte_t crc32[4];
        ubyte_t isize[4];
    };

    namespace {
        unsigned long number2 (const ubyte_t *arr) {
            return static_cast<unsigned long>(arr[0]) + static_cast<unsigned long>(arr[1]) * 256UL;
        }

        unsigned long number4 (const ubyte_t *arr) {
            return (static_cast<unsigned long>(arr[0]) +
                    static_cast<unsigned long>(arr[1]) * 256UL +
                    static_cast<unsigned long>(arr[2]) * 256UL * 256UL +
                    static_cast<unsigned long>(arr[3]) * 256UL * 256UL);
        }
    }

    namespace flags {
        const int text    = 0;
        const int hcrc    = 1;
        const int extra   = 2;
        const int name    = 3;
        const int comment = 4;
        
        bool is_set (ubyte_t b, int flag) {
            return b & (1 << flag);
        }
    }

    namespace {
        bool skip_cstr (ubyte_t *&s, unsigned long &srclen) {
            while (*s++) { 
                srclen--;
                if (srclen <= 0) return false;
            }
            srclen--;
            if (srclen <= 0) return false;
            return true;
        }
    }

    bool parse_meta_data (ubyte_t *src, unsigned long srclen,
                          ubyte_t *&compressed_start, 
                          unsigned long &compressed_len,
                          unsigned long &uncompressed_len) {
        if (srclen < sizeof(header) + sizeof(trailer)) return false;
        const header  *h = reinterpret_cast<const header*>(src);
        src += sizeof(header);
        srclen -= (sizeof(header) + sizeof(trailer));
        // correct gzip magic identification numbers?
        if (h->id1 != 0x1f || h->id2 != 0x8b) return false;
        // only compression method = deflate (8) is known to us.
        if (h->cm != 8) return false;
        
        if (flags::is_set (h->flags, flags::extra)) {
            unsigned long xlen = number2 (src);
            src += 2 + xlen;
            srclen -= (2 + xlen);
            if (srclen <= 0) return false;
        }

        if (flags::is_set (h->flags, flags::name) && !skip_cstr (src, srclen)) 
            return false;

        if (flags::is_set (h->flags, flags::comment) && !skip_cstr (src, srclen))
            return false;
        
        if (flags::is_set (h->flags, flags::hcrc)) {
            srclen -= 2;
            if (srclen <= 0) return false;
            src+=2;
        }
        
        compressed_start = src;
        
        const trailer *t = reinterpret_cast<const trailer*>(src + srclen);

        uncompressed_len = number4 (t->isize);
        compressed_len = srclen;

        return true;
    }
}


            
String *Unpickler::Unzip (String *s) {
    unsigned long compressed_len;
    unsigned long uncompressed_len;
    u_char *str = s->GetValue ();
    u_char *compressed_start;

    if (gzip::parse_meta_data (str, s->GetSize (), compressed_start,
                compressed_len, uncompressed_len)) {
      
        if (static_cast<uInt>(compressed_len) != compressed_len || static_cast<uInt>(uncompressed_len) != uncompressed_len)
          Error("Unpickler::Unzip");
        
        String *res = String::New (uncompressed_len);
        z_stream stream;
        stream.next_in   = static_cast<Bytef*>(compressed_start);
        stream.avail_in  = static_cast<uInt>(compressed_len);
        stream.next_out  = res->GetValue ();
        stream.avail_out = static_cast<uInt>(uncompressed_len);
        stream.zalloc    = static_cast<alloc_func>(0);
        stream.zfree     = static_cast<free_func>(0);

        // raw deflate no zlib / gzip header handling
        // as we have already done that
        int err = inflateInit2 (&stream, -15);
        if (err != Z_OK) { return 0; }

        err = inflate (&stream, Z_FINISH);
       
        if ( (err == Z_OK || err == Z_STREAM_END) && inflateEnd (&stream) == Z_OK) {
            return res;
        } else {
            return 0;
        }
    } else {
        return 0;
    }
}

