//
// Author:
//   Guido Tack <tack@ps.uni-sb.de>
// 
// Copyright:
//   Guido Tack, 2002
// 
// Last change:
//   $Date$ by $Author$
//   $Revision$
// 

#include "alice/Authoring.hh"
#include "generic/Minimizer.hh"

#define SIZEWORKERCONTINUE() {				\
  if (StatusWord::GetStatus() != 0)		\
    return Worker::PREEMPT;			\
  else						\
    return Worker::CONTINUE;			\
}

class SizeWorkerSeen: private Block {
private:
  static const BlockLabel SEEN_LABEL = MIN_DATA_LABEL;
  enum { COUNTER_POS, TABLE_POS, SIZE };
  static const u_int initialSize = 8; //--** to be checked
public:
  static const u_int NOT_FOUND = STATIC_CAST(u_int, -1);

  using Block::ToWord;

  static SizeWorkerSeen *New() {
    Block *p = Store::AllocMutableBlock(SEEN_LABEL, SIZE);
    p->InitArg(COUNTER_POS, STATIC_CAST(s_int, 0));
    p->InitArg(TABLE_POS, Map::New(initialSize)->ToWord());
    return STATIC_CAST(SizeWorkerSeen *, p);
  }
  static SizeWorkerSeen *FromWordDirect(word w) {
    Block *b = Store::DirectWordToBlock(w);
    Assert(b->GetLabel() == SEEN_LABEL);
    return STATIC_CAST(SizeWorkerSeen *, b);
  }

  void Add(Block *v) {
    word counter = GetArg(COUNTER_POS);
    Map *map     = Map::FromWordDirect(GetArg(TABLE_POS));
    map->Put(v->ToWord(), counter);
    ReplaceArg(COUNTER_POS, Store::DirectWordToInt(counter) + 1);
  }
  u_int Find(Block *v) {
    word vw  = v->ToWord();
    Map *map = Map::FromWordDirect(GetArg(TABLE_POS));
    if (map->IsMember(vw))
      return Store::DirectWordToInt(map->Get(vw));
    else
      return NOT_FOUND;
  }
};

class SizeWorkerArgs {
private:
  enum { BYNEEDS_POS, FUTURES_POS, NODES_POS, TRANSIENTS_POS, WORDS_POS,
         SEEN_POS, REQ_POS, SIZE };
public:
  static void New(SizeWorkerSeen *seen, bool request) {
    Scheduler::SetNArgs(SIZE);
    Scheduler::SetCurrentArg(NODES_POS, Store::IntToWord(0));
    Scheduler::SetCurrentArg(WORDS_POS, Store::IntToWord(0));
    Scheduler::SetCurrentArg(BYNEEDS_POS, Store::IntToWord(0));
    Scheduler::SetCurrentArg(FUTURES_POS, Store::IntToWord(0));
    Scheduler::SetCurrentArg(TRANSIENTS_POS, Store::IntToWord(0));
    Scheduler::SetCurrentArg(SEEN_POS, seen->ToWord());
    Scheduler::SetCurrentArg(REQ_POS, Store::IntToWord(request));
  }
  static SizeWorkerSeen *GetSeen() {
    Assert(Scheduler::GetNArgs() == SIZE);
    return SizeWorkerSeen::FromWordDirect(Scheduler::GetCurrentArg(SEEN_POS));
  }

  static u_int GetSize() {
    Assert(Scheduler::GetNArgs() == SIZE);
    return Store::DirectWordToInt(Scheduler::GetCurrentArg(WORDS_POS));
  }
  static bool GetReq() {
    Assert(Scheduler::GetNArgs() == SIZE);
    return Store::DirectWordToInt(Scheduler::GetCurrentArg(REQ_POS))==1;
  }
  static u_int GetNodes() {
    Assert(Scheduler::GetNArgs() == SIZE);
    return Store::DirectWordToInt(Scheduler::GetCurrentArg(NODES_POS));
  }
  static void IncrementNodes() {
    Scheduler::SetCurrentArg(NODES_POS, Store::IntToWord(GetNodes()+1));
  }
  static void IncrementByneeds() {
    int byneeds = Store::DirectWordToInt(Scheduler::GetCurrentArg(BYNEEDS_POS));
    Scheduler::SetCurrentArg(BYNEEDS_POS, Store::IntToWord(byneeds+1));
  }
  static void IncrementFutures() {
    int futures = Store::DirectWordToInt(Scheduler::GetCurrentArg(FUTURES_POS));
    Scheduler::SetCurrentArg(FUTURES_POS, Store::IntToWord(futures+1));
  }
  static void IncrementTransients() {
    int transients =
      Store::DirectWordToInt(Scheduler::GetCurrentArg(TRANSIENTS_POS));
    Scheduler::SetCurrentArg(TRANSIENTS_POS, Store::IntToWord(transients+1));
  }
  static void AddSize(u_int s) {
    Scheduler::SetCurrentArg(WORDS_POS, Store::IntToWord(GetSize()+s));
  }

};


class SizeWorkerFrame: private StackFrame {
private:
  enum { DATA_POS, SIZE };
public:

  static SizeWorkerFrame *New(Worker *worker, word data) {
    NEW_STACK_FRAME(frame, worker, SIZE);
    frame->InitArg(DATA_POS, data);
    return STATIC_CAST(SizeWorkerFrame *, frame);
  }

  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }

  word GetData() {
    return StackFrame::GetArg(DATA_POS);
  }
};

class  SizeWorker: public Worker {
private:
  static SizeWorker *self;
  // PicklingWorker Constructor
  SizeWorker(): Worker() {}
public:
  // PicklingWorker Static Constructor
  static void Init() {
    self = new SizeWorker();
  }
  // Frame Handling
  static void PushFrame(word data);
  virtual u_int GetFrameSize(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *sFrame);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *);
};

SizeWorker *SizeWorker::self;

void SizeWorker::PushFrame(word data) {
  SizeWorkerFrame::New(self, data);
}

u_int SizeWorker::GetFrameSize(StackFrame *sFrame) {
  Assert(sFrame->GetWorker() == this);
  SizeWorkerFrame *frame =
    STATIC_CAST(SizeWorkerFrame *, sFrame);  
  return frame->GetSize();
}

Worker::Result SizeWorker::Run(StackFrame *sFrame) {
  SizeWorkerFrame *frame =
    STATIC_CAST(SizeWorkerFrame *, sFrame);
  word x0 = frame->GetData();

  bool req = SizeWorkerArgs::GetReq();

  Transient *transient = Store::WordToTransient(x0);
  if (transient != INVALID_POINTER) {
    
    switch (transient->GetLabel()) {
    case BYNEED_LABEL:
      SizeWorkerArgs::IncrementByneeds();
      break;
    case FUTURE_LABEL:
      SizeWorkerArgs::IncrementFutures();
      break;
    default:
      SizeWorkerArgs::IncrementTransients();
    }

    if (req) {
      // Transients are requested.
      
      Scheduler::SetCurrentData(x0);
      return Worker::REQUEST;
    } else {
      Scheduler::PopFrame(frame->GetSize());
      SizeWorker::PushFrame(transient->GetArg());
      SIZEWORKERCONTINUE();
    }
  }
  
  s_int i = Store::WordToInt(x0);
  if (i!=INVALID_INT) {
    Scheduler::PopFrame(frame->GetSize());
    SIZEWORKERCONTINUE();
  }

  SizeWorkerSeen *seen = SizeWorkerArgs::GetSeen();

  Block *b = Store::WordToBlock(x0);

  int size;
  u_int oldIndex = seen->Find(b);
  if (oldIndex == SizeWorkerSeen::NOT_FOUND) {
    seen->Add(b);

    SizeWorkerArgs::IncrementNodes();
    size = b->GetSize();
    SizeWorkerArgs::AddSize(size+1);

    switch(b->GetLabel()) {
    case CHUNK_LABEL:
      // Chunks look like blocks but don't have children! ;-)
      break;
    default:
      Scheduler::PopFrame(frame->GetSize());
      for (u_int i = size; i--; ) {
	SizeWorker::PushFrame(b->GetArg(i));
      }
      SIZEWORKERCONTINUE();
    }
  }
  Scheduler::PopFrame(frame->GetSize());
  SIZEWORKERCONTINUE();
}

const char *SizeWorker::Identify() {
  return "SizeWorker";
}

void SizeWorker::DumpFrame(StackFrame *) {
  std::fprintf(stderr, "SizeWorker Task\n");
}

class StoreSize {
public:
  static Worker::Result Compute(word x, bool request) {
    Scheduler::PopFrame();
    SizeWorker::PushFrame(x);
    SizeWorkerArgs::New(SizeWorkerSeen::New(), request);
    return Worker::CONTINUE;
  }

};



// Alice interface

DEFINE1(StoreInterface_size) {
  PUSH_PRIM_SELF();
  return StoreSize::Compute(x0, true);
} END

DEFINE1(StoreInterface_sizeQuiet) {
  PUSH_PRIM_SELF();
  return StoreSize::Compute(x0, false);
} END

DEFINE1(StoreInterface_collect) {
  StatusWord::SetStatus(Store::GCStatus());
  RETURN_UNIT;
} END

DEFINE2(UnsafeStore_loadGraph) {
  Partition *p = Partition::FromWord(x0);
  PUSH_PRIM_SELF();
  return PartitionLoader::Load(p, x1);
} END

DEFINE0(UnsafeStore_newPartition) {
  word ret = Partition::New()->ToWord();
  RETURN(ret);
} END

DEFINE1(UnsafeStore_minimize) {
  Partition::FromWord(x0)->Minimize();
  RETURN_UNIT;
} END

AliceDll word UnsafeStore() {
  SizeWorker::Init();
  Record *record = Record::New(6);

  INIT_STRUCTURE(record, "UnsafeStore", "size",
		 StoreInterface_size, 1);
  INIT_STRUCTURE(record, "UnsafeStore", "sizeQuiet",
		 StoreInterface_sizeQuiet, 1);
  INIT_STRUCTURE(record, "UnsafeStore", "collect",
		 StoreInterface_collect, 1);
  INIT_STRUCTURE(record, "UnsafeStore", "newPartition",
		 UnsafeStore_newPartition, 0);  
  INIT_STRUCTURE(record, "UnsafeStore", "loadGraph",
		 UnsafeStore_loadGraph, 2);
  INIT_STRUCTURE(record, "UnsafeStore", "minimize",
		 UnsafeStore_minimize, 1);  
  RETURN_STRUCTURE("UnsafeStore$", record);
}
