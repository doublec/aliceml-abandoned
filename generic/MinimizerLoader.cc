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

#include "Minimizer.hh"
#include "store/Map.hh"
#include "generic/Scheduler.hh"

// The PartitionLoaderWorker fills a partition
// with the nodes of the subgraph starting from
// the given node.
// Transients are requested, integers ignored.
// The code is very similar to that of the Pickler
// (surprise... the functionality is very similar, too!)

class PartitionSeen: private Block {
private:
  static const BlockLabel SEEN_LABEL = MIN_DATA_LABEL;
  enum { COUNTER_POS, TABLE_POS, SIZE };
  static const u_int initialSize = 8; //--** to be checked
public:
  static const u_int NOT_FOUND = static_cast<u_int>(-1);

  using Block::ToWord;

  static PartitionSeen *New() {
    Block *p = Store::AllocMutableBlock(SEEN_LABEL, SIZE);
    p->InitArg(COUNTER_POS, static_cast<s_int>(0));
    p->InitArg(TABLE_POS, Map::New(initialSize)->ToWord());
    return static_cast<PartitionSeen *>(p);
  }
  static PartitionSeen *FromWordDirect(word w) {
    Block *b = Store::DirectWordToBlock(w);
    Assert(b->GetLabel() == SEEN_LABEL);
    return static_cast<PartitionSeen *>(b);
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
  void Reset() {
    Map *map     = Map::FromWordDirect(GetArg(TABLE_POS));
    map->Clear();
  }
};

// PLoaderStack
// used for depth first search
class PLoaderStack : private Stack {
private:
  enum { DATA_POS, EDGE_POS, PARENT_POS, FRAME_SIZE };

  static const u_int initialSize = 128*FRAME_SIZE;

public:
  using Stack::ToWord;
  using Stack::IsEmpty;

  static PLoaderStack *New() {
    Stack *s = Stack::New(initialSize);
    return static_cast<PLoaderStack *>(s);
  }

  static PLoaderStack *FromWordDirect(word w) {
    Stack *s = Stack::FromWordDirect(w);
    return static_cast<PLoaderStack *>(s);
  }

  void Push(word data, s_int edge, s_int parent) {
    Stack::AllocArgFrame(FRAME_SIZE);
    Stack::PutFrameArg(DATA_POS, data);
    Stack::PutFrameArg(EDGE_POS, Store::IntToWord(edge));
    Stack::PutFrameArg(PARENT_POS, Store::IntToWord(parent));
  }
  word GetData() {
    return Stack::GetFrameArg(DATA_POS);
  }
  s_int GetEdge() {
    return Store::WordToInt(Stack::GetFrameArg(EDGE_POS));
  }

  s_int GetParent() {
    return Store::WordToInt(Stack::GetFrameArg(PARENT_POS));
  }
  void PopTopFrame() {
    Stack::ClearArgFrameZero(FRAME_SIZE);
  }
  void Reset(word data) {
    SetTop(0);
    PLoaderStack::Push(data, -1, -1);
  }
};

class PartitionLoaderArgs {
private:
  enum { PARTITION_POS, SEEN_POS, STACK_POS, ROOT_POS, TRANSIENT_FOUND_POS,
	 SIZE };
public:
  static void New(Partition *p, word root) {
    Scheduler::SetNArgs(SIZE);
    Scheduler::SetCurrentArg(PARTITION_POS, p->ToWord());
    Scheduler::SetCurrentArg(SEEN_POS, PartitionSeen::New()->ToWord());
    PLoaderStack *ps = PLoaderStack::New();
    ps->Push(root, -1, -1);
    Scheduler::SetCurrentArg(STACK_POS, ps->ToWord());
    Scheduler::SetCurrentArg(ROOT_POS, root);
    Scheduler::SetCurrentArg(TRANSIENT_FOUND_POS, Store::IntToWord(0));
  }
  static Partition *GetPartition() {
    Assert(Scheduler::GetNArgs() == SIZE);
    return Partition::FromWordDirect(Scheduler::GetCurrentArg(PARTITION_POS));
  }
  static PartitionSeen *GetPartitionSeen() {
    Assert(Scheduler::GetNArgs() == SIZE);
    return PartitionSeen::FromWordDirect(Scheduler::GetCurrentArg(SEEN_POS));
  }
  static PLoaderStack *GetPLoaderStack() {
    Assert(Scheduler::GetNArgs() == SIZE);
    return PLoaderStack::FromWordDirect(Scheduler::GetCurrentArg(STACK_POS));
  }
  static bool GetTransientFound() {
    return
      Store::DirectWordToInt(Scheduler::GetCurrentArg(TRANSIENT_FOUND_POS));
  }
  static void SetTransientFound() {
    Scheduler::SetCurrentArg(TRANSIENT_FOUND_POS, Store::IntToWord(1));
  }
  static void Reset() {
    GetPartitionSeen()->Reset();
    GetPLoaderStack()->Reset(Scheduler::GetCurrentArg(ROOT_POS));
    Scheduler::SetCurrentArg(TRANSIENT_FOUND_POS, Store::IntToWord(0));
  }
};

class  PartitionLoaderWorker: public Worker {
private:
  static PartitionLoaderWorker *self;
  // PicklingWorker Constructor
  PartitionLoaderWorker(): Worker() {}
public:
  // PicklingWorker Static Constructor
  static void Init() {
    self = new PartitionLoaderWorker();
  }
  // Frame Handling
  static void PushFrame();
  virtual u_int GetFrameSize(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *sFrame);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *);
};

PartitionLoaderWorker *PartitionLoaderWorker::self;

void PartitionLoaderWorker::PushFrame() {
  NEW_STACK_FRAME(frame, self, 0);
}

u_int PartitionLoaderWorker::GetFrameSize(StackFrame *sFrame) {
  Assert(sFrame->GetWorker() == this);
  return sFrame->GetSize();
}

Worker::Result PartitionLoaderWorker::Run(StackFrame *sFrame) {
  Assert(sFrame != INVALID_POINTER);
  Assert(sFrame->GetWorker() == this);

  PLoaderStack *nps = PartitionLoaderArgs::GetPLoaderStack();
  PartitionSeen *seen = PartitionLoaderArgs::GetPartitionSeen();
  Partition *p = PartitionLoaderArgs::GetPartition();

  for (;;) {
    if (nps->IsEmpty()) {
      Scheduler::PopFrame(sFrame->GetSize());
      if (StatusWord::GetStatus() != 0)
	return Worker::PREEMPT;
      else
	return Worker::CONTINUE;
    }

    word x0 = nps->GetData();

    if (Store::WordToTransient(x0) != INVALID_POINTER) {
      PartitionLoaderArgs::SetTransientFound();
      Scheduler::SetCurrentData(x0);
      return Worker::REQUEST;
    }
  
    u_int i = Store::WordToInt(x0);
    if (i!=INVALID_INT) {
      nps->PopTopFrame();
      continue; // we don't want to handle ints
    }


    Block *b = Store::WordToBlock(x0);

    s_int edge = nps->GetEdge();
    s_int parent = nps->GetParent();
    s_int size;
    u_int oldIndex = seen->Find(b);
    if (oldIndex == PartitionSeen::NOT_FOUND) {
      s_int nodeIndex = p->InsertNode(x0);
      seen->Add(b);
      // parent==-1 means that this is the root node
      if (parent!=-1) {
	p->AddParent(nodeIndex, edge, parent);
      }
      switch(b->GetLabel()) {
      case CHUNK_LABEL:
	// Chunks look like blocks but don't have children! ;-)
	nps->PopTopFrame();
	break;
      default:
	size = b->GetSize();
	nps->PopTopFrame();
	for (u_int i = size; i--; ) {
	  nps->Push(b->GetArg(i), i, nodeIndex);
	}
      }
    } else {
      // If we've already seen this node, add the predecessor
      // to its parents list
      if (parent!=-1)
	p->AddParent(oldIndex, edge, parent);
      nps->PopTopFrame();
    }
  }
}

const char *PartitionLoaderWorker::Identify() {
  return "PartitionLoaderWorker";
}

void PartitionLoaderWorker::DumpFrame(StackFrame *) {
  std::fprintf(stderr, "PartitionLoader Task\n");
}

// PartitionLoaderCheckWorker
class PartitionLoaderCheckWorker: public Worker {
private:
  static PartitionLoaderCheckWorker *self;
  // PartitionLoaderCheckWorker Constructor
  PartitionLoaderCheckWorker(): Worker() {}
public:
  // PartitionLoaderCheckWorker Static Constructor
  static void Init() {
    self = new PartitionLoaderCheckWorker();
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
// PartitionLoaderCheckWorker Functions
//
PartitionLoaderCheckWorker *PartitionLoaderCheckWorker::self;

void PartitionLoaderCheckWorker::PushFrame() {
  NEW_STACK_FRAME(frame, self, 0);
}

u_int PartitionLoaderCheckWorker::GetFrameSize(StackFrame *sFrame) {
  Assert(sFrame->GetWorker() == this);
  return sFrame->GetSize();
}

Worker::Result PartitionLoaderCheckWorker::Run(StackFrame *sFrame) {
  Assert(sFrame->GetWorker() == this);

  if (PartitionLoaderArgs::GetTransientFound()) {
    // Restart if a transient was found
    PartitionLoaderArgs::Reset();
    PartitionLoaderWorker::PushFrame();
    return Worker::CONTINUE;
  }
  Scheduler::PopFrame(sFrame->GetSize());
  return Worker::CONTINUE;
}

const char *PartitionLoaderCheckWorker::Identify() {
  return "PartitionLoaderCheckWorker";
}

void PartitionLoaderCheckWorker::DumpFrame(StackFrame *) {
  std::fprintf(stderr, "PartitionLoader Check\n");
}

void PartitionLoader::Init() {
  PartitionLoaderWorker::Init();
  PartitionLoaderCheckWorker::Init();
}

Worker::Result PartitionLoader::Load(Partition *p, word x) {
  Scheduler::PopFrame();
  //  PartitionLoaderCheckWorker::PushFrame();
  PartitionLoaderWorker::PushFrame();
  PartitionLoaderArgs::New(p, x);
  return Worker::CONTINUE;
}
