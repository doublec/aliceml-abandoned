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
  static const u_int NOT_FOUND = STATIC_CAST(u_int, -1);

  using Block::ToWord;

  static PartitionSeen *New() {
    Block *p = Store::AllocMutableBlock(SEEN_LABEL, SIZE);
    p->InitArg(COUNTER_POS, 0);
    p->InitArg(TABLE_POS, Map::New(initialSize)->ToWord());
    return STATIC_CAST(PartitionSeen *, p);
  }
  static PartitionSeen *FromWordDirect(word w) {
    Block *b = Store::DirectWordToBlock(w);
    Assert(b->GetLabel() == SEEN_LABEL);
    return STATIC_CAST(PartitionSeen *, b);
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

class PartitionLoaderArgs {
private:
  enum { PARTITION_POS, SEEN_POS, SIZE };
public:
  static void New(Partition *p, PartitionSeen *seen) {
    Scheduler::SetNArgs(SIZE);
    Scheduler::SetCurrentArg(PARTITION_POS, p->ToWord());
    Scheduler::SetCurrentArg(SEEN_POS, seen->ToWord());
  }
  static Partition *GetPartition() {
    Assert(Scheduler::GetNArgs() == SIZE);
    return Partition::FromWordDirect(Scheduler::GetCurrentArg(PARTITION_POS));
  }
  static PartitionSeen *GetPartitionSeen() {
    Assert(Scheduler::GetNArgs() == SIZE);
    return PartitionSeen::FromWordDirect(Scheduler::GetCurrentArg(SEEN_POS));
  }
};


class PartitionLoaderFrame: private StackFrame {
private:
  enum { DATA_POS, EDGE_POS, PARENT_POS, SIZE };
public:

  static PartitionLoaderFrame *New(Worker *worker, word data, int edge,
                                   int parent) {
    NEW_STACK_FRAME(frame, worker, SIZE);
    frame->InitArg(DATA_POS, data);
    frame->InitArg(EDGE_POS, Store::IntToWord(edge));
    frame->InitArg(PARENT_POS, Store::IntToWord(parent));
    return STATIC_CAST(PartitionLoaderFrame *, frame);
  }

  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }

  word GetData() {
    return StackFrame::GetArg(DATA_POS);
  }

  int GetEdge() {
    return Store::WordToInt(GetArg(EDGE_POS));
  }

  int GetParent() {
    return Store::WordToInt(GetArg(PARENT_POS));
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
  static void PushFrame(word data, int edge, int parent);
  virtual u_int GetFrameSize(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *sFrame);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *);
};

PartitionLoaderWorker *PartitionLoaderWorker::self;

void PartitionLoaderWorker::PushFrame(word data, int edge, int parent) {
  PartitionLoaderFrame::New(self, data, edge, parent);
}

u_int PartitionLoaderWorker::GetFrameSize(StackFrame *sFrame) {
  Assert(sFrame->GetWorker() == this);
  PartitionLoaderFrame *frame =
    STATIC_CAST(PartitionLoaderFrame *, sFrame);  
  return frame->GetSize();
}

#define MYCONTINUE() {				\
  if (StatusWord::GetStatus() != 0)		\
    return Worker::PREEMPT;			\
  else						\
    return Worker::CONTINUE;			\
}

Worker::Result PartitionLoaderWorker::Run(StackFrame *sFrame) {
  PartitionLoaderFrame *frame =
    STATIC_CAST(PartitionLoaderFrame *, sFrame);
  word x0 = frame->GetData();

  if (Store::WordToTransient(x0) != INVALID_POINTER) {
    // Currently, transients are requested.
    // This seems natural but should be looked into!
    Scheduler::SetCurrentData(x0);
    //    Scheduler::PushFrameNoCheck(frame->ToWord());
    return Worker::REQUEST;
  }
  
  int i = Store::WordToInt(x0);
  if (i!=INVALID_INT) {
    Scheduler::PopFrame(frame->GetSize());
    MYCONTINUE(); // we don't want to handle ints
  }

  PartitionSeen *seen = PartitionLoaderArgs::GetPartitionSeen();
  Partition *p = PartitionLoaderArgs::GetPartition();

  Block *b = Store::WordToBlock(x0);

  int edge = frame->GetEdge();
  int parent = frame->GetParent();
  int size;
  u_int oldIndex = seen->Find(b);
  if (oldIndex == PartitionSeen::NOT_FOUND) {
    int nodeIndex = p->InsertNode(x0);
    seen->Add(b);
    // parent==-1 means that this is the root node
    if (parent!=-1) {
      p->AddParent(nodeIndex, edge, parent);
    }
    switch(b->GetLabel()) {
    case CHUNK_LABEL:
      // Chunks look like blocks but don't have children! ;-)
      break;
    default:
      size = b->GetSize();
      Scheduler::PopFrame(frame->GetSize());
      for (u_int i = size; i--; ) {
	PartitionLoaderWorker::PushFrame(b->GetArg(i), i, nodeIndex);
      }
      MYCONTINUE();
    }
  } else {
    // If we've already seen this node, add the predecessor
    // to its parents list
    if (parent!=-1)
      p->AddParent(oldIndex, edge, parent);
  }
  Scheduler::PopFrame(frame->GetSize());
  MYCONTINUE();
}

const char *PartitionLoaderWorker::Identify() {
  return "PartitionLoaderWorker";
}

void PartitionLoaderWorker::DumpFrame(StackFrame *) {
  std::fprintf(stderr, "PartitionLoader Task\n");
}

void PartitionLoader::Init() {
  PartitionLoaderWorker::Init();
}

Worker::Result PartitionLoader::Load(Partition *p, word x) {
  Scheduler::PopFrame();
  PartitionLoaderWorker::PushFrame(x, -1, -1);
  PartitionLoaderArgs::New(p, PartitionSeen::New());
  return Worker::CONTINUE;
}
