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

#if defined(INTERFACE)
#pragma implementation "generic/Minimizer.hh"
#endif

#include "Minimizer.hh"
#include "adt/DynamicArray.hh"
#include "generic/Tuple.hh"

class PBlock : private DynamicArray {
private:
  enum { START_INDEX_POS, END_INDEX_POS,
	 CUR_SPLIT_INDEX_POS,
	 SIZE };
  
public:
  using Block::ToWord;

  static PBlock *New(int start, int end);
  static PBlock *FromWord(word x);
  static PBlock *FromWordDirect(word x);

  void SetEnd(int end);
  int GetStart();
  int GetEnd();
  void SetSplitIndex(u_int i);
  int GetSplitIndex();
  bool HasToBeSplit();
  void IncrementIncoming(int edge);
  void DecrementIncoming(int edge);
  int GetIncoming(int edge);
  int SizeOfIncoming();
  void PutCompleteOnAgenda(u_int blockNo, Stack *agenda);
  void PutOnAgenda(int edge);
  bool IsOnAgenda();
  bool IsOnAgenda(int edge);
  void RemoveFromAgenda(int edge);
};

PBlock *PBlock::New(int start, int end) {
  DynamicArray *p = DynamicArray::New(SIZE+2);
  
  p->Update(START_INDEX_POS, Store::IntToWord(start));
  p->Update(END_INDEX_POS, Store::IntToWord(end));
  p->Update(CUR_SPLIT_INDEX_POS, Store::IntToWord(end));

  // Array cell [0] is used to count with how many edges this
  // block was put on the agenda
  p->Update(SIZE+0, Store::IntToWord(0));

  return STATIC_CAST(PBlock *, p);    
}

PBlock *PBlock::FromWord(word x) {
  return STATIC_CAST(PBlock *, DynamicArray::FromWord(x));
}

PBlock *PBlock::FromWordDirect(word x) {
  return STATIC_CAST(PBlock *, DynamicArray::FromWordDirect(x));
}

void PBlock::SetEnd(int end) {
  Update(END_INDEX_POS, Store::IntToWord(end));
}

int PBlock::GetStart() {
  return Store::DirectWordToInt(Sub(START_INDEX_POS));
}

int PBlock::GetEnd() {
  return Store::DirectWordToInt(Sub(END_INDEX_POS));
}

void PBlock::SetSplitIndex(u_int i) {
  Update(CUR_SPLIT_INDEX_POS, Store::IntToWord(i));
}

int PBlock::GetSplitIndex() {
  return Store::WordToInt(Sub(CUR_SPLIT_INDEX_POS));  
}

bool PBlock::HasToBeSplit() {
  // A block has to be split if the
  // split index was decremented at least once,
  // but not over the start of the block
  // (that is the case when all nodes were marked for splitting)

  int start = GetStart();
  int end = GetEnd();
  int split = GetSplitIndex();

  if (split < start) {
    SetSplitIndex(end);
    return false;
  } else {
    return (split < end);
  }
}

void PBlock::IncrementIncoming(int edge) {
  // maintains the array of incoming edges
  word old = Sub(SIZE+edge+1);
  if (old==DynamicArray::INVALID_ARRAY_ELEM) {
    Update(SIZE+edge+1, Store::IntToWord(1));
  } else {
    int oldI = Store::DirectWordToInt(old);
    // oldI<0 means this block is on agenda with this edge
    // => increment the absolute value
    Update(SIZE+edge+1, Store::IntToWord(oldI<0 ? oldI-1 : oldI+1));    
  }
}

void PBlock::DecrementIncoming(int edge) {
  // maintains the array of incoming edges
  word old = Sub(SIZE+edge+1);
  if (old!=DynamicArray::INVALID_ARRAY_ELEM) {
    int oldI = Store::WordToInt(old);
    // if last edge is removed, this block doesn't have to
    // stay on the agenda with that edge
    if (oldI==-1) {
      RemoveFromAgenda(edge);
      oldI = 1;
    }
    // oldI<0 means this block is on agenda with this edge
    // => decrement the absolute value
    Update(SIZE+edge+1, Store::IntToWord(oldI<0 ? oldI+1 : oldI-1));   
  }
}

int PBlock::GetIncoming(int edge) {
  // returns the number of incoming edges with label "edge"
  word old = Sub(SIZE+edge+1);
  if (old==DynamicArray::INVALID_ARRAY_ELEM) {
    return 0;
  } else {
    int oldI = Store::DirectWordToInt(old);
    return (oldI<0 ? -oldI : oldI);
  }
}

int PBlock::SizeOfIncoming() {
  // the length of the incoming array is used as an upper bound
  // for the maximum label of an incoming edge
  return GetLength()-1-SIZE;
}

void PBlock::PutOnAgenda(int edge) {
  // marks block as being on agenda with edge "edge"
  // increments the agenda count

  // an egde is marked by making its incoming counter negative

  int count = Store::DirectWordToInt(Sub(SIZE+0));
  if (!IsOnAgenda(edge)) {
    word old = Sub(SIZE+edge+1);
    if (old!=DynamicArray::INVALID_ARRAY_ELEM) {
      int oldI = Store::WordToInt(old);
      if (oldI!=0) {
	Update(SIZE+edge+1, Store::IntToWord(-oldI));
	count++;
	Update(SIZE+0, Store::IntToWord(count));
      }
    }
  }
}

bool PBlock::IsOnAgenda() {
  // Is this block on the agenda at all?
  return (Store::DirectWordToInt(Sub(SIZE+0))!=0);
}

bool PBlock::IsOnAgenda(int edge) {
  // Is this block on the agenda with egde "edge"?

  word old = Sub(SIZE+edge+1);
  return (old!=DynamicArray::INVALID_ARRAY_ELEM &&
	  Store::DirectWordToInt(old) < 0);
}

void PBlock::RemoveFromAgenda(int edge) {
  // removes the mark created with PutOnAgenda
  // decrements the agenda count

  int count = Store::DirectWordToInt(Sub(SIZE+0));
  if (IsOnAgenda(edge)) {
    word old = Sub(SIZE+edge+1);
    if (old!=DynamicArray::INVALID_ARRAY_ELEM) {
      int oldI = Store::WordToInt(old);
      if (oldI!=0) {
	Update(SIZE+edge+1, Store::IntToWord(-oldI));
	count--;
	Update(SIZE+0, Store::IntToWord(count));
      }
    }
  }

}

void PBlock::PutCompleteOnAgenda(u_int blockNo, Stack *agenda) {
  // Pushes the block on the agenda and marks every incoming edge
  // as being on the agenda

  bool flag = false;

  word incoming;
  
  for (int i = GetLength()-1-SIZE; i--; ) {
    incoming = Sub(SIZE+i+1);
    if (incoming != DynamicArray::INVALID_ARRAY_ELEM
	&& Store::WordToInt(incoming) != 0) {
      flag = true;
      PutOnAgenda(i);
    }
  }
  if (flag)
    agenda->SlowPush(blockNo);
}

class PNode : private DynamicArray {
private:
  enum { BLOCK_POS, NODE_POS, NUM_POS,
	 PRED_COUNT_POS,
	 SIZE };

  static int compareLabels(BlockLabel vl, BlockLabel wl);
  static int compareSize(::Block *v, ::Block *w);
  static int compareChildren(::Block *v, ::Block *w);
  static int compareChunks(Chunk *c1, Chunk *c2);
  static int compareBlocks(::Block *v, BlockLabel vl,
			   ::Block *w);
  static int compare(PNode *n1, PNode *n2);

public:
  using Block::ToWord;

  static PNode *New(word node);
  static PNode *FromWord(word x);
  static PNode *FromWordDirect(word x);
  int GetBlock();
  int GetNum();
  void SetNum(u_int num);
  void SetBlock(PBlock* newBlock, int newBlockNo);
  void ChangeBlock(PBlock* oldBlock, PBlock* newBlock, int newBlockNo);
  void AddParent(int egde, int parent);
  word GetNode();
  u_int GetPredCount();
  word Pred(u_int index);
  int Compare(PNode *pn);
};

///////////////////////
// Comparison function establishes a total order
// on nodes of the data graph
//
int PNode::compareLabels(BlockLabel vl, BlockLabel wl) {
  if (vl==wl) return 0;
  if (vl<wl) return -1;
    return 1;
}

int PNode::compareSize(::Block *v, ::Block *w) {
  u_int vs = v->GetSize();
  u_int ws = w->GetSize();
  if (vs == ws) return 0;
  if (vs < ws) return -1;
  return 1;
}

int PNode::compareChildren(::Block *v, ::Block *w) {
  // This comparison only looks for integers as children
  u_int vs = v->GetSize();

   for (int i=vs; i--; ) {
    int cv = Store::WordToInt(v->GetArg(i));
    int cw = Store::WordToInt(w->GetArg(i));

    if (cv != INVALID_INT && cw != INVALID_INT) {
      if (cv<cw) return -1;
      else if (cv>cw) return 1;
    } else if (cv == INVALID_INT && cw !=INVALID_INT) {
      return 1;
    } else if (cv != INVALID_INT && cw == INVALID_INT) {
      return -1;
    }
  }
  return 0;
}

int PNode::compareChunks(Chunk *c1, Chunk *c2) {
  // lexicographic ordering by size and then std::memcmp
  int size1 = c1->GetSize();
  int size2 = c2->GetSize();
  if (size1<size2) return -1;
  if (size1>size2) return 1;
  u_char *c1c = reinterpret_cast<u_char *>(c1->GetBase());
  u_char *c2c = reinterpret_cast<u_char *>(c2->GetBase());
  int res = std::memcmp(c1c, c2c, size1);
  if (res<0) return -1;
  if (res>0) return 1;
  return 0;
}

int PNode::compareBlocks(::Block *v, BlockLabel vl,
                         ::Block *w) {
  // this is a comparison of blocks which have
  // THE SAME label!
  
  int cSize;
  
  switch(vl) {
  case GENSET_LABEL:
  case INT_MAP_LABEL:
  case CHUNK_MAP_LABEL:
  case MAP_LABEL:
  case HASHNODEARRAY_LABEL:
  case HASHNODE_LABEL:
    //  case HANDLEDHASHNODE_LABEL:
  case QUEUE_LABEL:
    //  case QUEUEARRAY_LABEL:
  case STACK_LABEL:
    //  case STACKARRAY_LABEL:
  case ROOTSETELEMENT_LABEL:
  case THREAD_LABEL:
  case TASKSTACK_LABEL:
  case ARGS_LABEL:
  case IODESC_LABEL:
  case HOLE_LABEL:
  case FUTURE_LABEL:
  case REF_LABEL:
  case CANCELLED_LABEL:
  case BYNEED_LABEL:
  case WEAK_MAP_LABEL:
  case UNIQUESTRING_LABEL:
  case CONCRETE_LABEL: // is concrete really mutable???
    if (v<w) return -1;
    return 1;
    // incomparable types, each gets its own block
    
  case CLOSURE_LABEL:
  case TUPLE_LABEL:
  case TRANSFORM_LABEL:
    cSize = compareSize(v,w);
    if (cSize != 0)
      return cSize;
    return compareChildren(v,w);
  case CHUNK_LABEL:
    if (v->IsMutable()) {
      if (v<w) return -1;
      return 1;
    } else {
      return compareChunks(Chunk::FromWordDirect(v->ToWord()),
      			   Chunk::FromWordDirect(w->ToWord()));
    }
  default:
    if (v->IsMutable()) {
      if (v<w) return -1;
      else return 1;
    } else {
      cSize = compareSize(v,w);
      if (cSize != 0)
	return cSize;
      return compareChildren(v,w);
    }
  }
}

int PNode::compare(PNode *n1, PNode *n2) {
  word a = n1->GetNode();
  word b = n2->GetNode();
  
  if (a==b) return 0;
  
  // no integers should occur!!!

  Assert(Store::WordToInt(a) == INVALID_INT);
  Assert(Store::WordToInt(b) == INVALID_INT);
  
  // compare
  ::Block *v   = Store::WordToBlock(a);
  BlockLabel vl = v->GetLabel();
  
  ::Block *w   = Store::WordToBlock(b);
  BlockLabel wl = w->GetLabel();
  
  // first compare the labels
  switch(compareLabels(vl, wl)) {
  case -1:
    return -1;
  case 1:
    return 1;
  case 0:
    return compareBlocks(v,vl,w);
  }
  Assert(false);
}

// provides a dynamic array for PNodes
// with a quicksort implementation
// and a lookup table from initial indices to
// current indices (maintaining the permutation
// of the array)

class DynNodeArray : private Block {
private:
  static const BlockLabel DYNNODEARRAY_LABEL = (BlockLabel) (MIN_DATA_LABEL+1);
  enum { ARRAY_POS, LUA_ARRAY_POS, COUNT_POS, SIZE };

  static void swap(DynamicArray* a, DynamicArray *lua, u_int i, u_int j);
  static int partition(DynamicArray *a, DynamicArray *lua, int l, int r);
  static void sort(DynamicArray *a, DynamicArray *lua, int l, int r);

public:
  using Block::ToWord;

  static DynNodeArray *New(int initialSize);
  static DynNodeArray *FromWord(word x);
  static DynNodeArray *FromWordDirect(word x);

  int InitNext(PNode *pn);
  int GetCount();
  void Update(u_int index, PNode *pn);
  PNode *Sub(u_int index);
  PNode *LookUpSub(u_int index);
  void LookUpSwap(u_int i, u_int j);
  void Swap(u_int i, u_int j);
  u_int LookUp(u_int i);
  void Sort();
};

// provides a dynamic array for PNodes
// with a quicksort implementation
// and a lookup table from initial indices to
// current indices (maintaining the permutation
// of the array)


/////////////////////////////
// Quicksort helper functions

void DynNodeArray::swap(DynamicArray* a, DynamicArray *lua, u_int i, u_int j) {
  PNode *pni = PNode::FromWordDirect(a->Sub(i));
  PNode *pnj = PNode::FromWordDirect(a->Sub(j));

  a->Update(i, pnj->ToWord());
  a->Update(j, pni->ToWord());

  int lui = pni->GetNum();
  int luj = pnj->GetNum();
  
  //  fprintf(stderr, "swap %d <-> %d (%d, %d)\n", i, j, lui, luj);

  // maintain permutation information
  word dummy = lua->Sub(lui);
  int dummy_i = Store::WordToInt(dummy);
  if (dummy_i<0)
    fprintf(stderr, "STRANGE! dummy_i %d %d -> %d",i,j,dummy_i);
  int dummy_j = Store::WordToInt(lua->Sub(luj));
  if (dummy_j<0)
    fprintf(stderr, "STRANGE! dummy_j %d %d -> %d",lui,luj,dummy_j);

  lua->Update(lui, lua->Sub(luj));
  lua->Update(luj, dummy);
}

int DynNodeArray::partition(DynamicArray *a, DynamicArray *lua, int l, int r) {
  int i = l-1, j = r;
  PNode *v = PNode::FromWordDirect(a->Sub(r));
  for(;;) {
    while (PNode::FromWordDirect(a->Sub(++i))->Compare(v)==-1);
    while (v->Compare(PNode::FromWordDirect(a->Sub(--j))) == -1)
      if (j==l) break;
    if (i >= j) break;
    swap(a, lua, i, j);
  }
  swap(a, lua, i, r);
  return i;
}

void DynNodeArray::sort(DynamicArray *a, DynamicArray *lua, int l, int r) {
  // a standard (not optimized) quicksort
  // from --TODO--
  if (r <= l) return;
  int i = partition(a, lua, l, r);
  sort(a, lua, l, i-1);
  sort(a, lua, i+1, r);
}




/////////////////////////////

DynNodeArray *DynNodeArray::New(int initialSize) {
  Block *p = Store::AllocMutableBlock(DYNNODEARRAY_LABEL, SIZE);
  
  p->InitArg(COUNT_POS, 0);
  p->InitArg(ARRAY_POS, DynamicArray::New(initialSize)->ToWord());
  p->InitArg(LUA_ARRAY_POS, DynamicArray::New(initialSize)->ToWord());
  return STATIC_CAST(DynNodeArray *, p);
}

DynNodeArray *DynNodeArray::FromWord(word x) {
  Block *b = Store::WordToBlock(x);
  Assert(b == INVALID_POINTER || b->GetLabel() == DYNNODEARRAY_LABEL);
  return STATIC_CAST(DynNodeArray *, b);
}

DynNodeArray *DynNodeArray::FromWordDirect(word x) {
  Block *b = Store::DirectWordToBlock(x);
  Assert(b->GetLabel() == DYNNODEARRAY_LABEL);
  return STATIC_CAST(DynNodeArray *, b);
}

int DynNodeArray::InitNext(PNode *pn) {
  // Initializes the next free array cell
  // with pn
  DynamicArray *na = DynamicArray::FromWordDirect(GetArg(ARRAY_POS));
  DynamicArray *lua = DynamicArray::FromWordDirect(GetArg(LUA_ARRAY_POS));
  int count = Store::DirectWordToInt(GetArg(COUNT_POS));
  na->Update(count, pn->ToWord());
  lua->Update(count, Store::IntToWord(count)); // no permutation
  pn->SetNum(count); // each node carries its initial index

  count++;
  ReplaceArg(COUNT_POS, Store::IntToWord(count));
  return count-1;
}

int DynNodeArray::GetCount() {
  return Store::DirectWordToInt(GetArg(COUNT_POS));
}

void DynNodeArray::Update(u_int index, PNode *pn) {
  DynamicArray *na = DynamicArray::FromWordDirect(GetArg(ARRAY_POS));
  na->Update(index, pn->ToWord());
}
 
PNode *DynNodeArray::Sub(u_int index) {
  DynamicArray *na = DynamicArray::FromWordDirect(GetArg(ARRAY_POS));
  return PNode::FromWordDirect(na->Sub(index));
}

PNode *DynNodeArray::LookUpSub(u_int index) {
  // Translates index according to permutation,
  // then does a sub of the translated index
  DynamicArray *lua = DynamicArray::FromWordDirect(GetArg(LUA_ARRAY_POS));
  return Sub(Store::WordToInt(lua->Sub(index)));
}

void DynNodeArray::LookUpSwap(u_int i, u_int j) {
  // Translated both i and j according to permutation,
  // then does a swap of the translated indices
  DynamicArray *lua = DynamicArray::FromWordDirect(GetArg(LUA_ARRAY_POS));
  Swap(Store::WordToInt(lua->Sub(i)),
       Store::WordToInt(lua->Sub(j)));
}

u_int DynNodeArray::LookUp(u_int i) {
  // returns the translated index
  DynamicArray *lua = DynamicArray::FromWordDirect(GetArg(LUA_ARRAY_POS));
  return Store::WordToInt(lua->Sub(i));
}

void DynNodeArray::Swap(u_int i, u_int j) {
  // wrapper for swap
  DynamicArray *na = DynamicArray::FromWordDirect(GetArg(ARRAY_POS));
  DynamicArray *lua = DynamicArray::FromWordDirect(GetArg(LUA_ARRAY_POS));
  swap(na, lua, i, j);
}

void DynNodeArray::Sort() {
  // wrapper for sort
  DynamicArray *na = DynamicArray::FromWordDirect(GetArg(ARRAY_POS));
  DynamicArray *lua = DynamicArray::FromWordDirect(GetArg(LUA_ARRAY_POS));
  sort(na, lua, 0, GetCount()-1);
}

/////////////////////////////////////////////////////


// Constructors & accessors
///////////////////////////
PNode *PNode::New(word node) {
  DynamicArray *p = DynamicArray::New(SIZE+1);
  p->Update(BLOCK_POS, Store::IntToWord(-1));
  p->Update(NODE_POS, node);
  p->Update(NUM_POS, Store::IntToWord(-1));
  p->Update(PRED_COUNT_POS, Store::IntToWord(0));
  return STATIC_CAST(PNode *, p);    
}

PNode *PNode::FromWord(word x) {
  return STATIC_CAST(PNode *, DynamicArray::FromWord(x));
}
PNode *PNode::FromWordDirect(word x) {
  return STATIC_CAST(PNode *, DynamicArray::FromWordDirect(x));
}

int PNode::GetBlock() {
  // Returns the number of the block this node
  // currently belongs to
  return Store::DirectWordToInt(DynamicArray::Sub(BLOCK_POS));
}

int PNode::GetNum() {
  // Returns the initial index of this node in the partition array
  return Store::DirectWordToInt(DynamicArray::Sub(NUM_POS));
}

void PNode::SetNum(u_int num) {
  // Sets the initial index of this node in the partition array
  DynamicArray::Update(NUM_POS, Store::IntToWord(num));
}

void PNode::SetBlock(PBlock *newBlock, int newBlockNo) {
  // Sets the block no. of this node and
  // changes the block's table of incoming edges accordingly
  DynamicArray::Update(BLOCK_POS, Store::IntToWord(newBlockNo));

  int count = Store::WordToInt(DynamicArray::Sub(PRED_COUNT_POS));
  for (int i=count; i--;) {
    Tuple *tu = Tuple::FromWordDirect(Pred(i));
    int edge = Store::WordToInt(tu->Sel(0));
    newBlock->IncrementIncoming(edge);
  }
}

void PNode::ChangeBlock(PBlock *oldBlock, PBlock *newBlock, int newBlockNo) {
  // like SetBlock, but puts a node into a different block
  DynamicArray::Update(BLOCK_POS, Store::IntToWord(newBlockNo));

  int count = Store::WordToInt(DynamicArray::Sub(PRED_COUNT_POS));
  for (int i=count; i--;) {
    Tuple *tu = Tuple::FromWordDirect(Pred(i));
    int edge = Store::WordToInt(tu->Sel(0));
    oldBlock->DecrementIncoming(edge);
    newBlock->IncrementIncoming(edge);
  }
  
}

void PNode::AddParent(int egde, int parent) {
  // Adds node with no. parent to the array of parent
  // nodes at edge label edge
  Tuple *t = Tuple::New(2);
  t->Init(0,Store::IntToWord(egde));
  t->Init(1,Store::IntToWord(parent));

  int count = Store::WordToInt(DynamicArray::Sub(PRED_COUNT_POS));
  DynamicArray::Update(SIZE+count, t->ToWord());
  count++;
  DynamicArray::Update(PRED_COUNT_POS, Store::IntToWord(count));
}

word PNode::GetNode() {
  return DynamicArray::Sub(NODE_POS);
}

word PNode::Pred(u_int i) {
  // Returns the predecessor at index i
  return DynamicArray::Sub(SIZE+i);
}

u_int PNode::GetPredCount() {
  // Returns the no. of direct predecessors of this node
  return Store::WordToInt(DynamicArray::Sub(PRED_COUNT_POS));
}

int PNode::Compare(PNode *pn) {
  // wrapper for compare
  return compare(this, pn);
}

// Constructor & accessor functions
///////////////////////////////////

Partition *Partition::New() {
  Block *b = Store::AllocMutableBlock(PARTITION_LABEL, SIZE);
  b->InitArg(NA_POS, DynNodeArray::New(100)->ToWord());
  b->InitArg(BA_POS, DynamicArray::New(100)->ToWord());
  b->InitArg(TO_DO_POS, 1); /* nil */
  b->InitArg(BLOCK_COUNT_POS, 0);
  return STATIC_CAST(Partition *, b);    
}

Partition *Partition::FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == PARTITION_LABEL);
    return STATIC_CAST(Partition *, b);
  }

Partition *Partition::FromWordDirect(word x) {
  Block *b = Store::DirectWordToBlock(x);
  Assert(b->GetLabel() == PARTITION_LABEL);
  return STATIC_CAST(Partition *, b);
}

//////////////////////////////////////

int Partition::InsertNode(word node) {
  DynNodeArray *na = DynNodeArray::FromWordDirect(GetArg(NA_POS));
  return na->InitNext(PNode::New(node));
}

void Partition::InitBlocks() {
  // Sorts the partition array
  // Scans linearly through the array and puts
  // equal (<=> adjacent and equal) nodes into
  // the same (newly created) block

  DynNodeArray *na = DynNodeArray::FromWordDirect(GetArg(NA_POS));
  DynamicArray *ba = DynamicArray::FromWordDirect(GetArg(BA_POS));

  int count = na->GetCount();
  int blockCount = Store::DirectWordToInt(GetArg(BLOCK_COUNT_POS));
  
  na->Sort();
  
  PBlock *newBlock = PBlock::New(0,0);
  PNode *lastItem = na->Sub(0);
  lastItem->SetBlock(newBlock, blockCount);
  PNode *curItem;
  for (int i=1; i<count; i++) {
    curItem = na->Sub(i);
    if(curItem->Compare(lastItem) != 0) {
      newBlock->SetEnd(i-1);
      newBlock->SetSplitIndex(i-1);
      ba->Update(blockCount, newBlock->ToWord());
      blockCount++;
      newBlock = PBlock::New(i,0);
    }
    curItem->SetBlock(newBlock, blockCount);
    lastItem = curItem;
  }
  newBlock->SetEnd(count-1);
  newBlock->SetSplitIndex(count-1);
  ba->Update(blockCount, newBlock->ToWord());
  blockCount++;
  ReplaceArg(BLOCK_COUNT_POS, blockCount);
}

bool Partition::splitBlockAtNode(u_int block, u_int nodeIndex) {
  // Marks block "block" as to be split at node with index nodeIndex

  DynamicArray *ba = DynamicArray::FromWordDirect(GetArg(BA_POS));
  PBlock *b = PBlock::FromWordDirect(ba->Sub(block));
  DynNodeArray *na = DynNodeArray::FromWordDirect(GetArg(NA_POS));
  
  u_int si = b->GetSplitIndex();
  u_int end = b->GetEnd();

  bool result=false;

  if (si >= end) {
    // push block on toDo, because it has not been split yet
    // in this iteration
    word head = GetArg(TO_DO_POS);
    Block *tv = Store::AllocBlock(MIN_DATA_LABEL,2);
    tv->InitArg(0, Store::IntToWord(block));
    tv->InitArg(1, head);
    ReplaceArg(TO_DO_POS, tv->ToWord());
  }  
  if (si>=nodeIndex) {
    // only do the actual split if the split index is to the
    // right of the node!
    na->Swap(si, nodeIndex);
    b->SetSplitIndex(si-1);
    result=true;
  }
  return result;
}

void Partition::AddParent(int nodeIndex, int edge, int parent) {
  DynNodeArray *na = DynNodeArray::FromWordDirect(GetArg(NA_POS));
  PNode *p = na->Sub(nodeIndex);
  p->AddParent(edge, parent);
}

void Partition::DoSplits(Stack *q) {
  // for all blocks on toDo:
  //   if splitting has to be done:
  //     newBlock = split oldBlock
  //     for all possible edges i do:
  //       if oldBlock was on agenda (q) with label i:
  //         put (newBlock, i) on agenda
  //       else
  //         put smaller one on agenda
  DynamicArray *ba = DynamicArray::FromWordDirect(GetArg(BA_POS));
  DynNodeArray *na = DynNodeArray::FromWordDirect(GetArg(NA_POS));
  int blockCount = Store::DirectWordToInt(GetArg(BLOCK_COUNT_POS));

  word toDo = GetArg(TO_DO_POS);
  while (Store::WordToInt(toDo)==INVALID_INT) {
    Block *tv = Store::DirectWordToBlock(toDo);
    
    u_int blockNo = Store::WordToInt(tv->GetArg(0));
    PBlock *pb = PBlock::FromWordDirect(ba->Sub(blockNo));
    PBlock *newBlock;
    if (pb->HasToBeSplit()) {
      //      fprintf(stderr, "\tBlock no %d has to be split!\n", blockNo);
      int newEnd = pb->GetEnd();
      int newStart = pb->GetSplitIndex()+1;
      pb->SetEnd(newStart-1);
      pb->SetSplitIndex(newStart-1);
      newBlock = PBlock::New(newStart, newEnd);
      ba->Update(blockCount, newBlock->ToWord());
      for (int i=newStart; i<=newEnd; i++) {
	PNode *node = na->Sub(i);
	node->ChangeBlock(pb, newBlock, blockCount);
      }

      int max = pb->SizeOfIncoming();
      for(int i=0; i<=max; i++) {
	
	int pbInc = pb->GetIncoming(i);
	int nbInc = newBlock->GetIncoming(i);

	if (pb->IsOnAgenda(i)) {
	  //	  fprintf(stderr, "block %d was on agenda, putting new block %d with edge %d\n",
	  //		  blockNo, blockCount, i);
	  if (nbInc>0) {
	    if (!newBlock->IsOnAgenda()) {
	      q->SlowPush(blockCount);
	    }
	    newBlock->PutOnAgenda(i);
	  }
	} else if (pbInc < nbInc && pbInc!=0) {
	  //	  fprintf(stderr, "new block %d larger than %d, putting old block\n",
	  //		  blockCount, blockNo);
	  if (!pb->IsOnAgenda())
	    q->SlowPush(blockNo);
	  pb->PutOnAgenda(i);
	} else if (nbInc!=0) {
	  // fprintf(stderr, "new block %d smaller than %d, putting new block with edge %d\n",
	  // blockCount, blockNo, i);
	  if (!newBlock->IsOnAgenda()) {
	    q->SlowPush(blockCount);
	  }
	  newBlock->PutOnAgenda(i);
	}
      }
      blockCount++;
    }
    toDo = tv->GetArg(1);
  }
  
  ReplaceArg(BLOCK_COUNT_POS, blockCount);
  ReplaceArg(TO_DO_POS, Store::IntToWord(1));
}

void Partition::InitAgenda(Stack *agenda) {
  // Puts all the blocks onto the agenda
  // with all their incoming edges

  DynamicArray *ba = DynamicArray::FromWordDirect(GetArg(BA_POS));
  int blockCount = Store::DirectWordToInt(GetArg(BLOCK_COUNT_POS));

  for (int i=blockCount; i--;) {
    PBlock *p = PBlock::FromWordDirect(ba->Sub(i));
    p->PutCompleteOnAgenda(i, agenda);
  }
  
}

void Partition::FollowBack(u_int block, u_int edge) {
  // for each node in this block:
  //   follow their parent links and mark their
  //   parents' blocks as to be split

  DynamicArray *ba = DynamicArray::FromWordDirect(GetArg(BA_POS));
  DynNodeArray *na = DynNodeArray::FromWord(GetArg(NA_POS));

  PBlock *pb = PBlock::FromWordDirect(ba->Sub(block));
  int start = pb->GetStart();
  int end = pb->GetEnd();

  for (int i=start; i<=end; i++) {
    PNode *pn = na->Sub(i);
    //    DynamicArray *pred = pn->GetPred();
    for (int j=pn->GetPredCount(); j--; ) {
      Tuple *tu = Tuple::FromWordDirect(pn->Pred(j));
      u_int tu_edge = Store::WordToInt(tu->Sel(0));
      u_int parent = Store::WordToInt(tu->Sel(1));
      u_int realParent = na->LookUp(parent);
      if (tu_edge == edge) {
	PNode *parentNode = na->Sub(realParent);
	int parentBlock = parentNode->GetBlock();
	if (parentBlock != -1)
	  splitBlockAtNode(parentBlock, realParent);
      }
    }
  }
}

PBlock *Partition::GetBlock(u_int blockNo) {
  DynamicArray *ba = DynamicArray::FromWordDirect(GetArg(BA_POS));
  return PBlock::FromWordDirect(ba->Sub(blockNo));
}

void Partition::ReduceGraph() {
  // Reduces the original data graph
  // by replacing every node by the first node of its block
  // => the other nodes in the same block become garbage if they're
  // not referenced from anywhere else

  DynamicArray *ba = DynamicArray::FromWordDirect(GetArg(BA_POS));
  DynNodeArray *na = DynNodeArray::FromWordDirect(GetArg(NA_POS));

  int blockCount = Store::WordToInt(GetArg(BLOCK_COUNT_POS));

  // Make root Leader of its block

  PNode *root = na->LookUpSub(0);
  PBlock *rootBlock = PBlock::FromWordDirect(ba->Sub(root->GetBlock()));
  int rootStart = rootBlock->GetStart();
  PNode *rbStart = na->Sub(rootStart);

  if (root != rbStart) {
    na->LookUpSwap(0, rbStart->GetNum());
  }
  
  for (int i=blockCount; i--;) {
    PBlock *pb = PBlock::FromWordDirect(ba->Sub(i));
    int start = pb->GetStart();
    int end = pb->GetEnd();
    PNode *leaderNode = na->Sub(start);
    word leader = leaderNode->GetNode();
    PNode *curNode;
    //    DynamicArray *pred;
    int predCount;
    for (int j=start+1; j<=end; j++) {
      curNode = na->Sub(j);
      //      pred = curNode->GetPred();
      predCount = curNode->GetPredCount();
      for (int k=predCount;k--;) {
	Tuple *tu = Tuple::FromWordDirect(curNode->Pred(k));
	int edge = Store::WordToInt(tu->Sel(0));
	int parent = Store::WordToInt(tu->Sel(1));
	// replace this node by its block's leader
	PNode *parentNode = na->LookUpSub(parent);
	Block *b = Store::WordToBlock(parentNode->GetNode());
        u_int wasMutable = b->IsMutable();
	b->ReplaceArgUnchecked(edge, leader);
      }
    }
  }

}

void Partition::Minimize() {
  // Main routine, assumes that the PartitionLoaderWorker
  // has already filled the partition

  // Initialize & sort the blocks
//   fprintf(stderr, "Init blocks.\n");
  InitBlocks();

//   fprintf(stderr, "Init agenda.\n");
  Stack *agenda = Stack::New(100);  
  InitAgenda(agenda);
  
  while (!agenda->IsEmpty()) {
    int blockNo = Store::DirectWordToInt(agenda->Pop());
    PBlock *pb = GetBlock(blockNo);
    int pbSize = pb->SizeOfIncoming();
    
    while (pb->IsOnAgenda()) {
      for (int edge=0; edge<=pbSize; edge++) {
	//	  fprintf(stderr, "Block %d, edge %d\n", blockNo, edge);
	if (pb->IsOnAgenda(edge)) {
	  pb->RemoveFromAgenda(edge);
	  FollowBack(blockNo, edge);
	  DoSplits(agenda);
	}
      }
    }
  }
  
//   fprintf(stderr, "Reduce graph.\n");
  ReduceGraph();
//   fprintf(stderr, "Minimizer done.\n");

}
