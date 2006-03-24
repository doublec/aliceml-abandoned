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
  using DynamicArray::ToWord;

  static PBlock *New(s_int start, s_int end);
  static PBlock *FromWord(word x);
  static PBlock *FromWordDirect(word x);

  void SetEnd(s_int end);
  s_int GetStart();
  s_int GetEnd();
  void SetSplitIndex(s_int i);
  s_int GetSplitIndex();
  bool HasToBeSplit();
  void IncrementIncoming(s_int edge);
  void DecrementIncoming(s_int edge);
  s_int GetIncoming(s_int edge);
  s_int SizeOfIncoming();
  void PutCompleteOnAgenda(s_int blockNo, Stack *agenda);
  void PutOnAgenda(s_int edge);
  bool IsOnAgenda();
  bool IsOnAgenda(s_int edge);
  void RemoveFromAgenda(s_int edge);
};

PBlock *PBlock::New(s_int start, s_int end) {
  Assert(start >= 0);
  Assert(end == 0 || end>= start);

  DynamicArray *p = DynamicArray::NewInit(SIZE+2,
                                          DynamicArray::INVALID_ARRAY_ELEM);
  
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

void PBlock::SetEnd(s_int end) {
  Update(END_INDEX_POS, Store::IntToWord(end));
}

s_int PBlock::GetStart() {
  return Store::DirectWordToInt(Sub(START_INDEX_POS));
}

s_int PBlock::GetEnd() {
  return Store::DirectWordToInt(Sub(END_INDEX_POS));
}

void PBlock::SetSplitIndex(s_int i) {
  Assert(i>=-1);
  Update(CUR_SPLIT_INDEX_POS, Store::IntToWord(i));
}

s_int PBlock::GetSplitIndex() {
  return Store::WordToInt(Sub(CUR_SPLIT_INDEX_POS));  
}

bool PBlock::HasToBeSplit() {
  // A block has to be split if the
  // split index was decremented at least once,
  // but not over the start of the block
  // (that is the case when all nodes were marked for splitting)

  s_int start = GetStart();
  s_int end = GetEnd();
  s_int split = GetSplitIndex();

  if (split < start) {
    SetSplitIndex(end);
    return false;
  } else {
    return (split < end);
  }
}

void PBlock::IncrementIncoming(s_int edge) {
  // maintains the array of incoming edges
  word old = Sub(SIZE+edge+1);
  if (old==DynamicArray::INVALID_ARRAY_ELEM) {
    Update(SIZE+edge+1, Store::IntToWord(1));
  } else {
    s_int oldI = Store::DirectWordToInt(old);
    // oldI<0 means this block is on agenda with this edge
    // => increment the absolute value
    Update(SIZE+edge+1, Store::IntToWord(oldI<0 ? oldI-1 : oldI+1));    
  }
}

void PBlock::DecrementIncoming(s_int edge) {
  // maintains the array of incoming edges
  word old = Sub(SIZE+edge+1);
  if (old!=DynamicArray::INVALID_ARRAY_ELEM) {
    s_int oldI = Store::WordToInt(old);
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

s_int PBlock::GetIncoming(s_int edge) {
  // returns the number of incoming edges with label "edge"
  word old = Sub(SIZE+edge+1);
  if (old==DynamicArray::INVALID_ARRAY_ELEM) {
    return 0;
  } else {
    s_int oldI = Store::DirectWordToInt(old);
    return (oldI<0 ? -oldI : oldI);
  }
}

s_int PBlock::SizeOfIncoming() {
  // the length of the incoming array is used as an upper bound
  // for the maximum label of an incoming edge
  return GetLength()-1-SIZE;
}

void PBlock::PutOnAgenda(s_int edge) {
  // marks block as being on agenda with edge "edge"
  // increments the agenda count

  // an egde is marked by making its incoming counter negative

  s_int count = Store::DirectWordToInt(Sub(SIZE+0));
  if (!IsOnAgenda(edge)) {
    word old = Sub(SIZE+edge+1);
    if (old!=DynamicArray::INVALID_ARRAY_ELEM) {
      s_int oldI = Store::WordToInt(old);
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

bool PBlock::IsOnAgenda(s_int edge) {
  // Is this block on the agenda with egde "edge"?
  word old = Sub(SIZE+edge+1);
  return (old!=DynamicArray::INVALID_ARRAY_ELEM &&
	  Store::DirectWordToInt(old) < 0);
}

void PBlock::RemoveFromAgenda(s_int edge) {
  // removes the mark created with PutOnAgenda
  // decrements the agenda count

  s_int count = Store::DirectWordToInt(Sub(SIZE+0));
  if (IsOnAgenda(edge)) {
    word old = Sub(SIZE+edge+1);
    if (old!=DynamicArray::INVALID_ARRAY_ELEM) {
      s_int oldI = Store::WordToInt(old);
      if (oldI!=0) {
	Update(SIZE+edge+1, Store::IntToWord(-oldI));
	count--;
	Update(SIZE+0, Store::IntToWord(count));
      }
    }
  }

}

void PBlock::PutCompleteOnAgenda(s_int blockNo, Stack *agenda) {
  // Pushes the block on the agenda and marks every incoming edge
  // as being on the agenda

  Assert(blockNo >= 0);

  bool flag = false;

  word incoming;
  
  for (s_int i = GetLength()-1-SIZE; i--; ) {
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
  using DynamicArray::ToWord;

  static PNode *New(word node);
  static PNode *FromWord(word x);
  static PNode *FromWordDirect(word x);
  s_int GetBlock();
  s_int GetNum();
  void SetNum(s_int num);
  void SetBlock(PBlock* newBlock, s_int newBlockNo);
  void ChangeBlock(PBlock* oldBlock, PBlock* newBlock, s_int newBlockNo);
  void AddParent(s_int egde, s_int parent);
  word GetNode();
  s_int GetPredCount();
  word Pred(s_int index);
  int Compare(PNode *pn);
};

///////////////////////
// Comparison function establishes a total order
// on nodes of the data graph
//
int PNode::compareLabels(BlockLabel vl, BlockLabel wl) {
  if (vl==wl) return 0;
  if (vl==CHUNK_LABEL)
    return -1;
  if (wl==CHUNK_LABEL)
    return 1;
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
  
  for (u_int i=vs; i--; ) {
    s_int cv = Store::WordToInt(v->GetArg(i));
    s_int cw = Store::WordToInt(w->GetArg(i));

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
  u_int size1 = c1->GetSize();
  u_int size2 = c2->GetSize();
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
  
  s_int cSize;
  
  switch(vl) {
  case GENSET_LABEL:
  case INT_MAP_LABEL:
  case CHUNK_MAP_LABEL:
  case MAP_LABEL:
  case HASHNODEARRAY_LABEL:
  case HASHNODE_LABEL:
  case QUEUE_LABEL:
  case STACK_LABEL:
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
  
  if (PointerOp::Deref(a)==PointerOp::Deref(b)) return 0;
  
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
  default:
    Assert(false);
    return -1;
  }
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

  static void swap(DynamicArray* a, DynamicArray *lua, s_int i, s_int j);
  static s_int partition(DynamicArray *a, DynamicArray *lua, s_int l, s_int r);
  static void sort(DynamicArray *a, DynamicArray *lua, s_int l, s_int r);

public:
  using Block::ToWord;

  static DynNodeArray *New(u_int initialSize);
  static DynNodeArray *FromWord(word x);
  static DynNodeArray *FromWordDirect(word x);

  s_int InitNext(PNode *pn);
  s_int GetCount();
  void Update(s_int index, PNode *pn);
  PNode *Sub(s_int index);
  PNode *LookUpSub(s_int index);
  void LookUpSwap(s_int i, s_int j);
  void Swap(s_int i, s_int j);
  s_int LookUp(s_int i);
  void Reset();
  void Sort();
};

/////////////////////////////
// Quicksort helper functions

void DynNodeArray::swap(DynamicArray* a, DynamicArray *lua, s_int i, s_int j) {
  Assert(i>=0);
  Assert(j>=0);

  word wi = a->Sub(i);
  word wj = a->Sub(j);

  s_int lui = PNode::FromWordDirect(wi)->GetNum();
  s_int luj = PNode::FromWordDirect(wj)->GetNum();

  a->Update(i, wj);
  a->Update(j, wi);
  
  // maintain permutation information
  Assert(Store::WordToInt(lua->Sub(lui))>=0);
  Assert(Store::WordToInt(lua->Sub(luj))>=0);

  lua->Update(lui, Store::IntToWord(j));
  lua->Update(luj, Store::IntToWord(i));
}

s_int DynNodeArray::partition(DynamicArray *a, DynamicArray *lua,
			      s_int l, s_int r) {
  s_int i = l-1, j = r;
  PNode *v = PNode::FromWordDirect(a->Sub(r));
  for(;;) {
    while (PNode::FromWordDirect(a->Sub(++i))->Compare(v)==-1);
    while (v->Compare(PNode::FromWordDirect(a->Sub(--j))) == -1) {
      if (j==l) break;
    }
    if (i >= j) break;
    swap(a, lua, i, j);
  }
  swap(a, lua, i, r);
  return i;
}

void DynNodeArray::sort(DynamicArray *a, DynamicArray *lua, s_int l, s_int r) {
  // a standard (not optimized) quicksort
  // from Robert Sedgewick, Algorithms in C++ 1-4
  if (r <= l) return;
  s_int i = partition(a, lua, l, r);
  sort(a, lua, l, i-1);
  sort(a, lua, i+1, r);
}


/////////////////////////////

DynNodeArray *DynNodeArray::New(u_int initialSize) {
  Block *p = Store::AllocMutableBlock(DYNNODEARRAY_LABEL, SIZE);
  
  p->InitArg(COUNT_POS, STATIC_CAST(s_int, 0));
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

s_int DynNodeArray::InitNext(PNode *pn) {
  // Initializes the next free array cell
  // with pn
  DynamicArray *na = DynamicArray::FromWordDirect(GetArg(ARRAY_POS));
  DynamicArray *lua = DynamicArray::FromWordDirect(GetArg(LUA_ARRAY_POS));
  s_int count = Store::DirectWordToInt(GetArg(COUNT_POS));
  na->Update(count, pn->ToWord());
  lua->Update(count, Store::IntToWord(count)); // no permutation
  pn->SetNum(count); // each node carries its initial index

  count++;
  ReplaceArg(COUNT_POS, Store::IntToWord(count));
  return count-1;
}

s_int DynNodeArray::GetCount() {
  return Store::DirectWordToInt(GetArg(COUNT_POS));
}

void DynNodeArray::Update(s_int index, PNode *pn) {
  Assert(index >= 0);
  DynamicArray *na = DynamicArray::FromWordDirect(GetArg(ARRAY_POS));
  na->Update(index, pn->ToWord());
}
 
PNode *DynNodeArray::Sub(s_int index) {
  Assert(index >= 0);
  DynamicArray *na = DynamicArray::FromWordDirect(GetArg(ARRAY_POS));
  return PNode::FromWordDirect(na->Sub(index));
}

PNode *DynNodeArray::LookUpSub(s_int index) {
  // Translates index according to permutation,
  // then does a sub of the translated index
  Assert(index >= 0);
  DynamicArray *lua = DynamicArray::FromWordDirect(GetArg(LUA_ARRAY_POS));
  return Sub(Store::WordToInt(lua->Sub(index)));
}

void DynNodeArray::LookUpSwap(s_int i, s_int j) {
  // Translated both i and j according to permutation,
  // then does a swap of the translated indices
  Assert(i >= 0);
  Assert(j >= 0);
  DynamicArray *lua = DynamicArray::FromWordDirect(GetArg(LUA_ARRAY_POS));
  Swap(Store::WordToInt(lua->Sub(i)),
       Store::WordToInt(lua->Sub(j)));
}

s_int DynNodeArray::LookUp(s_int i) {
  // returns the translated index
  Assert(i >= 0);
  DynamicArray *lua = DynamicArray::FromWordDirect(GetArg(LUA_ARRAY_POS));
  return Store::WordToInt(lua->Sub(i));
}

void DynNodeArray::Swap(s_int i, s_int j) {
  Assert(i >= 0);
  Assert(j >= 0);
  // wrapper for swap
  DynamicArray *na = DynamicArray::FromWordDirect(GetArg(ARRAY_POS));
  DynamicArray *lua = DynamicArray::FromWordDirect(GetArg(LUA_ARRAY_POS));
  swap(na, lua, i, j);
}

void DynNodeArray::Reset() {
  DynamicArray *na = DynamicArray::FromWordDirect(GetArg(ARRAY_POS));
  DynamicArray *lua = DynamicArray::FromWordDirect(GetArg(LUA_ARRAY_POS));
  na->Clear();
  lua->Clear();
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

s_int PNode::GetBlock() {
  // Returns the number of the block this node
  // currently belongs to
  return Store::DirectWordToInt(DynamicArray::Sub(BLOCK_POS));
}

s_int PNode::GetNum() {
  // Returns the initial index of this node in the partition array
  return Store::DirectWordToInt(DynamicArray::Sub(NUM_POS));
}

void PNode::SetNum(s_int num) {
  // Sets the initial index of this node in the partition array
  Assert(num >= 0);
  DynamicArray::Update(NUM_POS, Store::IntToWord(num));
}

void PNode::SetBlock(PBlock *newBlock, s_int newBlockNo) {
  // Sets the block no. of this node and
  // changes the block's table of incoming edges accordingly
  DynamicArray::Update(BLOCK_POS, Store::IntToWord(newBlockNo));

  s_int count = Store::WordToInt(DynamicArray::Sub(PRED_COUNT_POS));
  for (s_int i=count; i--;) {
    Tuple *tu = Tuple::FromWordDirect(Pred(i));
    s_int edge = Store::WordToInt(tu->Sel(0));
    newBlock->IncrementIncoming(edge);
  }
}

void PNode::ChangeBlock(PBlock *oldBlock, PBlock *newBlock, s_int newBlockNo) {
  // like SetBlock, but puts a node into a different block
  DynamicArray::Update(BLOCK_POS, Store::IntToWord(newBlockNo));

  s_int count = Store::WordToInt(DynamicArray::Sub(PRED_COUNT_POS));
  for (s_int i=count; i--;) {
    Tuple *tu = Tuple::FromWordDirect(Pred(i));
    s_int edge = Store::WordToInt(tu->Sel(0));
    oldBlock->DecrementIncoming(edge);
    newBlock->IncrementIncoming(edge);
  }
  
}

void PNode::AddParent(s_int edge, s_int parent) {
  // Adds node with no. parent to the array of parent
  // nodes at edge label edge
  Tuple *t = Tuple::New(2);
  t->Init(0,Store::IntToWord(edge));
  t->Init(1,Store::IntToWord(parent));

  s_int count = Store::WordToInt(DynamicArray::Sub(PRED_COUNT_POS));
  DynamicArray::Update(SIZE+count, t->ToWord());
  count++;
  DynamicArray::Update(PRED_COUNT_POS, Store::IntToWord(count));
}

word PNode::GetNode() {
  return DynamicArray::Sub(NODE_POS);
}

word PNode::Pred(s_int i) {
  Assert(i >= 0);
  // Returns the predecessor at index i
  return DynamicArray::Sub(SIZE+i);
}

s_int PNode::GetPredCount() {
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
  b->InitArg(BLOCK_COUNT_POS, STATIC_CAST(s_int, 0));
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

s_int Partition::InsertNode(word node) {
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

  s_int count = na->GetCount();
  s_int blockCount = 0;
  
  na->Sort();

  PBlock *newBlock = PBlock::New(0,0);
  PNode *lastItem = na->Sub(0);
  lastItem->SetBlock(newBlock, blockCount);
  PNode *curItem;
  for (s_int i=1; i<count; i++) {
    curItem = na->Sub(i);
    Assert(curItem->Compare(lastItem) != -1);
    if(curItem->Compare(lastItem) == 1) {
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

bool Partition::splitBlockAtNode(s_int block, s_int theNodeIndex) {
  // Marks block "block" as to be split at node with index nodeIndex
  Assert(block >= 0);

  DynamicArray *ba = DynamicArray::FromWordDirect(GetArg(BA_POS));
  PBlock *b = PBlock::FromWordDirect(ba->Sub(block));
  DynNodeArray *na = DynNodeArray::FromWordDirect(GetArg(NA_POS));

  s_int nodeIndex = na->LookUp(theNodeIndex);
  
  s_int si = b->GetSplitIndex();
  s_int end = b->GetEnd();

  if (end - b->GetStart() == 0) {
    // Don't split singleton blocks
    return false;
  }

  Assert(nodeIndex <= end);
  Assert(si <= end);

  bool result=false;

  if (si == end) {
    // push block on toDo, because it has not been split yet
    // in this iteration
    word head = GetArg(TO_DO_POS);
    Block *tv = Store::AllocBlock(MIN_DATA_LABEL,2);
    tv->InitArg(0, Store::IntToWord(block));
    tv->InitArg(1, head);
    ReplaceArg(TO_DO_POS, tv->ToWord());
  }  
  if (si >= nodeIndex) {
    // only do the actual split if the split index is to the
    // right of the node!
    na->Swap(si, nodeIndex);
    b->SetSplitIndex(si-1);
    result=true;
  }
  return result;
}

void Partition::AddParent(s_int nodeIndex, s_int edge, s_int parent) {
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
  s_int blockCount = Store::DirectWordToInt(GetArg(BLOCK_COUNT_POS));
  word toDo = GetArg(TO_DO_POS);
  while (Store::WordToInt(toDo)==INVALID_INT) {
    Block *tv = Store::DirectWordToBlock(toDo);
    
    s_int blockNo = Store::WordToInt(tv->GetArg(0));
    Assert(blockNo >= 0);
    PBlock *pb = PBlock::FromWordDirect(ba->Sub(blockNo));
    PBlock *newBlock;
    if (pb->HasToBeSplit()) {
      s_int newEnd = pb->GetEnd();
      s_int newStart = pb->GetSplitIndex()+1;
      pb->SetEnd(newStart-1);
      pb->SetSplitIndex(newStart-1);
      newBlock = PBlock::New(newStart, newEnd);
      ba->Update(blockCount, newBlock->ToWord());
      for (s_int i=newStart; i<=newEnd; i++) {
	PNode *node = na->Sub(i);
	node->ChangeBlock(pb, newBlock, blockCount);
      }

      s_int max = pb->SizeOfIncoming();
      for(s_int i=0; i<=max; i++) {
	
	s_int pbInc = pb->GetIncoming(i);
	s_int nbInc = newBlock->GetIncoming(i);
	
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
  s_int blockCount = Store::DirectWordToInt(GetArg(BLOCK_COUNT_POS));

  for (s_int i=blockCount; i--;) {
    PBlock *p = PBlock::FromWordDirect(ba->Sub(i));
    p->PutCompleteOnAgenda(i, agenda);
  }
  
}

void Partition::FollowBack(s_int block, s_int edge) {
  // for each node in this block:
  //   follow their parent links and mark their
  //   parents' blocks as to be split
  Assert(block >= 0);
  Assert(edge >= 0);

  DynamicArray *ba = DynamicArray::FromWordDirect(GetArg(BA_POS));
  DynNodeArray *na = DynNodeArray::FromWord(GetArg(NA_POS));

  PBlock *pb = PBlock::FromWordDirect(ba->Sub(block));
  s_int start = pb->GetStart();
  s_int end = pb->GetEnd();

  // Collect splits that happen in the current block
  // to prevent items in this block from being swapped under
  // our feet
  word ownSplit = Store::IntToWord(1); // nil

  for (s_int i=start; i<=end; i++) {
    PNode *pn = na->Sub(i);
    for (s_int j=pn->GetPredCount(); j--; ) {
      Tuple *tu = Tuple::FromWordDirect(pn->Pred(j));
      s_int tu_edge = Store::WordToInt(tu->Sel(0));
      if (tu_edge == edge) {
	s_int parent = Store::WordToInt(tu->Sel(1));
	s_int realParent = na->LookUp(parent);
	PNode *parentNode = na->Sub(realParent);
	s_int parentBlock = parentNode->GetBlock();
	Assert(parentBlock != -1);
	if (parentBlock == block) {
	  // postpone splitting of this block
	  Tuple *cons = Tuple::New(2);
	  cons->Init(0, Store::IntToWord(parent));
	  cons->Init(1, ownSplit);
	  ownSplit = cons->ToWord();
	} else {
	  splitBlockAtNode(parentBlock, parent);
	}
      }
    }
  }

  // if this block was split, perform the postponed split operations
  while (Store::WordToInt(ownSplit) == INVALID_INT) {
    Tuple *cons = Tuple::FromWordDirect(ownSplit);
    splitBlockAtNode(block, Store::DirectWordToInt(cons->Sel(0)));
    ownSplit = cons->Sel(1);
  }

}

PBlock *Partition::GetBlock(s_int blockNo) {
  Assert(blockNo >= 0);
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

  s_int blockCount = Store::WordToInt(GetArg(BLOCK_COUNT_POS));

  // Make root Leader of its block

  PNode *root = na->LookUpSub(0);
  PBlock *rootBlock = PBlock::FromWordDirect(ba->Sub(root->GetBlock()));
  s_int rootStart = rootBlock->GetStart();
  PNode *rbStart = na->Sub(rootStart);

  if (root != rbStart) {
    na->LookUpSwap(0, rbStart->GetNum());
  }

  for (s_int i=blockCount; i--;) {
    PBlock *pb = PBlock::FromWordDirect(ba->Sub(i));
    s_int start = pb->GetStart();
    s_int end = pb->GetEnd();
    Assert(start <= end);
    PNode *leaderNode = na->Sub(start);
    word leader = leaderNode->GetNode();
    PNode *curNode;
    s_int predCount;

    for (s_int j=start+1; j<=end; j++) {
      curNode = na->Sub(j);
      predCount = curNode->GetPredCount();
      for (s_int k=predCount;k--;) {
	Tuple *tu = Tuple::FromWordDirect(curNode->Pred(k));
	s_int edge = Store::WordToInt(tu->Sel(0));
	s_int parent = Store::WordToInt(tu->Sel(1));
	// replace this node by its block's leader
	PNode *parentNode = na->LookUpSub(parent);
	Block *b = Store::WordToBlock(parentNode->GetNode());
	b->ReplaceArgUnchecked(edge, leader);
      }
    }
  }

}

void Partition::ResetNodeArray() {
  DynNodeArray *na = DynNodeArray::FromWordDirect(GetArg(NA_POS));
  na->Reset();
}

void Partition::Minimize() {
  // Main routine, assumes that the PartitionLoaderWorker
  // has already filled the partition

  // Initialize & sort the blocks
  DynNodeArray *na = DynNodeArray::FromWordDirect(GetArg(NA_POS));
  if (na->GetCount() < 2)
    return;

  InitBlocks();

  Stack *agenda = Stack::New(100);  
  InitAgenda(agenda);
  
  while (!agenda->IsEmpty()) {
    s_int blockNo = Store::DirectWordToInt(agenda->Pop());
    PBlock *pb = GetBlock(blockNo);
    s_int pbSize = pb->SizeOfIncoming();

    while (pb->IsOnAgenda()) {
      for (s_int edge=0; edge<=pbSize; edge++) {
	if (pb->IsOnAgenda(edge)) {
	  pb->RemoveFromAgenda(edge);
	  FollowBack(blockNo, edge);
	  DoSplits(agenda);
	}
      }
    }
  }
  
  ReduceGraph();
}
