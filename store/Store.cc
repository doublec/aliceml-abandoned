//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#include <cstdlib>
#include <cstring>
#include <cstdio>

#if defined(INTERFACE)
#pragma implementation "store/StatusWord.hh"
#pragma implementation "store/HeaderOp.hh"
#pragma implementation "store/PointerOp.hh"
#pragma implementation "store/Store.hh"
#pragma implementation "store/GCHelper.hh"
#pragma implementation "store/Value.hh"
#pragma implementation "store/Set.hh"
#endif

#include "store/StatusWord.hh"
#include "store/Store.hh"
#include "store/Heap.hh"
#include "store/GCHelper.hh"
#include "store/Map.hh"
#include "store/WeakMap.hh"
#include "store/MapNode.hh"

#if defined(STORE_PROFILE) || defined(STORE_NOGCBENCH)
#include "generic/Time.hh"
#endif

// Using Set in a anonymous namespace prevents
// class Set from appearing outside
namespace {
#include "store/Set.hh"

  Set *intgenSet;
  Set *wkDictSet;
  Set *finSet;
};

// Status Word
u_int StatusWord::status;

//
// Class Fields and Global Vars
//
Heap Store::roots[STORE_GENERATION_NUM];
u_int Store::memFree;
u_int Store::memTolerance;

#if defined(STORE_PROFILE)
u_int Store::totalMem  = 0;
u_int Store::gcLiveMem = 0;
#endif

#if defined(STORE_PROFILE) || defined(STORE_NOGCBENCH)
double Store::sum_t;
#endif


//
// Method Implementations
//
inline Block *Store::CloneBlock(Block *p, const u_int gen) {
  u_int size  = p->GetSize();
  Block *newp = (Block *) Store::Alloc(gen, p->GetLabel(), size);
  std::memcpy(newp->GetBase(), p->GetBase(), size * sizeof(u_int));
  GCHelper::MarkMoved(p, newp);
  return newp;
}

inline word Store::ForwardBlock(word p, const u_int gen) {
  AssertStore(PointerOp::IsInt(p) == 0);
  Block *sp = PointerOp::RemoveTag(p);
  // order is important because moving ptr overwrites gen assignment
  if (GCHelper::AlreadyMoved(sp)) {
    sp = GCHelper::GetForwardPtr(sp);
    p  = PointerOp::EncodeTag(sp, PointerOp::DecodeTag(p));
  }
  else if (HeaderOp::DecodeGeneration(sp) < gen) {
    sp = CloneBlock(sp, gen);
    p  = PointerOp::EncodeTag(sp, PointerOp::DecodeTag(p));
  }
  return p;
}

#define FORWARD(p, gen) \
  (PointerOp::IsInt(p) ? p : Store::ForwardBlock(p, gen))

inline void Store::CheneyScan(HeapChunk *chunk, char *scan, const u_int gen) {
  goto have_scan;
  do {
    scan = chunk->GetBase();
  have_scan:
    while (scan < chunk->GetTop()) {
      Block *p   = (Block *) scan;
      u_int size = p->GetSize();
      // Move scan pointer ahead
      scan += SIZEOF_BLOCK(size);
      // CHUNK_LABEL, WEAK_MAP_LABEL, DYNAMIC_LABEL are the largest labels
      BlockLabel l = p->GetLabel();
      if (l < CHUNK_LABEL) {
      scan:
	for (u_int i = size; i--;) {
	  word item = GCHelper::Deref(p->GetArg(i));
	  p->InitArg(i, FORWARD(item, gen));
	}
      }
      else if (l == DYNAMIC_LABEL) {
    	size = ((DynamicBlock *) p)->GetScanSize();
//  	// Purge non-active size
//  	u_int scanSize = ((DynamicBlock *) p)->GetScanSize();
//  	for (u_int i = size - 1; i >= scanSize; i--)
//  	  p->InitArg(i, Store::IntToWord(0));
//  	size = scanSize;
	goto scan;
      }
    }
    chunk = chunk->GetPrev();
  } while (chunk != NULL);
}

// to be done: more efficient solution
static int IsInFromSpace(Heap *roots, char *p) {
  for (u_int i = STORE_GENERATION_NUM - 1; i--;) {
    HeapChunk *chunk = roots[i].GetChain();
    while (chunk != NULL) {
      if ((chunk->GetBase() <= p) && (p <= chunk->GetTop()))
	return 1;
      chunk = chunk->GetNext();
    }
  }
  return 0;
}

// Finalization needs a second cheney scan. This yields the problem
// that we cannot distinguish the from and the to space for the oldest
// generation using our generational match. Instead we do explicit
// checking which is expensive; therefore use finalization with care.
//
// to be done: more efficient and better code reusing solution
inline void Store::FinalizeCheneyScan(HeapChunk *chunk, char *scan) {
  goto have_scan;
  do {
    scan = chunk->GetBase();
  have_scan:
    while (scan < chunk->GetTop()) {
      Block *p   = (Block *) scan;
      u_int size = p->GetSize();
      // Move scan pointer ahead
      scan += SIZEOF_BLOCK(size);
      // CHUNK_LABEL, WEAK_MAP_LABEL and DYNAMIC_LABEL are the largest
      BlockLabel l = p->GetLabel();
      if (l < CHUNK_LABEL) {
      scan:
	for (u_int i = size; i--;) {
	  word item = p->GetArg(i);
	  item = GCHelper::Deref(item);
	  if (PointerOp::IsInt(item) == 0) {
	    Block *sp = PointerOp::RemoveTag(item);
	    if (GCHelper::AlreadyMoved(sp)) {
	      sp   = GCHelper::GetForwardPtr(sp);
	      item = PointerOp::EncodeTag(sp, PointerOp::DecodeTag(item)); 
	    }
	    else if (IsInFromSpace(roots, (char *) sp)) {
	      sp   = CloneBlock(sp, STORE_GENERATION_NUM - 1);
	      item = PointerOp::EncodeTag(sp, PointerOp::DecodeTag(item)); 
	    }
	  }
	  p->InitArg(i, item);
	}
      }
      else if (l == DYNAMIC_LABEL) {
	size = ((DynamicBlock *) p)->GetScanSize();
	goto scan;
      }
    }
    chunk = chunk->GetPrev();
  } while (chunk != NULL);
}

void Store::InitStore(u_int mem_max[STORE_GENERATION_NUM],
		      u_int mem_free, u_int mem_tolerance) {
  StatusWord::Init();
  for (u_int i = STORE_GENERATION_NUM; i--;) {
    char *p = (char *) roots + sizeof(Heap) * i;
    new(p) Heap(STORE_MEMCHUNK_SIZE, mem_max[i]);
  }
  Store::memFree      = mem_free;
  Store::memTolerance = mem_tolerance;
  // Alloc Intgen- and WKDict-Set
  intgenSet = Set::New(STORE_INTGENSET_SIZE);
  wkDictSet = Set::New(STORE_WKDICTSET_SIZE);
  finSet    = Set::New(1024); // to be done
  // Enable BlockHashTables
  Map::Init();
#if defined(STORE_PROFILE)
  Time::Init();
  totalMem = 0;
#endif
}

void Store::CloseStore() {
  for (int i = (STORE_GENERATION_NUM - 1); i--;) {
    Heap *heap = roots + sizeof(Heap) * i;
    delete heap;
  }
}

void Store::AddToIntgenSet(Block *v) {
  HeaderOp::SetChildishFlag(v);
  intgenSet = intgenSet->Add(v->ToWord());
}

void Store::RegisterWeakDict(WeakMap *v) {
  wkDictSet = wkDictSet->Add(v->ToWord());
}

void Store::HandleInterGenerationalPointers(const u_int gen) {
#if defined(STORE_GC_DEBUG)
  std::fprintf(stderr, "initial intgen_size is %d\n", intgenSet->GetSize());
  std::fflush(stderr);
#endif
  Set *intgen_set = intgenSet;
  u_int size      = intgen_set->GetSize();
  intgen_set->Clear();
  // Traverse intgen_set entries (set reusage enforces upward traversal)
  for (u_int i = 0; i < size; i++) {
    word wEntry = GCHelper::Deref(intgen_set->GetArgUnchecked(i));
    AssertStore(PointerOp::IsInt(wEntry) == 0);
    Block *entry = PointerOp::RemoveTag(wEntry);
    // entry is no longer old but alive
    if (GCHelper::AlreadyMoved(entry))
      HeaderOp::ClearChildishFlag(GCHelper::GetForwardPtr(entry));
    else {
      u_int entryGen = HeaderOp::DecodeGeneration(entry);
      // entry is still old
      if (entryGen > gen) {
	// Traverse entry for young references
	bool foundYoungPtrs = false;
	u_int entrySize =
	  ((entry->GetLabel() == DYNAMIC_LABEL) ?
	   ((DynamicBlock *) entry)->GetScanSize() : entry->GetSize());
	for (u_int k = entrySize; k--;) {
	  word wItem = GCHelper::Deref(entry->GetArg(k));
	  if (PointerOp::IsInt(wItem))
	    entry->InitArg(k, wItem);
	  else {
	    wItem = ForwardBlock(wItem, gen + 1);
	    entry->InitArg(k, wItem);
	    Block *p = PointerOp::RemoveTag(wItem);
	    if (HeaderOp::DecodeGeneration(p) < entryGen)
	      foundYoungPtrs = true;
	  }
	}
	if (foundYoungPtrs)
	  intgen_set->AddUnchecked(wEntry);
	else
	  HeaderOp::ClearChildishFlag(entry);
      }
      // entry is garbage
    }
  }
#if defined(STORE_GC_DEBUG)
  std::fprintf(stderr, "new_intgen_size is %d\n", intgenSet->GetSize());
  std::fflush(stderr);
#endif
}

// Value is non Dict or empty Dict ?
#define FINALIZE_PERMITTED(p) \
  ((p->GetLabel() != WEAK_MAP_LABEL) || \
   ((p->GetLabel() == WEAK_MAP_LABEL) && ((WeakMap *) p)->GetCounter() == 0))

void Store::HandleWeakDictionaries(const u_int gen) {
#if defined(STORE_DEBUG)
  std::fprintf(stderr, "initial weakdict_size is %d\n", wkDictSet->GetSize()); 
  std::fflush(stderr);
#endif
  // Clone wkDictSet and initialize finSet
  Set *wkdict_set = wkDictSet;
  u_int size      = wkdict_set->GetSize();
  Set *db_set     = wkdict_set->Export(gen);
  wkdict_set->Clear();
  finSet->Clear();
  // Phase One: Forward all Dictionaries but not the contents
  for (u_int i = size; i--;) {
    word dict = db_set->GetArg(i);
    Block *dp = Store::DirectWordToBlock(dict);
    word ndict;
    // Dictionary has been reached from Root Set and must kept alive
    if (GCHelper::AlreadyMoved(dp)) {
      ndict = PointerOp::EncodeTag(GCHelper::GetForwardPtr(dp),
				   PointerOp::DecodeTag(dict));
      wkdict_set->AddUnchecked(ndict);
    }
    // Dictionary might be finalized
    else if (HeaderOp::DecodeGeneration(dp) < gen) {
      Block *newp = CloneBlock(dp, gen);
      ndict = PointerOp::EncodeTag(newp, PointerOp::DecodeTag(dict));
      // Finalize only empty dict
      if (((WeakMap *) newp)->GetCounter() == 0) {
	word handler = ((WeakMap *) newp)->GetHandler();
	finSet = finSet->Add(ndict, gen);
	finSet = finSet->Add(handler, gen);
      }
      // Keep it alive (thanks to Denys for pointing that out)
      else
	wkdict_set->AddUnchecked(ndict);
    }
    // Can't decide whether it was reached or not; must assume yes.
    else {
      ndict = dict;
      wkdict_set->AddUnchecked(ndict);
    }
    // Keep Dict References complete for working
    db_set->InitArg(i, ndict);
    // Now Process DictTable and its MapNodes but NOT the content
    WeakMap *p = WeakMap::FromWordDirect(ndict);
    word arr   = ForwardBlock(p->GetTable()->ToWord(), gen);
    p->SetTable(arr);
    Block *table = Store::DirectWordToBlock(arr);
    for (u_int k = table->GetSize(); k--;) {
      word nodes = FORWARD(table->GetArg(k), gen);
      table->InitArg(k, nodes);
      while (nodes != Store::IntToWord(0)) {
	MapNode *node = MapNode::FromWordDirect(nodes);
	nodes = FORWARD(node->GetNext(), gen);
	node->SetNextDirect(nodes);
      }
    }
  }
  // Phase Two: Forward Dictionary Contents and record Finalize Candiates
  HeapChunk *chunk = roots[gen].GetChain();
  char *scan       = chunk->GetTop();
  for (u_int i = size; i--;) {
    WeakMap *dict = WeakMap::FromWordDirect(db_set->GetArg(i));
    word handler  = dict->GetHandler();
    Block *table  = dict->GetTable();
    for (u_int k = table->GetSize(); k--;) {
      word nodes = table->GetArg(k);
      word prev  = Store::IntToWord(0);
      while (nodes != Store::IntToWord(0)) {
	MapNode *node = MapNode::FromWordDirect(nodes);
	bool deleted  = false;
	word val      = GCHelper::Deref(node->GetValue());
	// Immediately finalize integer values
	if (PointerOp::IsInt(val)) {
	  dict->RemoveEntry(k, prev, node);
	  deleted = true;
	  finSet = finSet->Add(val, gen);
	  finSet = finSet->Add(handler, gen);
	} else {
	  Block *valp = PointerOp::RemoveTag(val);
	  // Value has been reached; keep it
	  if (GCHelper::AlreadyMoved(valp)) {
	    node->SetValue(PointerOp::EncodeTag(GCHelper::GetForwardPtr(valp),
						PointerOp::DecodeTag(val)));
	  }
	  // Value might be finalized
	  else if (HeaderOp::DecodeGeneration(valp) < gen) {
	    word fVal = ForwardBlock(val, gen);
	    if (FINALIZE_PERMITTED(valp)) {
	      dict->RemoveEntry(k, prev, node);
	      deleted = true;
	      finSet = finSet->Add(fVal, gen);
	      finSet = finSet->Add(handler, gen);
	    }
	    // No, forward and save it again
	    else
	      node->SetValue(fVal);
	  }
	  // Unable to decide; leave value untouched but derefed
	  else
	    node->SetValue(val);
	}
	if (deleted)
	  deleted = false;
	else
	  prev = nodes;
	nodes = node->GetNext();
      }
    }
  }
  // Now successivly forward the finalized tree
#if defined(STORE_DEBUG)
  std::fprintf(stderr, "HandleWeakDictionaries: performing cheney scan\n");
  std::fflush(stderr);
#endif
  if (gen == (STORE_GENERATION_NUM - 1))
    Store::FinalizeCheneyScan(chunk, scan);
  else
    Store::CheneyScan(chunk, scan, gen);
#if defined(STORE_DEBUG)
  std::fprintf(stderr, "new_weakdict_size is %d\n", wkDictSet->GetSize());
  std::fflush(stderr);
#endif
}

#undef min
static inline u_int min(u_int a, u_int b) {
  return ((a <= b) ? a : b);
}
#undef max
static inline u_int max(u_int a, u_int b) {
  return ((a >= b) ? a : b);
}

inline void Store::GC(word &root, const u_int gen) {
  // Obtain scan start
  HeapChunk *chunk = roots[gen].GetChain();
  char *scan       = chunk->GetTop();
  // Forward root set
  root = CloneBlock(Store::DirectWordToBlock(root), gen)->ToWord();
  // Scanning chunks (root set amount)
  Store::CheneyScan(chunk, scan, gen);
  // Obtain new scan start (to scan intgen set stuff)
  chunk = roots[gen].GetChain();
  scan  = chunk->GetTop();
  // Handle InterGenerational Pointers
  Store::HandleInterGenerationalPointers(gen - 1);
  // Scan chunks (intgen set amount)
  Store::CheneyScan(chunk, scan, gen);
  // Handle Weak Dictionaries, if any (performs scanning itself)
  if (wkDictSet->GetSize() != 0)
    Store::HandleWeakDictionaries(gen);
 // Rehash BlockHashTable Contents
  Map::RehashAll(gen);
}

#if defined(STORE_DEBUG)
static u_int gcCounter = 0;
#endif

void Store::DoGCWithoutFinalize(word &root) {
#if defined(STORE_DEBUG)
  std::fprintf(stderr, "GC Nb %d...\n", gcCounter++);
  std::fflush(stderr);
#endif
#if defined(STORE_GC_DEBUG)
  std::fprintf(stderr, "Pre-GC checking...");
  std::fflush(stderr);
  VerifyGC(root);
  std::fprintf(stderr, "passed.\n");
  std::fflush(stderr);
#endif
#if defined(STORE_PROFILE) || defined(STORE_NOGCBENCH)
  double start_t, end_t;
  start_t = Time::GetElapsedMicroseconds();
#endif
  // Determine GC Range
  u_int gen = (STORE_GENERATION_NUM - 2);
  while ((gen > 0) && (roots[gen].GetSize()) <= roots[gen].GetLimit())
    gen--;
#if defined(STORE_DEBUG)
  std::fprintf(stderr, "GCing up to %d...\n", gen);
  std::fprintf(stderr, "FinSet = %p is %d/%d\n", finSet,
	       finSet->GetSize(), ((Block *) finSet)->GetSize());
  std::fflush(stderr);
#endif
  switch (gen) {
  case STORE_GEN_YOUNGEST:
    GC(root, STORE_GEN_YOUNGEST + 1); break;
  case 1:
    GC(root, 2); break;
  case STORE_GEN_OLDEST:
    {
      // Major collection
      intgenSet = (Set *) CloneBlock((Block *) intgenSet, STORE_GEN_OLDEST + 1);
      wkDictSet = (Set *) CloneBlock((Block *) wkDictSet, STORE_GEN_OLDEST + 1);
      finSet    = (Set *) CloneBlock((Block *) finSet, STORE_GEN_OLDEST + 1);
      GC(root, STORE_GEN_OLDEST + 1);
      // Calc limits for next major GC
      // to be done: find appropriate heuristics
      u_int usage = roots[STORE_GEN_OLDEST + 1].GetSize();
      u_int wanted = max(usage * 2, 35 * 1024 * 1024);

      s_int block_size = STORE_MEMCHUNK_SIZE;
      s_int block_dist = wanted % block_size;
      if (block_dist > 0)
	block_dist = block_size - block_dist;
      wanted += min(block_dist, ((wanted * memTolerance) / 100));
      roots[STORE_GEN_OLDEST + 1].SetLimit(wanted);
    }
    break;
  default:
    GC(root, gen + 1);
  }
  // Release Collected regions
  for (u_int i = gen + 1; i--;)
    roots[i].Shrink();
  // Switch Semispaces
  if (gen == STORE_GEN_OLDEST) {
    Heap tmp = roots[STORE_GEN_OLDEST];
    roots[STORE_GEN_OLDEST]     = roots[STORE_GEN_OLDEST + 1];
    roots[STORE_GEN_OLDEST + 1] = tmp;
  }
  // Clear GC Flag
  StatusWord::ClearStatus(GCStatus());
  // TODO: Reenable gc/no gc benchmarking (needs to include finalization time)
#if defined(STORE_PROFILE) || defined(STORE_NOGCBENCH)
  end_t  = Time::GetElapsedMicroseconds();
  sum_t += (end_t - start_t);
#endif
#if defined(STORE_PROFILE)
  gcLiveMem += (GetMemUsage(roots[hdrGen]) - memUsage);
#endif
#if defined(STORE_GC_DEBUG)
  std::fprintf(stderr, "Post-GC checking...");
  std::fflush(stderr);
  VerifyGC(root);
  std::fprintf(stderr, "passed.\n");
  std::fflush(stderr);
#endif
#if defined(STORE_DEBUG)
  std::fprintf(stderr, "GC done.\n");
  std::fflush(stderr);
#endif
}

void Store::DoFinalize() {
  Set *set = finSet;
  for (u_int i = set->GetSize(); i--;) {
    Finalization *handler =
      (Finalization *) Store::DirectWordToUnmanagedPointer(set->GetArg(i--));
    handler->Finalize(set->GetArg(i));
  }
}

void Store::DoGC(word &root) {
  DoGCWithoutFinalize(root);
  DoFinalize();
}

void Store::SetGCParams(u_int mem_free, u_int mem_tolerance) {
  Store::memFree      = mem_free;
  Store::memTolerance = mem_tolerance;
}

#if defined(STORE_GC_DEBUG)
#define MAX_ITERATION_STEPS 40000000

static u_int path[MAX_ITERATION_STEPS];
static u_int depth = 0;

static Block *elems[MAX_ITERATION_STEPS];
static u_int size = 0;

static word rootWord;

static void InitVerify() {
  for (u_int i = MAX_ITERATION_STEPS; i--;)
    elems[i] = NULL;
  size  = 0;
  depth = 0;
}

static const char *LabelToString(u_int l) {
  switch ((BlockLabel) l) {
  case HOLE_LABEL:
    return "HOLE";
  case FUTURE_LABEL:
    return "FUTURE";
  case REF_LABEL:
    return "REF";
  case CANCELLED_LABEL:
    return "CANCELLED";
  case BYNEED_LABEL:
    return "BYNEED";
  case MAP_LABEL:
    return "MAP";
  case INT_MAP_LABEL:
    return "INTMAP";
  case CHUNK_MAP_LABEL:
    return "CHUNKMAP";
  case WEAK_MAP_LABEL:
    return "WEAKMAP";
  case QUEUE_LABEL:
    return "QUEUE";
  case STACK_LABEL:
    return "STACK";
  case THREAD_LABEL:
    return "THREAD";
  case TUPLE_LABEL:
    return "TUPLE";
  case ARGS_LABEL:
    return "ARGS";
  case CLOSURE_LABEL:
    return "CLOSURE";
  case CONCRETE_LABEL:
    return "CONCRETE";
  case DYNAMIC_LABEL:
    return "DYNAMIC_BLOCK";
  default:
    return NULL;
  }
}
static void PrintFailurePath() {
  Block *level = Store::WordToBlock(rootWord);
  for (u_int i = 0; i < depth; i++) {
    u_int branch  = path[i];
    u_int label   = level->GetLabel();
    u_int sz      = level->GetSize();
    const char *s = LabelToString(label);
    if (s == NULL)
      std::fprintf(stderr, "Branch %d/%d in %x, type %d\n",
		   branch, sz, level, label);
    else if (label == DYNAMIC_LABEL) {
      u_int aSz = ((DynamicBlock *) level)->GetActiveSize();
      std::fprintf(stderr, "Branch %d/%d (%d) in %x, type %s\n",
		   branch, aSz, sz, level, s);
    }
    else
      std::fprintf(stderr, "Branch %d/%d in %x, type %s\n",
		   branch, sz, level, s);
    level = Store::WordToBlock(level->GetArg(branch));
  }
}

static void PrintLocatePath() {
  Block *level = Store::WordToBlock(rootWord);
  for (u_int i = 0; i < depth; i++) {
    u_int branch  = path[i];
    std::fprintf(stderr, "%d/", branch);
  }
  std::fprintf(stderr, "\n");
}

static bool IsAlive(Heap *roots, char *p,
		    const u_int maxGen = STORE_GENERATION_NUM - 1) {
  if (p != NULL) {
    for (u_int i = 0; i < maxGen; i++) {
      HeapChunk *chunk = roots[i].GetChain();
      while (chunk != NULL) {
	if (p >= chunk->GetBase() && (p < chunk->GetTop()))
	  return true;
	chunk = chunk->GetNext();
      }
    }
  }
  return false;
}

static void Verify(Heap *roots, word x) {
  AssertStore(depth < MAX_ITERATION_STEPS);
  AssertStore(size < MAX_ITERATION_STEPS);
  if (size >= MAX_ITERATION_STEPS) {
    std::fprintf(stderr, "Verify: size exceeded MAX_ITERATION_STEPS\n");
    std::fflush(stderr);
    AssertStore(0);
  }
  if (depth >= MAX_ITERATION_STEPS) {
    std::fprintf(stderr, "Verify: depth exceeded MAX_ITERATION_STEPS\n");
    std::fflush(stderr);
    AssertStore(0);
  }
  if (PointerOp::IsInt(x)) {
    AssertStore(PointerOp::DecodeInt(x) != INVALID_INT);
  } else {
    Block *p = PointerOp::RemoveTag(x);
    if (p == NULL) {
      std::fprintf(stderr, "Verify: null pointer encountered: %x --> %x\n",
	      (word) p, x);
      PrintFailurePath();
      AssertStore(0);
    }
    else if (!IsAlive(roots, (char *) p, STORE_GENERATION_NUM)) {
      std::fprintf(stderr, "Verify: found stale pointer %x (word=%x)\n",
		   p, x);
      PrintFailurePath();
      AssertStore(0);
    }
    else if (GCHelper::AlreadyMoved(p)) {
      std::fprintf(stderr, "Verify: found forward pointer %x (%x)\n",
		   (word) p, x);
      PrintFailurePath();
      AssertStore(0);
    }
    else if (!IsAlive(roots, (char *) p)) {
      std::fprintf(stderr, "Verify: found non-alive pointer %x (word=%x)\n",
		   p, x);
      PrintFailurePath();
      AssertStore(0);
    }
    u_int key = ((u_int) p % MAX_ITERATION_STEPS);
    if (elems[key] == NULL) {
      elems[key] = p;
      size++;
    }
    else {
      while ((elems[key] != NULL) && (key < MAX_ITERATION_STEPS)) {
	if (elems[key] == p)
	  return;
	else
	  key++;
      }
      AssertStore(key < MAX_ITERATION_STEPS);
      elems[key] = p;
      size++;
    }
    BlockLabel l = p->GetLabel();
    if (l != CHUNK_LABEL) {
      u_int size;
      if (l == DYNAMIC_LABEL)
	size = ((DynamicBlock *) p)->GetActiveSize();
      else
	size = p->GetSize();
      for (u_int i = size; i--;) {
  	word item = p->GetArg(i);
  	path[depth++] = i;
  	Verify(roots, item);
  	depth--;
      }
    }
  }
}

static void Locate(word x, word v) {
  AssertStore(depth < MAX_ITERATION_STEPS);
  AssertStore(size < MAX_ITERATION_STEPS);
  if (!PointerOp::IsInt(x)) {
    Block *p = PointerOp::RemoveTag(x);
    u_int key = ((u_int) p % MAX_ITERATION_STEPS);
    if (elems[key] == NULL) {
      elems[key] = p;
      size++;
    }
    else {
      while ((elems[key] != NULL) && (key < MAX_ITERATION_STEPS)) {
	if (elems[key] == p)
	  return;
	else
	  key++;
      }
      AssertStore(key < MAX_ITERATION_STEPS);
      elems[key] = p;
      size++;
    }
    BlockLabel l = p->GetLabel();
    if (l != CHUNK_LABEL) {
      u_int size = p->GetSize();
      for (u_int i = size; i--;) {
  	word item = p->GetArg(i);
  	path[depth++] = i;
	if (item == v) {
	  PrintLocatePath();
	}
  	Locate(item, v);
  	depth--;
      }
    }
  }
}

void Store::VerifyGC(word root) {
  InitVerify();
  rootWord = root;
  Verify(roots, root);
  InitVerify();
  rootWord = Map::mapLs;
  Verify(roots, Map::mapLs);
}
#endif

void Store::MemStat() {
  std::fprintf(stderr, "---\n");
  for (u_int i = 0; i < STORE_GENERATION_NUM - 1; i++) {
    HeapChunk *chunk = roots[i].GetChain();
    u_int used      = 0;
    u_int total     = 0;
    while (chunk != NULL) {
      char *base = chunk->GetBase();
      used  += (u_int) (chunk->GetTop() - base);
      total += (u_int) (chunk->GetMax() - base);
      chunk = chunk->GetNext();
    }
    std::fprintf(stderr, "G%d --> Used: %8u; Total: %8u; GC-Limit: %8u.\n",
		 i, used, total, roots[i].GetLimit());
  }
  std::fprintf(stderr, "---\n");
}

void Store::JITReplaceArg(u_int i, Block *p, word v) {
  AssertStore(v != (word) 0);
  if (!PointerOp::IsInt(v)) {
    u_int valgen = HeaderOp::DecodeGeneration(PointerOp::RemoveTag(v));
    u_int mygen  = HeaderOp::DecodeGeneration(p);
    if ((valgen < mygen) && (!HeaderOp::IsChildish(p)))
      Store::AddToIntgenSet(p);
  }
  p->InitArg(i, v);
}

#if defined(STORE_PROFILE) || defined(STORE_NOGCBENCH)
void Store::ResetTime() {
  sum_t = 0;
#if defined(STORE_PROFILE)
  totalMem = 0;
  for (u_int i = STORE_GENERATION_NUM; i--;) {
    totalMem += roots[i].GetSize();
  }
  gcLiveMem = 0;
#endif
}

double Store::ReadTime() {
#if defined(STORE_PROFILE)
  u_int total = 0;
  for (u_int i = STORE_GENERATION_NUM; i--;) {
    total += roots[i].GetSize();
  }
  totalMem = (total - totalMem);
#endif
  return sum_t;
}
#endif
