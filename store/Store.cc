//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000-2001
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#include <cstdlib>
#include <cstring>
#include <cstdio>

#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
#include <sys/time.h>
#include <unistd.h>
#endif

#if defined(INTERFACE)
#pragma implementation "store/HeaderOp.hh"
#pragma implementation "store/PointerOp.hh"
#pragma implementation "store/Store.hh"
#pragma implementation "store/Memory.hh"
#pragma implementation "store/GCHelper.hh"
#pragma implementation "store/Value.hh"
#pragma implementation "store/Set.hh"
#endif

#include "store/Store.hh"
#include "store/Memory.hh"
#include "store/GCHelper.hh"
#include "store/Set.hh"

//
// Class Fields and Global Vars
//

MemChunk *Store::roots[STORE_GENERATION_NUM];
u_int Store::memMax[STORE_GENERATION_NUM];
u_int Store::memFree;
u_int Store::memTolerance;

char *Store::curChunkMax;
s_int Store::curChunkTop;
MemChunk *Store::curChunk;
u_int Store::hdrGen;
u_int Store::dstGen;

Set *Store::intgenSet        = INVALID_POINTER;
Set *Store::wkDictSet        = INVALID_POINTER;
u_int Store::needGC          = 0;
Finalization *Store::handler = INVALID_POINTER;

#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
u_int Store::totalMem = 0;
u_int Store::gcLiveMem = 0;
u_int MemChunk::counter =  0;
struct timeval *Store::sum_t;
#endif

//
// Method Implementations
//

static inline u_int GetMagicHeader() {
  return HeaderOp::EncodeHeader(REF_LABEL, 0, 0);
}

inline u_int Store::GetMemUsage(MemChunk *chunk) {
  u_int size = 0;
  
  chunk = chunk->GetNext();
  while (!chunk->IsAnchor()) {
    s_int top = (s_int) ((chunk == curChunk) ? curChunkTop : chunk->GetTop());
    
    size += (chunk->GetSize() + top);
    chunk = chunk->GetNext();
  }
  return size;
}

inline void Store::SwitchToChunk(MemChunk *chunk) {
  curChunk->SetTop(curChunkTop);
  curChunkTop = chunk->GetTop();
  curChunkMax = chunk->GetMax();
  curChunk    = chunk;
}

inline void Store::AllocNewMemChunk(u_int size, const u_int gen) {
  // Compute necessary MemChunk Size (requested size must fit in)
  u_int alloc_size = STORE_MEMCHUNK_SIZE;
  size += sizeof(u_int);
  if (alloc_size < size) {
    div_t d    = std::div(size, STORE_MEMCHUNK_SIZE);
    alloc_size = ((d.quot + (d.rem ? 1 : 0)) * STORE_MEMCHUNK_SIZE);
  }

  // Allocate a Chunk
  MemChunk *root  = roots[gen];
  MemChunk *chunk = new MemChunk(root, curChunk, alloc_size);
  // Store chunk into Chain and prepare for next (Fast|GC)Alloc
  root->SetNext(chunk);
  curChunk->SetPrev(chunk);
  SwitchToChunk(chunk);

  needGC = (GetMemUsage(roots[gen]) > memMax[gen]);
}

inline char *Store::GCAlloc(u_int size, u_int header) {
  for (;;) {
    char *p      = (curChunkMax + curChunkTop);
    s_int newtop = (curChunkTop + size);

    ((u_int *) p)[-1] = header;
    if (newtop >= 0) {
      ((u_int *) p)[-1] = GetMagicHeader();  // Restore old end mark
      AllocNewMemChunk(size, Store::dstGen);
      continue;
    }
    curChunkTop = newtop;
    return p;
  }
}

inline char *Store::GCAlloc(u_int size) {
  return Store::GCAlloc(BlockMemSize(size), HeaderOp::EncodeHeader(MIN_DATA_LABEL, size, hdrGen));
}

inline Block *Store::AddToFinSet(Block *p, word value) {
  if (p == INVALID_POINTER) {
    p = (Block *) Store::GCAlloc(120);
    p->InitArg(0, 1);
  }

  u_int top    = Store::DirectWordToInt(p->GetArg(0));
  u_int newtop = (top + 1);
  u_int size   = p->GetSize();
  Block *np;

  if (newtop >= size) {
    u_int newsize = ((size * 3) >> 1);

    np = (Block *) Store::GCAlloc(newsize);
    HeaderOp::EncodeHeader(MIN_DATA_LABEL, newsize, hdrGen);
    AssertStore(np != INVALID_POINTER);
    std::memcpy(np->GetBase(), p->GetBase(), (size * sizeof(u_int)));
  }
  else {
    np = p;
  }

  np->InitArg(0, newtop);
  np->InitArg(top, value);
  return np;
}

void Store::AllocNewMemChunk() {
  Block *p = (Block *) (curChunkMax + curChunkTop);
  AllocNewMemChunk((1 + HeaderOp::DecodeSize(p)) * sizeof(u_int), HeaderOp::DecodeGeneration(p));
}

inline void Store::FreeMemChunks(MemChunk *chunk, const u_int threshold) {
  chunk = chunk->GetNext();

  u_int used = 0;
  while (!chunk->IsAnchor()) {
    MemChunk *next = chunk->GetNext();
    
    if (used < threshold) {
      used += chunk->GetSize();
#if defined(STORE_DEBUG)
      std::printf("clearing... %d\n", chunk->id);
#endif
      chunk->Clear();
    }
    else {
      MemChunk *prev = chunk->GetPrev();

      prev->SetNext(next);
      next->SetPrev(prev);
      delete chunk;
    }
    chunk = next;
  }
}

inline Block *Store::CloneBlock(Block *p) {
  u_int size   = HeaderOp::DecodeSize(p);
  u_int header = HeaderOp::EncodeHeader(HeaderOp::DecodeLabel(p), size, hdrGen);
  Block *newp  = (Block *) Store::GCAlloc(BlockMemSize(size), header);

  std::memcpy(newp, p, (size * sizeof(u_int)));
  GCHelper::MarkMoved(p, newp);
  return newp;
}

inline word Store::ForwardWord(word p) {
  if (PointerOp::IsInt(p)) {
    return p;
  }

  Block *sp = PointerOp::RemoveTag(p);
  // order is important because moving ptr overwrites gen assignment
  if (GCHelper::AlreadyMoved(sp)) {
    return PointerOp::EncodeTag(GCHelper::GetForwardPtr(sp), PointerOp::DecodeTag(p));
  }
  else if (HeaderOp::DecodeGeneration(sp) < dstGen) {
    return PointerOp::EncodeTag(CloneBlock(sp), PointerOp::DecodeTag(p));
  }
  else {
    return p;
  }
}

inline Block *Store::ForwardSet(Block *p) {
  return ((HeaderOp::DecodeGeneration(p) < dstGen) ? CloneBlock(p) : p);
}

inline s_int Store::CanFinalize(Block *p) {
  BlockLabel l = p->GetLabel();
  // Value is non Dict or empty Dict ?
  return ((l != WEAK_DICT_LABEL) ||
	  ((l == WEAK_DICT_LABEL) && ((WeakDictionary *) p)->GetCounter() == 0));
}

inline void Store::CheneyScan(MemChunk *chunk, Block *scan) {
  while (!chunk->IsAnchor()) {
    // Scan current MemChunk
    while (HeaderOp::GetHeader(scan) != GetMagicHeader()) {
      // Test for HandlerBlock must be done only once
      BlockLabel l = scan->GetLabel();
      if (l == HANDLERBLOCK_LABEL) {
	PointerOp::DecodeHandler(scan)->PrepareForGC(scan);
      }
    
      // Scan current tuple (if not CHUNK or WEAK_DICT_LABEL)
      u_int cursize = HeaderOp::DecodeSize(scan);
      if ((l != CHUNK_LABEL) && (l != WEAK_DICT_LABEL)) {
	for (u_int i = cursize; i--;) {
	  scan->InitArg(i, Store::ForwardWord(PointerOp::Deref(scan->GetArg(i))));
	}
      }
      scan = (Block *) ((char *) scan + BlockMemSize(cursize));
    }

    chunk = chunk->GetPrev();
    scan  = (Block *) (chunk->GetBottom() + sizeof(u_int));
  }
}

void Store::InitStore(u_int mem_max[STORE_GENERATION_NUM], u_int mem_free, u_int mem_tolerance) {
  for (u_int i = STORE_GENERATION_NUM; i--;) {
    MemChunk *lanchor  = new MemChunk();
    MemChunk *ranchor  = new MemChunk();
    MemChunk *memChunk = new MemChunk(lanchor, ranchor, STORE_MEMCHUNK_SIZE);

    lanchor->SetNext(memChunk);
    ranchor->SetPrev(memChunk);
    Store::roots[i]  = lanchor;
    Store::memMax[i] = mem_max[i];
  }
  Store::memFree      = mem_free;
  Store::memTolerance = mem_tolerance;
  // Prepare Memory Allocation
  MemChunk *anchor = roots[0]->GetNext();
  curChunkTop = anchor->GetTop();
  curChunkMax = anchor->GetMax();
  curChunk    = anchor;
  // Alloc Intgen- and WKDict-Set
  intgenSet = Set::New(STORE_INTGENSET_SIZE);
  wkDictSet = Set::New(STORE_WKDICTSET_SIZE);
  handler   = INVALID_POINTER;
#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
  totalMem = 0;
  sum_t    = (struct timeval *) malloc(sizeof(struct timeval));
#endif
}

void Store::CloseStore() {
  for (int i = (STORE_GENERATION_NUM - 1); i--;) {
    MemChunk *chain = roots[i];

    while (chain != INVALID_POINTER) {
      MemChunk *tmp = chain->GetNext();
      delete chain;
      chain = tmp;
    }
  }
}

void Store::AddToIntgenSet(Block *v) {
  HeaderOp::SetChildishFlag(v);
  intgenSet = intgenSet->Push(v->ToWord());
}

void Store::RegisterWeakDict(WeakDictionary *v) {
  wkDictSet = wkDictSet->Push(v->ToWord());
}

word Store::ResolveForwardPtr(word v) {
  word dv = PointerOp::Deref(v);

  if (!PointerOp::IsInt(dv)) {
    Block *p = PointerOp::RemoveTag(dv);

    if (HeaderOp::DecodeGeneration(p) == 0) {
      return PointerOp::EncodeTag(GCHelper::GetForwardPtr(p), PointerOp::DecodeTag(dv));
    }
  }
  return dv;
}

inline void Store::HandleInterGenerationalPointers(u_int gen) {
  Set *intgen_set = intgenSet;
#if defined(STORE_DEBUG)
  std::printf("initial intgen_size is %d\n", intgen_set->GetSize());
#endif
  u_int rs_size = intgen_set->GetSize();
  intgen_set->MakeEmpty();
  // Traverse intgen_set entries (to be changed soon)
  for (u_int i = 1; i <= rs_size; i++) {
    word p = PointerOp::Deref(intgen_set->GetArg(i));
    
    if (!PointerOp::IsInt(p)) {
      Block *curp = PointerOp::RemoveTag(p);
      
      // Block is no longer old but alive. It can't contain intgens any longer
      if (GCHelper::AlreadyMoved(curp)) {
	HeaderOp::ClearChildishFlag(GCHelper::GetForwardPtr(curp));
      }
      else {
	u_int curgen = HeaderOp::DecodeGeneration(curp);

	// Block is still old
	if (curgen > gen) {
	  u_int hasyoungptrs = 0;

	  // Traverse intgen_set entry for young references
	  for (u_int k = curp->GetSize(); k--;) {
	    word fp = PointerOp::Deref(curp->GetArg(k));
	    
	    if (!PointerOp::IsInt(fp)) {
	      Block *curfp = PointerOp::RemoveTag(fp);
	      
	      // found young moved ptr
	      if (GCHelper::AlreadyMoved(curfp)) {
		hasyoungptrs = 1;
		curp->InitArg(k, PointerOp::EncodeTag(GCHelper::GetForwardPtr(curfp),
						      PointerOp::DecodeTag(fp)));
	      }
	      // need to check ptrs age
	      else {
		u_int curfgen = HeaderOp::DecodeGeneration(curfp);
		
		// found young ptr to be moved
		if (curfgen <= gen) {
		  hasyoungptrs = 1;
		  curp->InitArg(k,
				PointerOp::EncodeTag(CloneBlock(curfp), PointerOp::DecodeTag(fp)));
		}
		// found young normal ptr
		else if (curfgen < curgen) {
		  hasyoungptrs = 1;
		}
		// ptr is equal or older
	      }
	    }
	  }

	  // p contains young ptrs and remains within intgen_set
	  if (hasyoungptrs) {
	    intgen_set->Push(p);
	  }
	  // p does not contain young ptrs any longer
	  else {
	    HeaderOp::ClearChildishFlag(curp);
	  }
	}
	// block is garbage
      }
    }
  }
#if defined(STORE_DEBUG)
  std::printf("new_intgen_size is %d\n", intgen_set->GetSize());
#endif
}
inline Block *Store::HandleWeakDictionaries() {
  Set *wkdict_set = wkDictSet;
#if defined(STORE_DEBUG)
  std::printf("initial weakdict_size is %d\n", wkdict_set->GetSize()); 
#endif
  // Allocate and initialize Finalisation Set
  Block *finset = INVALID_POINTER;

  u_int rs_size = wkdict_set->GetSize();
  Block *db_set = (Block *) Store::GCAlloc((rs_size + 1));
  wkdict_set->MakeEmpty();
  std::memcpy(db_set->GetBase(), ((Block *) wkdict_set)->GetBase(),
	      ((rs_size + 1) * sizeof(u_int)));

  // Phase One: Forward all Dictionaries but not the contents
  for (u_int i = rs_size; i >= 1; i--) {
    word dict  = db_set->GetArg(i);
    Block *dp  = Store::DirectWordToBlock(dict);
    word ndict;

    // Dictionary has been reached from Root Set and must kept alive
    if (GCHelper::AlreadyMoved(dp)) {
      ndict = PointerOp::EncodeTag(GCHelper::GetForwardPtr(dp), PointerOp::DecodeTag(dict));
      wkdict_set->Push(ndict);
    }
    // Dictionary might be finalized
    else if (HeaderOp::DecodeGeneration(dp) < dstGen) {
      Block *newp = CloneBlock(dp);
	
      ndict = PointerOp::EncodeTag(newp, PointerOp::DecodeTag(dict));
      // Finalize only empty dict
      if (((WeakDictionary *) newp)->GetCounter() == 0) {
	finset = Store::AddToFinSet(finset, ndict);
      }
      // Keep it alive (thanks to Denys for pointing that out)
      else {
	wkdict_set->Push(ndict);
      }
    }
    // Can't decide whether it was reached or not; must assume yes.
    else {
      ndict = dict;
      wkdict_set->Push(ndict);
    }
    // Keep Dict References complete for working
    db_set->InitArg(i, ndict);

    // Now Process DictTable and its HashNodes but NOT the content
    WeakDictionary *p = WeakDictionary::FromWordDirect(ndict);
    word arr          = Store::ForwardWord(p->GetTable()->ToWord());
    p->SetTable(arr);
    Block *table = Store::DirectWordToBlock(arr);
    for (u_int k = table->GetSize(); k--;) {
      table->InitArg(k, Store::ForwardWord(table->GetArg(k)));
    }
  }
  // Phase Two: Check for integer or forwarded entries in all dictionaries and handle them
  for (u_int i = rs_size; i >= 1; i--) {
    WeakDictionary *dict = WeakDictionary::FromWordDirect(db_set->GetArg(i));
    Block *table         = dict->GetTable();

    for (u_int k = table->GetSize(); k--;) {
      HashNode *node = HashNode::FromWord(table->GetArg(k));

      if (!node->IsEmpty()) {
	word val = PointerOp::Deref(node->GetValue());

	// Store Integers and mark node as handled
	if (PointerOp::IsInt(val)) {
	  node->SetValue(val);
	  node->MarkHandled();
	}
	// Store Forward ptr and mark node as handled; otherwise leave untouched
	else {
	  Block *valp = PointerOp::RemoveTag(val);

	  if (GCHelper::AlreadyMoved(valp)) {
	    node->SetValue(PointerOp::EncodeTag(GCHelper::GetForwardPtr(valp),
						PointerOp::DecodeTag(val)));
	    node->MarkHandled();
	  }
	}
      }
    }
  }
  // Phase Three: Forward Dictionary Contents and record Finalize Candiates
  MemChunk *chunk = curChunk;
  char *scan      = (curChunkMax + curChunkTop);
  for (u_int i = rs_size; i >= 1; i--) {
    WeakDictionary *dict = WeakDictionary::FromWordDirect(db_set->GetArg(i));
    Block *table         = dict->GetTable();
    for (u_int k = table->GetSize(); k--;) {
      HashNode *node = HashNode::FromWord(table->GetArg(k));

      if (!node->IsEmpty()) {
	// Remove handled marks
	if (node->IsHandled()) {
	  node->MarkNormal();
	}
	// This node possibly contains finalisation data
	// invariant: it is a block
	else {
	  word val    = PointerOp::Deref(node->GetValue());
	  Block *valp = PointerOp::RemoveTag(val);
	  
	  // Value has been finalized or saved before
	  if (GCHelper::AlreadyMoved(valp)) {
	    if (Store::CanFinalize(valp)) {
	      dict->RemoveEntry(node);
	    }
	    else {
	      node->SetValue(PointerOp::EncodeTag(GCHelper::GetForwardPtr(valp),
						  PointerOp::DecodeTag(val)));
	    }
	  }
	  // Value might be finalized
	  else if (HeaderOp::DecodeGeneration(valp) < dstGen) {
	    if (Store::CanFinalize(valp)) {
	      dict->RemoveEntry(node);
	      finset = Store::AddToFinSet(finset, ForwardWord(val));
	    }
	    // No, forward and save it again
	    else {
	      node->SetValue(ForwardWord(val));
	    }
	  }
	  // Unable to decide; leave value untouched but derefed
	  else {
	    node->SetValue(val);
	  }
	}
      }
    }
  }
  // Now successivly forward the finalized tree
  Store::CheneyScan(chunk, (Block *) scan);
#if defined(STORE_DEBUG)
  std::printf("new_weakdict_size is %d\n", wkdict_set->GetSize());
#endif
  return finset;
}

static inline u_int min(u_int a, u_int b) {
  return ((a <= b) ? a : b);
}

inline void Store::DoGC(word &root, const u_int gen) {
  dstGen = (gen + 1);
  hdrGen = ((dstGen == (STORE_GENERATION_NUM - 1)) ? gen : dstGen);

  // Switch to the new Generation
  SwitchToChunk(roots[dstGen]->GetNext());

  // Copy Root-, Intgen- and WeakDict-Set to New Memory (if appropriate)
  Block *root_set = ForwardSet(Store::WordToBlock(root));
  intgenSet       = (Set *) ForwardSet((Block *) intgenSet);
  wkDictSet       = (Set *) ForwardSet((Block *) wkDictSet);

  // Obtain scan start
  MemChunk *chunk = curChunk;
  char *scan      = (curChunkMax + curChunkTop);

  // Copy matching rootset entries
  for (u_int i = root_set->GetSize(); i--;) {
    root_set->InitArg(i, Store::ForwardWord(PointerOp::Deref(root_set->GetArg(i))));
  }
  // Scanning chunks (root_set amount)
  Store::CheneyScan(chunk, (Block *) scan);

  // Obtain new scan start (to scan intgen set stuff)
  chunk = curChunk;
  scan  = (curChunkMax + curChunkTop);
  // Handle InterGenerational Pointers
  Store::HandleInterGenerationalPointers(gen);
  // Scan chunks (intgen_set amount)
  Store::CheneyScan(chunk, (Block *) scan);

  // Handle Weak Dictionaries, if any (performs scanning itself)
  Block *arr = INVALID_POINTER;
  if (wkDictSet->GetSize() != 0) {
    arr = Store::HandleWeakDictionaries();
  }
  
  // Clean up Collected regions
  for (u_int i = dstGen; i--;) {
    Store::FreeMemChunks(roots[i], memMax[i]);
  }

  // Switch Semispaces
  if (dstGen == (STORE_GENERATION_NUM - 1)) {
    MemChunk *tmp = roots[STORE_GENERATION_NUM - 2];
    roots[STORE_GENERATION_NUM - 2] = roots[STORE_GENERATION_NUM - 1];
    roots[STORE_GENERATION_NUM - 1] = tmp;
  }

  // Clear GC Flag and Calc Limits for next gen GC
  needGC = 0;
  // Calc Limits for next GC
  u_int wanted = ((GetMemUsage(roots[hdrGen]) * 100) / (100 - memFree));

  // Try to align them to block size
  s_int block_size = STORE_MEMCHUNK_SIZE;
  s_int block_dist = wanted % block_size;

  if (block_dist > 0) {
    block_dist = block_size - block_dist;
  }

  wanted += min(block_dist, ((wanted * memTolerance) / 100));
  memMax[hdrGen] = wanted;

  // Switch back to Generation Zero and Adjust Root Set
  SwitchToChunk(roots[0]->GetNext());
  root = root_set->ToWord();

  // Call Finalization Handler
  if ((arr != INVALID_POINTER) && (handler != INVALID_POINTER)) {
    u_int size = (Store::WordToInt(arr->GetArg(0)) - 1);
    for (u_int i = size; i >= 1; i--) {
      handler->Finalize(arr->GetArg(i));
    }
  }
}

void Store::DoGC(word &root) {
#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
  struct timeval start_t, end_t;
  gettimeofday(&start_t, INVALID_POINTER);
#endif
  // Determine GC Range
  u_int gen = (STORE_GENERATION_NUM - 2);
  while ((gen > 0) && (GetMemUsage(roots[gen]) <= memMax[gen])) {
    gen--;
  }

#if defined(STORE_DEBUG)
  std::printf("GCing all gens <= %d.\n", gen);

  std::printf("root_set   gen %d\n", HeaderOp::DecodeGeneration(Store::WordToBlock(root)));
  std::printf("intgen_set gen %d\n", HeaderOp::DecodeGeneration((Block *) intgenSet));
  std::printf("wkdict_set gen %d\n", HeaderOp::DecodeGeneration((Block *) wkDictSet));
#endif

#if defined(STORE_PROFILE)
  u_int dstGen   = (gen + 1);
  u_int hdrGen   = ((dstGen == (STORE_GENERATION_NUM - 1)) ? gen : dstGen);
  u_int memUsage = GetMemUsage(roots[hdrGen]);
#endif

  switch (gen) {
  case STORE_GEN_YOUNGEST:
    DoGC(root, STORE_GEN_YOUNGEST); break;
#if defined(STORE_GEN_OLDEST)
  case STORE_GEN_OLDEST:
    DoGC(root, STORE_GEN_OLDEST); break;
#endif
  default:
    DoGC(root, gen); break;
  }
#if defined(STORE_DEBUG)
  std::printf("Done GC.\n");
#endif
#if defined(STORE_DEBUG) 
  gettimeofday(&end_t, INVALID_POINTER);
  sum_t->tv_sec  += (end_t.tv_sec - start_t.tv_sec);
  sum_t->tv_usec += (end_t.tv_usec - start_t.tv_usec);
#endif
#if defined(STORE_PROFILE)
  gcLiveMem += (GetMemUsage(roots[hdrGen]) - memUsage);
#endif
}

void Store::SetGCParams(u_int mem_free, u_int mem_tolerance) {
  Store::memFree      = mem_free;
  Store::memTolerance = mem_tolerance;
}

#if defined(STORE_DEBUG)
void Store::ForceGC(word &root, const u_int gen) {
  Store::DoGC(root, gen);
}
#endif


#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
void Store::MemStat() {
  std::printf("---\n");
  std::printf("ingen_set size: %u\n", intgenSet->GetSize());
  std::printf("---\n");
  for (u_int i = 0; i < STORE_GENERATION_NUM; i++) {
    MemChunk *chunk = roots[i]->GetNext();
    u_int used      = 0;
    u_int total     = 0;

    while (!chunk->IsAnchor()) {
      u_int size = chunk->GetSize();
      s_int top  = (s_int) ((chunk == curChunk) ? curChunkTop : chunk->GetTop());
      
      used += (size + top);
      total += size;
      chunk = chunk->GetNext();
    }
    std::printf("G%d --> Used: %8u; Total: %8u; GC-Limit: %8u.\n", i, used, total, memMax[i]);
  }
  std::printf("---\n");
  std::fflush(stdout);
}

void Store::ResetTime() {
  sum_t->tv_sec = sum_t->tv_usec = 0;

  totalMem = 0;
  for (u_int i = STORE_GENERATION_NUM; i--;) {
    totalMem += Store::GetMemUsage(roots[i]);
  }
  gcLiveMem = 0;
}

struct timeval *Store::ReadTime() {
  u_int total = 0;

  for (u_int i = STORE_GENERATION_NUM; i--;) {
    total += Store::GetMemUsage(roots[i]);
  }
  totalMem = (total - totalMem);
  return sum_t;
}
#endif
