//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
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

#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
u_int MemChunk::counter =  0;
struct timeval *Store::sum_t;
#endif

//
// Helper Functions
//

static inline u_int ComputeUsage(MemChunk *chunk) {
  u_int used = 0;

  chunk = chunk->GetNext();
  while (!chunk->IsAnchor()) {
    used++;
    chunk = chunk->GetNext();
  }

  return used;
}

//
// Class Fields and Global Vars
//

MemChunk *Store::roots[STORE_GENERATION_NUM];
u_int Store::memUsage[STORE_GENERATION_NUM];
u_int Store::memLimits[STORE_GENERATION_NUM];

Set *Store::intgenSet = INVALID_POINTER;
Set *Store::wkDictSet = INVALID_POINTER;
u_int Store::needGC   = 0;
u_int Store::maxGcGen = 0;

char *storeChunkMax;
#if defined(STORE_CHUNKTOP_IN_REG)
register s_int storeChunkTop;
#else
s_int storeChunkTop;
#endif
MemChunk *storeCurChunk;

//
// Method Implementations
//

inline void Store::SetInitMark(u_int size) {
  ((u_int *) (storeChunkMax + storeChunkTop + size - sizeof(u_int)))[0] =
    HeaderOp::EncodeHeader(REF_LABEL, 0, 0);
}

inline void Store::SwitchToNewChunk(MemChunk *chunk) {
  storeCurChunk->SetTop(storeChunkTop);
  storeChunkTop = chunk->GetTop();
  storeChunkMax = chunk->GetMax();
  storeCurChunk = chunk;
}

inline void Store::AllocNewMemChunk(u_int size, const u_int gen) {
  // Compute necessary MemChunk Size (size must fit in)
  u_int alloc_size = STORE_MEMCHUNK_SIZE;
  size += sizeof(u_int);
  if (alloc_size < size) {
    div_t d    = std::div(size, STORE_MEMCHUNK_SIZE);
    alloc_size = ((d.quot + (d.rem ? 1 : 0)) * STORE_MEMCHUNK_SIZE);
  }

  // Allocate a new Chunk
  MemChunk *root     = roots[gen];
  MemChunk *newChunk = new MemChunk(root, storeCurChunk, alloc_size);
  // Store new Chunk into Chain and prepare for next (Fast|GC)Alloc
  root->SetNext(newChunk);
  storeCurChunk->SetPrev(newChunk);
  SwitchToNewChunk(newChunk);
  SetInitMark(0); // neded for gc

  memUsage[gen]++;
  // Generation Zero implies GC
  if (gen == 0) {
    needGC = 1;
  }
}

inline char *Store::GCAlloc(u_int size, u_int header, u_int gen) {
  for (;;) {
    char *p      = (storeChunkMax + storeChunkTop);
    s_int newtop = (storeChunkTop + size);

    ((u_int *) p)[-1] = header;
    if (newtop >= 0) {
      SetInitMark(0); // Restore old init mark
      AllocNewMemChunk(size, gen); // gen != header_gen
      continue;
    }
    SetInitMark(size);
    storeChunkTop = newtop;
    return p;
  }
}

inline Block *Store::TempAlloc(u_int size, u_int dst_gen, u_int cpy_gen) {
  u_int header = HeaderOp::EncodeHeader(MIN_DATA_LABEL, size, cpy_gen);
  Block *p     = (Block *) Store::GCAlloc((size + 1) * sizeof(u_int), header, dst_gen);

  AssertStore(p != INVALID_POINTER);
  // to be determined
  p->InitArg(0, 1);

  return p;
}

inline Block *Store::AddToFinSet(Block *p, Handler *h, word value, u_int dst_gen, u_int cpy_gen) {
  u_int size   = p->GetSize();
  u_int top    = Store::DirectWordToInt(p->GetArg(0));
  u_int newtop = (top + 2);
  Block *np;

  if (newtop >= size) {
    u_int newsize = ((size * 3) >> 1);
    
    np = Store::TempAlloc(newsize, dst_gen, cpy_gen);
    std::memcpy(np->GetBase(), p->GetBase(), (size * sizeof(u_int)));
  }
  else {
    np = p;
  }

  np->InitArg(0, newtop);
  np->InitArg(top, Store::UnmanagedPointerToWord((void *) h));
  np->InitArg((top + 1), value);
  return np;
}

void Store::AllocNewMemChunk() {
  Block *p = (Block *) (storeChunkMax + storeChunkTop);
  AllocNewMemChunk(HeaderOp::DecodeSize(p), HeaderOp::DecodeGeneration(p));
}

void Store::Shrink(MemChunk *list, int threshold) {
  list = list->GetNext();

  while (!list->IsAnchor()) {
    MemChunk *next = list->GetNext();

    if (threshold-- > 0) {
#if defined(STORE_DEBUG)
      std::printf("clearing... %d\n", list->id);
#endif
      list->Clear();
    }
    else {
      MemChunk *prev = list->GetPrev();

      prev->SetNext(next);
      next->SetPrev(prev);
      delete list;
    }
    list = next;
  }
}

inline Block *Store::CopyBlockToDst(Block *p, u_int dst_gen, u_int cpy_gen) {
  u_int size   = HeaderOp::DecodeSize(p);
  u_int header = HeaderOp::EncodeHeader(HeaderOp::DecodeLabel(p), size, cpy_gen);
  Block *newp  = (Block *) Store::GCAlloc(((1 + size) * sizeof(u_int)), header, dst_gen);

  std::memcpy(newp, p, (size * sizeof(u_int)));
  GCHelper::MarkMoved(p, newp);
  return newp;
}

inline word Store::ForwardBlock(word p, u_int dst_gen, u_int cpy_gen) {
  if (PointerOp::IsInt(p)) {
    return p;
  }

  Block *sp = PointerOp::RemoveTag(p);
  // order is important because moving ptr overwrites gen assignment
  if (GCHelper::AlreadyMoved(sp)) {
    return PointerOp::EncodeTag(GCHelper::GetForwardPtr(sp), PointerOp::DecodeTag(p));
  }
  else if (HeaderOp::DecodeGeneration(sp) < dst_gen) {
    return PointerOp::EncodeTag(CopyBlockToDst(sp, dst_gen, cpy_gen), PointerOp::DecodeTag(p));
  }
  else {
    return p;
  }
}

inline Block *Store::ForwardSet(Block *p, u_int cpy_gen, u_int dst_gen) {
  return ((HeaderOp::DecodeGeneration(p) < dst_gen) ? CopyBlockToDst(p, dst_gen, cpy_gen) : p);
}

inline void Store::ScanChunks(u_int dst_gen, u_int cpy_gen, MemChunk *anchor, Block *scan) {
  while (!anchor->IsAnchor()) {
    // Scan current MemChunk
    while (HeaderOp::GetHeader(scan) != HeaderOp::EncodeHeader(REF_LABEL, 0, 0)) {
      // Test for HandlerBlock must be done only once
      BlockLabel l = scan->GetLabel();
      if (l == HANDLERBLOCK_LABEL) {
	PointerOp::DecodeHandler(scan)->PrepareForGC(scan);
      }
    
      // Scan current tuple (if not CHUNK or WEAK_DICT_LABEL)
      u_int cursize = HeaderOp::DecodeSize(scan);
      if ((l != CHUNK_LABEL) && (l != WEAK_DICT_LABEL)) {
	for (u_int i = cursize; i--;) {
	  scan->InitArg(i, Store::ForwardBlock(PointerOp::Deref(scan->GetArg(i)),
					       dst_gen, cpy_gen));
	}
      }
      scan = (Block *) ((char *) scan + ((cursize + 1) * sizeof(u_int)));
    }

    anchor = anchor->GetPrev();
    scan   = (Block *) (anchor->GetBottom() + sizeof(u_int));
  }
}

void Store::InitStore(u_int memLimits[STORE_GENERATION_NUM]) {
  for (u_int i = STORE_GENERATION_NUM; i--;) {
    MemChunk *lanchor  = new MemChunk();
    MemChunk *ranchor  = new MemChunk();
    MemChunk *memChunk = new MemChunk(lanchor, ranchor, STORE_MEMCHUNK_SIZE);

    lanchor->SetNext(memChunk);
    ranchor->SetPrev(memChunk);
    Store::roots[i]     = lanchor;
    Store::memUsage[i]  = 1;
    Store::memLimits[i] = memLimits[i];
  }
  // Prepare Fast Memory Allocation
  MemChunk *anchor = roots[0]->GetNext();
  storeChunkTop = anchor->GetTop();
  storeChunkMax = anchor->GetMax();
  storeCurChunk = anchor;
  // Alloc Intgen- and WKDict-Set
  intgenSet = Set::New(STORE_INTGENSET_SIZE);
  wkDictSet = Set::New(STORE_WKDICTSET_SIZE);
#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
  sum_t = (struct timeval *) malloc(sizeof(struct timeval));
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

inline void Store::HandleInterGenerationalPointers(u_int gcGen, u_int dst_gen, u_int cpy_gen) {
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
	if (curgen > gcGen) {
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
		if (curfgen <= gcGen) {
		  hasyoungptrs = 1;
		  curp->InitArg(k, PointerOp::EncodeTag(CopyBlockToDst(curfp, dst_gen, cpy_gen),
							PointerOp::DecodeTag(fp)));
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

inline Block *Store::HandleWeakDictionaries(u_int dst_gen, u_int cpy_gen) {
  Set *wkdict_set = wkDictSet;
#if defined(STORE_DEBUG)
  std::printf("initial weakdict_size is %d\n", wkdict_set->GetSize()); 
#endif

  // Alloc Fin Set
  Block *finset = Store::TempAlloc(120, dst_gen, cpy_gen);
  finset->InitArg(0, 1);

  u_int rs_size = wkdict_set->GetSize();
  wkdict_set->MakeEmpty();
  Block *db_set = Store::TempAlloc((rs_size + 1), dst_gen, cpy_gen);
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
    else if (HeaderOp::DecodeGeneration(dp) < dst_gen) {
      Handler *h  = dp->GetHandler();
      Block *newp = CopyBlockToDst(dp, dst_gen, cpy_gen);
	
      ndict = PointerOp::EncodeTag(newp, PointerOp::DecodeTag(dict));
      // Finalize only empty dict
      if (((WeakDictionary *) newp)->GetCounter() == 0) {
	finset = Store::AddToFinSet(finset, h, ndict, dst_gen, cpy_gen);
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

    // Now Process Dict Table but NOT its content
    WeakDictionary *p = WeakDictionary::FromWordDirect(ndict);
    word arr          = Store::ForwardBlock(p->GetTable()->ToWord(), dst_gen, cpy_gen);
    Block *table      = Store::DirectWordToBlock(arr);
    u_int table_size  = table->GetSize();

    p->SetTable(arr);
    for (u_int k = table_size; k--;) {
      table->InitArg(k, Store::ForwardBlock(table->GetArg(k), dst_gen, cpy_gen));
    }
  }

  // Phase Two: Forward Dictionary Contents and record Finalize Candiates
  for (u_int i = rs_size; i >= 1; i--) {
    WeakDictionary *dict = WeakDictionary::FromWordDirect(db_set->GetArg(i));
    Handler *h           = dict->GetHandler();
    Block *table         = dict->GetTable();
    u_int table_size     = table->GetSize();
    MemChunk *anchor     = storeCurChunk;
    char *scan           = (storeChunkMax + storeChunkTop);

    for (u_int k = table_size; k--;) {
      HashNode *node = HashNode::FromWord(table->GetArg(k));

      if (!node->IsEmpty()) {
	word val = node->GetValue();

	if (!PointerOp::IsInt(val)) {
	  Block *valp = PointerOp::RemoveTag(val);

	  // Value has been reached by root and must kept alive
	  if (GCHelper::AlreadyMoved(valp)) {
	    node->SetValue(PointerOp::EncodeTag(GCHelper::GetForwardPtr(valp),
						PointerOp::DecodeTag(val)));
	  }
	  // Value might be finalized
	  else if (HeaderOp::DecodeGeneration(valp) < dst_gen) {
	    BlockLabel l = valp->GetLabel();

	    // Value is non Dict or empty Dict ?
	    if ((l != WEAK_DICT_LABEL) ||
		((l == WEAK_DICT_LABEL) && ((WeakDictionary *) valp)->GetCounter() == 0)) {
	      dict->RemoveEntry(node);
	      finset = Store::AddToFinSet(finset, h,
					  ForwardBlock(val, dst_gen, cpy_gen),
					  dst_gen, cpy_gen);
	    }
	    // No, save it again
	    else {
	      node->SetValue(ForwardBlock(val, dst_gen, cpy_gen));
	    }
	  }
	}
      }
    }
    Store::ScanChunks(dst_gen, cpy_gen, anchor, (Block *) scan);
  }
#if defined(STORE_DEBUG)
  std::printf("new_weakdict_size is %d\n", wkdict_set->GetSize());
#endif
  return finset;
}

inline void Store::DoGC(word &root, const u_int gcGen) {
  u_int dst_gen = (gcGen + 1);
  u_int cpy_gen = ((dst_gen == (STORE_GENERATION_NUM - 1)) ? gcGen : dst_gen);

#if defined(STORE_DEBUG)
  std::printf("GCing all gens <= %d; dst_gen %d; cpy_gen %d\n", gcGen, dst_gen, cpy_gen);

  std::printf("root_set   gen %d\n", HeaderOp::DecodeGeneration(Store::WordToBlock(root)));
  std::printf("intgen_set gen %d\n", HeaderOp::DecodeGeneration((Block *) intgenSet));
  std::printf("wkdict_set gen %d\n", HeaderOp::DecodeGeneration((Block *) wkDictSet));
#endif

  // Switch to the new Generation
  SwitchToNewChunk(roots[dst_gen]->GetNext());
  SetInitMark(0);

  // Copy Root-, Intgen- and WeakDict-Set to New Memory (if appropriate)
  Block *root_set = ForwardSet(Store::WordToBlock(root), cpy_gen, dst_gen);
  intgenSet       = (Set *) ForwardSet((Block *) intgenSet, cpy_gen, dst_gen);
  wkDictSet       = (Set *) ForwardSet((Block *) wkDictSet, cpy_gen, dst_gen);

  // Obtain scan anchor
  MemChunk *anchor = storeCurChunk;
  char *scan       = (storeChunkMax + storeChunkTop);

  // Copy matching rootset entries
  for (u_int i = root_set->GetSize(); i--;) {
    root_set->InitArg(i, Store::ForwardBlock(PointerOp::Deref(root_set->GetArg(i)),
					     dst_gen, cpy_gen));
  }
  // Scanning chunks (root_set amount)
  Store::ScanChunks(dst_gen, cpy_gen, anchor, (Block *) scan);
  // Reset Anchor and Scan Ptr (to scan the new stuff)
  anchor = storeCurChunk;
  scan   = (storeChunkMax + storeChunkTop);

  // Handle InterGenerational Pointers
  Store::HandleInterGenerationalPointers(gcGen, dst_gen, cpy_gen);
  // Scan chunks (intgen_set amount)
  Store::ScanChunks(dst_gen, cpy_gen, anchor, (Block *) scan);

  // Handle Weak Dictionaries, if any (performs scanning itself)
  Block *arr = INVALID_POINTER;
  if (wkDictSet->GetSize() != 0) {
    arr = Store::HandleWeakDictionaries(dst_gen, cpy_gen);
  }
  
  // Clean up Collected regions
  for (u_int i = dst_gen; i--;) {
    Store::Shrink(roots[i], memLimits[i]);
    memUsage[i] = ComputeUsage(roots[i]);
  }
  // Compute GC Flag (to be determined)
  needGC   = 0;
  maxGcGen = ((memUsage[cpy_gen] > memLimits[cpy_gen]) ? cpy_gen : 0);
  //  maxGcGen = (STORE_GENERATION_NUM - 2);

  // Switch Semispaces
  if (dst_gen == (STORE_GENERATION_NUM - 1)) {
#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
    std::printf("switching semispaces\n");
    std::fflush(stdout);
#endif
    MemChunk *tmp = roots[STORE_GENERATION_NUM - 2];
    roots[STORE_GENERATION_NUM - 2] = roots[STORE_GENERATION_NUM - 1];
    roots[STORE_GENERATION_NUM - 1] = tmp;

    u_int mem_tmp = memUsage[STORE_GENERATION_NUM - 2];
    memUsage[STORE_GENERATION_NUM - 2] = memUsage[STORE_GENERATION_NUM - 1];
    memUsage[STORE_GENERATION_NUM - 1] = mem_tmp;
  }

  // Switch back to Generation Zero and Adjust Root Set
  SwitchToNewChunk(roots[0]->GetNext());
  root = root_set->ToWord();

  // Call Finalization Handler
  if (arr != INVALID_POINTER) {
    u_int size = Store::WordToInt(arr->GetArg(1));
    for (u_int i = 2; i < size; i += 2) {
      Handler *h = (Handler *) PointerOp::DecodeUnmanagedPointer(arr->GetArg(i));
      h->Finalize(arr->GetArg(i + 1));
    }
  }
#if defined(STORE_DEBUG)
  std::printf("Done GC; maxGcGen is %d\n", maxGcGen);
#endif
}

void Store::DoGC(word &root) {
#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
  struct timeval start_t, end_t;
  gettimeofday(&start_t, INVALID_POINTER);
#endif
  switch (maxGcGen) {
  case STORE_GEN_YOUNGEST:
    DoGC(root, STORE_GEN_YOUNGEST); break;
  case STORE_GEN_OLDEST:
    DoGC(root, STORE_GEN_OLDEST); break;
  default:
    DoGC(root, maxGcGen); break;
  }
#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
  gettimeofday(&end_t, INVALID_POINTER);
  sum_t->tv_sec  += (end_t.tv_sec - start_t.tv_sec);
  sum_t->tv_usec += (end_t.tv_usec - start_t.tv_usec);
#endif
}

#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
void Store::MemStat() {
  std::printf("---\n");
  std::printf("maxGcGen is %d\n", maxGcGen);
  for (u_int i = 0; i < STORE_GENERATION_NUM; i++) {
    MemChunk *anchor = roots[i];
    u_int used       = 0;
    u_int total      = 0;
    
    anchor = anchor->GetNext();
    while (!anchor->IsAnchor()) {
      u_int size = (anchor->GetMax() - anchor->GetBottom());

      used  += (size + ((anchor == storeCurChunk) ? storeChunkTop : anchor->GetTop()));
      total += size;
      anchor = anchor->GetNext();
    }

    std::printf("G%d --> Used: %8d; Total: %8d; GC-Limit: %8d; Mem-Usage: %8d\n", i, used, total,
		memLimits[i] * STORE_MEMCHUNK_SIZE,
		memUsage[i] * STORE_MEMCHUNK_SIZE);
  }
  std::printf("---\n");
  std::fflush(stdout);
}

void Store::ForceGCGen(u_int gen) {
  maxGcGen = gen;
}

void Store::ResetTime() {
  sum_t->tv_sec = sum_t->tv_usec = 0;
}

struct timeval *Store::ReadTime() {
  return sum_t;
}
#endif
