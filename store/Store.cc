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

#if defined(INTERFACE)
#pragma implementation "store/HeaderOp.hh"
#endif

#if defined(INTERFACE)
#pragma implementation "store/PointerOp.hh"
#endif

#if defined(INTERFACE)
#pragma implementation "store/Store.hh"
#endif

#if defined(INTERFACE)
#pragma implementation "store/Memory.hh"
#endif

#if defined(INTERFACE)
#pragma implementation "store/GCHelper.hh"
#endif

#if defined(INTERFACE)
#pragma implementation "store/Value.hh"
#endif

#if defined(INTERFACE)
#pragma implementation "store/Set.hh"
#endif

#include "store/Base.hh"
#include "store/Types.hh"
#include "store/HeaderOp.hh"
#include "store/Handler.hh"
#include "store/PointerOp.hh"
#include "store/Store.hh"
#include "store/Memory.hh"
#include "store/GCHelper.hh"
#include "store/Value.hh"
#include "store/Set.hh"

#if defined(STORE_DEBUG)
u_int MemChunk::counter =  0;
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

word Store::intgenSet = (word) 1;
word Store::wkDictSet = (word) 1;
u_int Store::needGC   = 0;
u_int Store::gcGen    = 0;

#if defined(STORE_CHUNKTOP_IN_REG)
register char *storeChunkTop;
#else
char *storeChunkTop;
#endif
char *storeChunkMax;
MemChunk *storeCurChunk;

//
// Method Implementations
//

inline char *Store::GCAlloc(u_int size, u_int gen) {
 retry:
  char *top = storeChunkTop;
  
  storeChunkTop += size;
  if (storeChunkTop > storeChunkMax) {
    AllocNewMemChunk(size, gen);
    goto retry;
  }

  return top;
}

inline Block *Store::AllocFinSet(u_int size, u_int dst_gen, u_int cpy_gen) {
  Block *p = (Block *) Store::GCAlloc((size + 1) * sizeof(u_int), dst_gen);

  Assert(p != INVALID_POINTER);
  HeaderOp::EncodeHeader(p, MIN_DATA_LABEL, size);
  GCHelper::EncodeGen(p, cpy_gen);
  p->InitArg(1, 2);
  return p;
}

inline Block *Store::PushToFinSet(Block *p, Handler *h, word value, u_int dst_gen, u_int cpy_gen) {
  u_int size   = p->GetSize();
  u_int top    = Store::DirectWordToInt(p->GetArg(1));
  u_int newtop = (top + 2);
  Block *np;

  if (newtop > size) {
    u_int newsize = ((size * 3) >> 1);
    
    np = Store::AllocFinSet(newsize, dst_gen, cpy_gen);
    std::memcpy(np->GetBase(), p->GetBase(), (size * sizeof(u_int)));
  }
  else {
    np = p;
  }

  np->InitArg(1, newtop);
  np->InitArg(top, Store::UnmanagedPointerToWord((void *) h));
  np->InitArg((top + 1), value);

  return np;
}

void Store::AllocNewMemChunk(u_int size, u_int gen) {
  // Adjust ChunkTop: Undo last alloc try
  storeChunkTop -= size;

  // Compute necessary MemChunk Size (size must fit in)
  u_int alloc_size = STORE_MEMCHUNK_SIZE;
  if (alloc_size < size) {
    div_t d    = std::div(size, STORE_MEMCHUNK_SIZE);
    alloc_size = ((d.quot + (d.rem ? 1 : 0)) * STORE_MEMCHUNK_SIZE);
  }

  // Allocate a new Chunk
  MemChunk *root     = roots[gen];
  MemChunk *newChunk = new MemChunk(root, storeCurChunk, alloc_size);
  // Store new Chunk into Chain
  root->SetNext(newChunk);
  storeCurChunk->SetPrev(newChunk);
  memUsage[gen]++;

  // Generation Zero implies GC
  if (gen == 0) {
    needGC = 1;
  }

  // Prepare for next (Fast|GC)Alloc
  SwitchToNewChunk(newChunk);
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

#if defined(STORE_DEBUG)
static inline const char *LabelToString(Block *p) {
  switch (p->GetLabel()) {
  case MIN_DATA_LABEL:
    return "MIN_DATA_LABEL";
  case GENSET_LABEL:
    return "GENSET_LABEL";
  case STACK_LABEL:
    return "STACK_LABEL";
  case STACKARRAY_LABEL:
    return "STACKARRAY_LABEL";
  default:
    return "Unknown Label";
  }
}
#endif

inline Block *Store::CopyBlockToDst(Block *p, u_int dst_gen, u_int cpy_gen) {
  u_int size  = (1 + HeaderOp::DecodeSize(p));
  Block *newp = (Block *) Store::GCAlloc((size * sizeof(u_int)), dst_gen);
    
  std::memcpy(newp, p, (size * sizeof(word)));
  GCHelper::EncodeGen(newp, cpy_gen);

#if defined(STORE_DEBUG)
  std::printf("MOVING BLOCK: src: `%s' and dst: `%s'\n",
	      LabelToString(p), LabelToString(newp));
#endif

  return newp;
}

inline word Store::ForwardBlock(word p, u_int dst_gen, u_int cpy_gen, u_int match_gen) {
  Block *sp = PointerOp::RemoveTag(p);
     
  if (HeaderOp::GetHeader(sp) <= match_gen) {
    if (GCHelper::AlreadyMoved(sp)) {
      return GCHelper::GetForwardPtr(sp);
    }
    else {
      Block *newsp = CopyBlockToDst(sp, dst_gen, cpy_gen);
      word newp    = PointerOp::EncodeTag(newsp, PointerOp::DecodeTag(p));
      
      GCHelper::MarkMoved(sp, newp);
      return newp;
    }
  }
  else {
    return p;
  }
}

void Store::ScanChunks(u_int dst_gen, u_int cpy_gen, u_int match_gen,
		       MemChunk *anchor, char *scan) {
  while (!anchor->IsAnchor()) {
    char **chunkTop = ((anchor == storeCurChunk) ? &storeChunkTop : anchor->GetTopAddr());

    // Scan current chunk
    while (scan < *chunkTop) {
      Block *curp   = (Block *) scan;
      u_int cursize = HeaderOp::DecodeSize(curp);
      BlockLabel l  = curp->GetLabel();

      // Test for HandlerBlock must be done only once
      if (HeaderOp::HasHandlerMark(curp)) {
	PointerOp::DecodeHandler(curp)->PrepareForGC(curp);
      }

      // Scan current tuple (if not CHUNK or WEAK_DICT_LABEL)
      if ((l != CHUNK_LABEL) && (l != WEAK_DICT_LABEL)) {
	for (u_int i = 1; i <= cursize; i++) {
	  word p = PointerOp::Deref(curp->GetArg(i));
	  
	  if (!PointerOp::IsInt(p)) {
	    curp->InitArg(i, Store::ForwardBlock(p, dst_gen, cpy_gen, match_gen));
	  }
	}
      }
      scan += ((cursize + 1) * sizeof(word));
    }

    anchor = anchor->GetPrev();
    if (!anchor->IsAnchor()) {
      scan = anchor->GetBottom();
    }
  }
}

void Store::InitStore(u_int memLimits[STORE_GENERATION_NUM]) {
  for (u_int i = 0; i < STORE_GENERATION_NUM; i++) {
    MemChunk *lanchor  = new MemChunk();
    MemChunk *ranchor  = new MemChunk();
    MemChunk *memChunk = new MemChunk(lanchor, ranchor, STORE_MEMCHUNK_SIZE);

    lanchor->SetNext(memChunk);
    ranchor->SetPrev(memChunk);
    Store::roots[i]     = lanchor;
    Store::memLimits[i] = memLimits[i];
  }

  // Prepare Fast Memory Allocation
  MemChunk *anchor = roots[0]->GetNext();
  storeChunkTop = anchor->GetTop();
  storeChunkMax = anchor->GetMax();
  storeCurChunk = anchor;

  // Alloc Intgen-Set
  intgenSet = Set::New(STORE_INTGENSET_SIZE)->ToWord();
  // Alloc WKDict-Set
  wkDictSet = Set::New(STORE_WKDICTSET_SIZE)->ToWord();

}

void Store::CloseStore() {
  for (int i = (STORE_GENERATION_NUM - 1); i--;) {
    MemChunk *chain = roots[i];

    while (chain != NULL) {
      MemChunk *tmp = chain->GetNext();

      delete chain;
      chain = tmp;
    }
  }
}

void Store::AddToIntgenSet(Block *v) {
  Set *p = Set::FromWord(intgenSet);

  HeaderOp::SetIntgenMark(v);
  intgenSet = p->Push(v->ToWord())->ToWord();
}

void Store::RegisterWeakDict(WeakDictionary *v) {
  Set *p = Set::FromWord(wkDictSet);

  wkDictSet = p->Push(v->ToWord())->ToWord();
}

void Store::SwitchToNewChunk(MemChunk *chunk) {
  storeCurChunk->SetTop(storeChunkTop);
  storeChunkTop = chunk->GetTop();
  storeChunkMax = chunk->GetMax();
  storeCurChunk = chunk;
}

word Store::ResolveForwardPtr(word v) {
  word dv = PointerOp::Deref(v);

  if (!PointerOp::IsInt(dv)) {
    Block *p = PointerOp::RemoveTag(dv);

    if (GCHelper::AlreadyMoved(p)) {
      return GCHelper::GetForwardPtr(p);
    }
    else {
      return dv;
    }
  }
  else {
    return dv;
  }
}

inline void Store::HandleInterGenerationalPointers(Set *intgen_set, Set *new_intgen_set,
						   u_int gcGen, u_int dst_gen, u_int cpy_gen) {
  u_int rs_size = intgen_set->GetSize();
  
  new_intgen_set->MakeEmpty();

#if defined(STORE_DEBUG)
  std::printf("initial intgen_size is %d\n", rs_size);
#endif
  
  // Traverse intgen_set entries
  for (u_int i = 2; i <= rs_size; i++) {
    word p = PointerOp::Deref(intgen_set->GetArg(i));
    
    if (!PointerOp::IsInt(p)) {
      Block *curp  = PointerOp::RemoveTag(p);
      u_int curgen = HeaderOp::DecodeGeneration(curp);
      
      // Block is still old
      if (curgen > gcGen) {
	u_int cursize      = curp->GetSize();
	u_int hasyoungptrs = 0;
	
	// Traverse intgen_set entry for references
	for (u_int k = 1; k <= cursize; k++) {
	  word fp = PointerOp::Deref(curp->GetArg(k));
	  
	  if (!PointerOp::IsInt(fp)) {
	    Block *curfp  = PointerOp::RemoveTag(fp);
	    u_int curfgen = HeaderOp::DecodeGeneration(curfp);
	    
	    // found young gc'ed ptr
	    if (curfgen <= gcGen) {
	      hasyoungptrs = 1;
	      
	      if (GCHelper::AlreadyMoved(curfp)) {
		curp->InitArg(k, GCHelper::GetForwardPtr(curfp));
	      }
	      else {
		Block *newsp = CopyBlockToDst(curfp, dst_gen, cpy_gen);
		word newp    = PointerOp::EncodeTag(newsp, PointerOp::DecodeTag(fp));

		GCHelper::MarkMoved(newsp, newp);
		curp->InitArg(k, newp);
	      }
	    }
	    // found young normal ptr 
	    else if (curfgen < curgen) {
	      hasyoungptrs = 1;
	    }
	    // ptr is equal or older
	    else {
	      // nothing to be done
	    }
	  }
	}

	// p contains young ptrs and remains within intgen_set
	if (hasyoungptrs) {
	  new_intgen_set->Push(p);
	}
	// p does not contain youngs ptr any longer
	else {
	  HeaderOp::ClearIntgenMark(curp);
	}
      }
      // Block is no longer old and therefore must be alive and can't contain intgens any longer
      else if (GCHelper::AlreadyMoved(curp)) {
	HeaderOp::ClearIntgenMark(PointerOp::RemoveTag(GCHelper::GetForwardPtr(curp)));
      }
      // Block is garbage
      else {
	// nothing to be done
      }
    }
  }

#if defined(STORE_DEBUG)
  std::printf("new_intgen_size is %d\n", new_intgen_size->GetSize());
#endif
}

inline Block *Store::HandleWeakDictionaries(Set *wkdict_set, Set *new_wkdict_set,
					    u_int match_gen, u_int dst_gen, u_int cpy_gen) {
  u_int rs_size = wkdict_set->GetSize();

  new_wkdict_set->MakeEmpty();

#if defined(STORE_DEBUG)
  std::printf("initial weakdict_size is %d\n", rs_size); 
#endif

  // Alloc Fin Set
  Block *finset = Store::AllocFinSet(120, dst_gen, cpy_gen);

  // Phase One: Forward all Dictionaries but not the contents
  for (u_int i = 2; i <= rs_size; i++) {
    word dict  = wkdict_set->GetArg(i);
    Block *dp  = Store::DirectWordToBlock(dict);
    word ndict;

    if (HeaderOp::GetHeader(dp) <= match_gen) {
      // Dictionary has been reached from Root Set and must kept alive
      if (GCHelper::AlreadyMoved(dp)) {
	ndict = GCHelper::GetForwardPtr(dp);
	new_wkdict_set->Push(ndict);
      }
      // Dictionary must be finalized
      else {
	Handler *h  = dp->GetHandler();
	Block *newp = CopyBlockToDst(dp, dst_gen, cpy_gen);
	
	ndict = PointerOp::EncodeTag(newp, PointerOp::DecodeTag(dict));
	GCHelper::MarkMoved(dp, ndict);
	finset = Store::PushToFinSet(finset, h, ndict, dst_gen, cpy_gen);
      }
    }
    // Can't decide whether it was reached or not; must assume yes.
    else {
      ndict = dict;
      new_wkdict_set->Push(ndict);
    }
    // Keep Dict References complete for working
    wkdict_set->InitArg(i, ndict);

    // Now Process Dict Tables but NOT their Contents
    WeakDictionary *p = WeakDictionary::FromWordDirect(ndict);
    word arr          = Store::ForwardBlock(p->GetTable()->ToWord(), dst_gen, cpy_gen, match_gen);
    Block *table      = Store::DirectWordToBlock(arr);
    u_int table_size  = table->GetSize();

    p->SetTable(arr);

    for (u_int k = 1; k <= table_size; k++) {
      table->InitArg(k, Store::ForwardBlock(table->GetArg(k), dst_gen, cpy_gen, match_gen));
    }
  }

  // Phase Two: Forward Dictionary Contents and record Finalize Candiates
  for (u_int i = 2; i <= rs_size; i++) {
    WeakDictionary *dict = WeakDictionary::FromWordDirect(wkdict_set->GetArg(i));
    Handler *h           = dict->GetHandler();
    Block *table         = dict->GetTable();
    u_int table_size     = table->GetSize();
    MemChunk *anchor     = storeCurChunk;
    char *scan           = storeChunkTop;

    for (u_int k = 1; k <= table_size; k++) {
      HashNode *node = HashNode::FromWord(table->GetArg(k));

      if (!node->IsEmpty()) {
	word val = node->GetValue();

	if (!PointerOp::IsInt(val)) {
	  Block *valp = PointerOp::RemoveTag(val);

	  if (HeaderOp::GetHeader(valp) <= match_gen) {
	    // Value has been reached by root and must kept alive
	    if (GCHelper::AlreadyMoved(valp)) {
	      node->SetValue(GCHelper::GetForwardPtr(valp));
	    }
	    // Value should be finalized
	    else {
	      dict->RemoveEntry(node);
	      // This Is A Finalization Candidate
	      finset = Store::PushToFinSet(finset, h,
					   ForwardBlock(val, dst_gen, cpy_gen, match_gen),
					   dst_gen, cpy_gen);
	    }
	  }
	}
      }
    }
    Store::ScanChunks(dst_gen, cpy_gen, match_gen, anchor, scan);
  } 

#if defined(STORE_DEBUG)
  std::printf("new_weakdict_size is %d\n", new_wkdict_set->GetSize());
#endif

  return finset;
}

void Store::DoGC(word &root) {
  PLACEGENERATIONLIMIT;
  u_int match_gen = gen_limits[gcGen];
  u_int dst_gen   = (gcGen + 1);
  u_int cpy_gen   = ((dst_gen == (STORE_GENERATION_NUM - 1)) ? (dst_gen - 1) : dst_gen);
  Block *root_set = Store::WordToBlock(root);
  Set *intgen_set = Set::FromWord(intgenSet);
  Set *wkdict_set = Set::FromWord(wkDictSet);
  u_int rs_size   = root_set->GetSize();
  Block *new_root_set;
  Set *new_intgen_set;
  Set *new_wkdict_set;

  // Switch to the new Generation
  SwitchToNewChunk(roots[dst_gen]->GetNext());

  // Copy Root-, Intgen- and WeakDict-Set to New Memory (if appropriate)
  new_root_set   = ((HeaderOp::GetHeader(root_set) <= match_gen) ?
		    CopyBlockToDst(root_set, dst_gen, cpy_gen) : root_set);
  
  new_intgen_set = ((HeaderOp::GetHeader((Block *) intgen_set) <= match_gen) ?
		    (Set *) CopyBlockToDst((Block *) intgen_set, dst_gen, cpy_gen) : intgen_set);
  
  new_wkdict_set = ((HeaderOp::GetHeader((Block *) wkdict_set) <= match_gen) ?
		    (Set *) CopyBlockToDst((Block *) wkdict_set, dst_gen, cpy_gen) : intgen_set);

  // Obtain scan anchor
  MemChunk *anchor = storeCurChunk;
  char *scan       = storeChunkTop;

  // Copy matching rootset entries
  for (u_int i = 1; i <= rs_size; i++) {
    word p = PointerOp::Deref(new_root_set->GetArg(i));

    if (!PointerOp::IsInt(p)) {
      new_root_set->InitArg(i, Store::ForwardBlock(p, dst_gen, cpy_gen, match_gen));
    }
  }
  
  // Scanning chunks (root_set amount)
  Store::ScanChunks(dst_gen, cpy_gen, match_gen, anchor, scan);

  // Reset Anchor and Scan Ptr (to scan the new stuff)
  anchor = storeCurChunk;
  scan   = storeChunkTop;

  // Handle InterGenerational Pointers
  Store::HandleInterGenerationalPointers(intgen_set, new_intgen_set, gcGen, dst_gen, cpy_gen);

  // change to the new intgenset
  Store::intgenSet = new_intgen_set->ToWord();

  // Scan chunks (intgen_set amount)
  Store::ScanChunks(dst_gen, cpy_gen, match_gen, anchor, scan);

  // Handle Weak Dictionaries (performs scanning itself)
  Block *arr = Store::HandleWeakDictionaries(wkdict_set, new_wkdict_set,
					     match_gen, dst_gen, cpy_gen);

  // change to new dict list
  wkDictSet = new_wkdict_set->ToWord();
  
  // Clean up Collected regions
  for (u_int i = 0; i <= gcGen; i++) {
    u_int threshold = ((i == 0) ? 1 : 2);

#if defined(STORE_DEBUG)
    std::printf("Shrinking generation: %d\n", i);
#endif

    Store::Shrink(roots[i], threshold);
    memUsage[i] = ComputeUsage(roots[i]);
  }

  // Compute GC Flag
  needGC = 0;
  gcGen  = ((memUsage[dst_gen] > memLimits[dst_gen]) ? cpy_gen : 0);

  // Switch Semispaces
  if (dst_gen == (STORE_GENERATION_NUM - 1)) {
    MemChunk *tmp = roots[STORE_GENERATION_NUM - 2];

    roots[STORE_GENERATION_NUM - 2] = roots[STORE_GENERATION_NUM - 1];
    roots[STORE_GENERATION_NUM - 1] = tmp;
  }

  // Switch back to Generation Zero and Adjust Root Set
  SwitchToNewChunk(roots[0]->GetNext());
  root = new_root_set->ToWord();

  // Call Finalisation Handler
  rs_size = arr->GetSize();
  for (u_int i = 1; i <= rs_size; i += 2) {
    ((Handler *) PointerOp::DecodeUnmanagedPointer(arr->GetArg(i)))->Finalize(arr->GetArg(i + 1));
  }
}

#if defined(STORE_DEBUG)
void Store::MemStat() {
  static const char *val[] = { "no", "yes" };

  std::printf("---\n");
  std::printf("GC necessary: %s \n", val[NeedGC()]);
  std::printf("---\n");
  for (u_int i = 0; i < STORE_GENERATION_NUM; i++) {
    MemChunk *anchor = roots[i];
    u_int used       = 0;
    u_int total      = 0;
    
    anchor = anchor->GetNext();
    while (!anchor->IsAnchor()) {
      std::printf("Scanning Id: %d\n", anchor->id);
      used += (((anchor == storeCurChunk) ? storeChunkTop : anchor->GetTop())
	       - anchor->GetBottom());
      total += (anchor->GetMax() - anchor->GetBottom());
      anchor = anchor->GetNext();
    }

    std::printf("G%d --> Used: %d; Total: %d\n", i, used, total);
  }
  std::printf("---\n");
  std::fflush(stdout);
}

void Store::ForceGCGen(u_int gen) {
  gcGen = gen;
}
#endif
