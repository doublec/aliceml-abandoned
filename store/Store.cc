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
u_int Store::maxGcGen = 1;

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

inline void Store::AllocNewMemChunk(u_int size, u_int gen) {
  // Compute necessary MemChunk Size (size must fit in)
  u_int alloc_size = STORE_MEMCHUNK_SIZE;
  u_int need_size  = (size + sizeof(u_int));
  if (alloc_size < need_size) {
    div_t d    = std::div(need_size, STORE_MEMCHUNK_SIZE);
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

inline char *Store::GCAlloc(u_int size, u_int header, u_int gen) {
 retry:
  char *p      = (storeChunkMax + storeChunkTop);
  s_int newtop = (storeChunkTop + size);

  ((u_int *) p)[-1] = header;
  if (newtop >= 0) {
    AllocNewMemChunk(size, gen); // gen != header_gen
    goto retry;
  }
  SetInitMark(size);
  storeChunkTop = newtop;
  return p;
}

inline Block *Store::AllocFinSet(u_int size, u_int dst_gen, u_int cpy_gen) {
  u_int header = HeaderOp::EncodeHeader(MIN_DATA_LABEL, size, cpy_gen);
  Block *p     = (Block *) Store::GCAlloc((size + 1) * sizeof(u_int), header, dst_gen);

  AssertStore(p != INVALID_POINTER);
  // to be determined
  p->InitArg(0, 1);

  return p;
}

inline Block *Store::PushToFinSet(Block *p, Handler *h, word value, u_int dst_gen, u_int cpy_gen) {
  u_int size   = p->GetSize();
  u_int top    = Store::DirectWordToInt(p->GetArg(0));
  u_int newtop = (top + 2);
  Block *np;

  if (newtop >= size) {
    u_int newsize = ((size * 3) >> 1);
    
    np = Store::AllocFinSet(newsize, dst_gen, cpy_gen);
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
  Block *p = ((Block **) (storeChunkMax + storeChunkTop))[0];
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
  u_int size   = HeaderOp::DecodeSize(p);
  BlockLabel l = HeaderOp::DecodeLabel(p);
  u_int header = HeaderOp::EncodeHeader(l, size, cpy_gen);
  Block *newp  = (Block *) Store::GCAlloc(((1 + size) * sizeof(u_int)), header, dst_gen);

  std::memcpy(newp, p, size * sizeof(u_int));
#if defined(STORE_DEBUG)
  std::printf("MOVING BLOCK: src: `%s' and dst: `%s'\n",
	      LabelToString(p), LabelToString(newp));
#endif
  GCHelper::MarkMoved(p, newp);

  return newp;
}

inline word Store::ForwardBlock(word p, u_int dst_gen, u_int cpy_gen) {
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

void Store::ScanChunks(u_int dst_gen, u_int cpy_gen, MemChunk *anchor, char *scan) {
  while (!anchor->IsAnchor()) {
    // Scan current chunk
  scan_loop:
    Block *curp   = (Block *) scan;
    u_int cursize = HeaderOp::DecodeSize(curp);
    BlockLabel l  = curp->GetLabel();

    if (HeaderOp::GetHeader(curp) == HeaderOp::EncodeHeader(REF_LABEL, 0, 0)) {
      goto scan_next;
    }

    // Test for HandlerBlock must be done only once
    if (l == HANDLERBLOCK_LABEL) {
      PointerOp::DecodeHandler(curp)->PrepareForGC(curp);
    }
    
    // Scan current tuple (if not CHUNK or WEAK_DICT_LABEL)
    if ((l != CHUNK_LABEL) && (l != WEAK_DICT_LABEL)) {
      for (u_int i = cursize; i--;) {
	word p = PointerOp::Deref(curp->GetArg(i));
	
	if (!PointerOp::IsInt(p)) {
	  curp->InitArg(i, Store::ForwardBlock(p, dst_gen, cpy_gen));
	}
      }
    }
    scan += ((cursize + 1) * sizeof(u_int));
    goto scan_loop;

  scan_next:
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

  HeaderOp::SetChildishFlag(v);
  intgenSet = p->Push(v->ToWord())->ToWord();
}

void Store::RegisterWeakDict(WeakDictionary *v) {
  Set *p = Set::FromWord(wkDictSet);

  wkDictSet = p->Push(v->ToWord())->ToWord();
}

word Store::ResolveForwardPtr(word v) {
  word dv = PointerOp::Deref(v);

  if (!PointerOp::IsInt(dv)) {
    Block *p = PointerOp::RemoveTag(dv);

    if (HeaderOp::DecodeGeneration(p) == 0) {
      return PointerOp::EncodeTag(GCHelper::GetForwardPtr(p), PointerOp::DecodeTag(dv));
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
  u_int rs_size = new_intgen_set->GetSize();
  
  new_intgen_set->MakeEmpty();

#if defined(STORE_DEBUG)
  std::printf("initial intgen_size is %d\n", rs_size);
#endif
  
  // Traverse intgen_set entries
  for (u_int i = rs_size; i >= 1; i--) {
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
	    new_intgen_set->Push(p);
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
  std::printf("new_intgen_size is %d\n", new_intgen_set->GetSize());
#endif
}

inline Block *Store::HandleWeakDictionaries(Set *wkdict_set, Set *new_wkdict_set,
					    u_int dst_gen, u_int cpy_gen) {
  u_int rs_size = new_wkdict_set->GetSize();

  new_wkdict_set->MakeEmpty();

#if defined(STORE_DEBUG)
  std::printf("initial weakdict_size is %d\n", rs_size); 
#endif

  // Alloc Fin Set
  Block *finset = Store::AllocFinSet(120, dst_gen, cpy_gen);

  // Phase One: Forward all Dictionaries but not the contents
  for (u_int i = rs_size; i >= 1; i--) {
    word dict  = wkdict_set->GetArg(i);
    Block *dp  = Store::DirectWordToBlock(dict);
    word ndict;

    // Dictionary has been reached from Root Set and must kept alive
    if (GCHelper::AlreadyMoved(dp)) {
      ndict = PointerOp::EncodeTag(GCHelper::GetForwardPtr(dp), PointerOp::DecodeTag(dict));
      new_wkdict_set->Push(ndict);
    }
    // Dictionary might be finalized
    else if (HeaderOp::DecodeGeneration(dp) < dst_gen) {
      Handler *h  = dp->GetHandler();
      Block *newp = CopyBlockToDst(dp, dst_gen, cpy_gen);
	
      ndict = PointerOp::EncodeTag(newp, PointerOp::DecodeTag(dict));
      // Finalize only empty dict
      if (((WeakDictionary *) newp)->GetCounter() == 0) {
	finset = Store::PushToFinSet(finset, h, ndict, dst_gen, cpy_gen);
      }
      // Keep it alive (thanks to Denys for pointing that out)
      else {
	new_wkdict_set->Push(ndict);
      }
    }
    // Can't decide whether it was reached or not; must assume yes.
    else {
      ndict = dict;
      new_wkdict_set->Push(ndict);
    }
    // Keep Dict References complete for working
    wkdict_set->InitArg(i, ndict);

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
    WeakDictionary *dict = WeakDictionary::FromWordDirect(wkdict_set->GetArg(i));
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
	      finset = Store::PushToFinSet(finset, h,
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
    Store::ScanChunks(dst_gen, cpy_gen, anchor, scan);
  }

#if defined(STORE_DEBUG)
  std::printf("new_weakdict_size is %d\n", new_wkdict_set->GetSize());
#endif

  return finset;
}

inline void Store::DoGC(word &root, const u_int gcGen) {
  u_int dst_gen   = (gcGen + 1);
  u_int cpy_gen   = ((dst_gen == STORE_GENERATION_NUM) ? gcGen : dst_gen);
  Block *root_set = Store::WordToBlock(root);
  Set *intgen_set = Set::FromWord(intgenSet);
  Set *wkdict_set = Set::FromWord(wkDictSet);
  u_int rs_size   = root_set->GetSize(); // to be determined
  Block *new_root_set;
  Set *new_intgen_set;
  Set *new_wkdict_set;

  // Switch to the new Generation
  SwitchToNewChunk(roots[dst_gen]->GetNext());
  SetInitMark(0);

  // Copy Root-, Intgen- and WeakDict-Set to New Memory (if appropriate)
  new_root_set   = ((HeaderOp::DecodeGeneration(root_set) < dst_gen) ?
		    CopyBlockToDst(root_set, dst_gen, cpy_gen) : root_set);
  
  new_intgen_set = ((HeaderOp::DecodeGeneration((Block *) intgen_set) < dst_gen) ?
		    (Set *) CopyBlockToDst((Block *) intgen_set, dst_gen, cpy_gen) : intgen_set);
  
  new_wkdict_set = ((HeaderOp::DecodeGeneration((Block *) wkdict_set) < dst_gen) ?
		    (Set *) CopyBlockToDst((Block *) wkdict_set, dst_gen, cpy_gen) : intgen_set);

  // Obtain scan anchor
  MemChunk *anchor = storeCurChunk;
  char *scan       = (storeChunkMax + storeChunkTop);

  // Copy matching rootset entries
  for (u_int i = rs_size; i--;) {
    word p = PointerOp::Deref(new_root_set->GetArg(i));

    if (!PointerOp::IsInt(p)) {
      new_root_set->InitArg(i, Store::ForwardBlock(p, dst_gen, cpy_gen));
    }
  }
  
  // Scanning chunks (root_set amount)
  Store::ScanChunks(dst_gen, cpy_gen, anchor, scan);

  // Reset Anchor and Scan Ptr (to scan the new stuff)
  anchor = storeCurChunk;
  scan   = (storeChunkMax + storeChunkTop);

  // Handle InterGenerational Pointers
  Store::HandleInterGenerationalPointers(intgen_set, new_intgen_set, gcGen, dst_gen, cpy_gen);

  // change to the new intgenset
  Store::intgenSet = new_intgen_set->ToWord();

  // Scan chunks (intgen_set amount)
  Store::ScanChunks(dst_gen, cpy_gen, anchor, scan);

  // Handle Weak Dictionaries, if any (performs scanning itself)
  Block *arr = INVALID_POINTER;
  if (wkdict_set->GetSize() != 0) {
    arr = Store::HandleWeakDictionaries(wkdict_set, new_wkdict_set, dst_gen, cpy_gen);
  }

  // change to new dict list
  wkDictSet = new_wkdict_set->ToWord();
  
  // Clean up Collected regions
  for (u_int i = dst_gen; i--;) {
    u_int threshold = ((i == 0) ? 1 : 2);

#if defined(STORE_DEBUG)
    std::printf("Shrinking generation: %d\n", i);
#endif

    Store::Shrink(roots[i], threshold);
    memUsage[i] = ComputeUsage(roots[i]);
  }

  // Compute GC Flag (to be determined)
  needGC   = 0;
  maxGcGen = ((memUsage[dst_gen - 1] > memLimits[dst_gen - 1]) ? cpy_gen : 0);

  // Switch Semispaces
  if (dst_gen == (STORE_GENERATION_NUM - 1)) {
    MemChunk *tmp = roots[STORE_GENERATION_NUM - 2];
    roots[STORE_GENERATION_NUM - 2] = roots[STORE_GENERATION_NUM - 1];
    roots[STORE_GENERATION_NUM - 1] = tmp;

    u_int mem_tmp = memUsage[STORE_GENERATION_NUM - 2];
    memUsage[STORE_GENERATION_NUM - 2] = memUsage[STORE_GENERATION_NUM - 1];
    memUsage[STORE_GENERATION_NUM - 1] = mem_tmp;
  }

  // Switch back to Generation Zero and Adjust Root Set
  SwitchToNewChunk(roots[0]->GetNext());
  root = new_root_set->ToWord();

  // Call Finalization Handler
  if (arr != INVALID_POINTER) {
    rs_size = Store::WordToInt(arr->GetArg(1));
    for (u_int i = 2; i < rs_size; i += 2) {
      Handler *h = (Handler *) PointerOp::DecodeUnmanagedPointer(arr->GetArg(i));
      h->Finalize(arr->GetArg(i + 1));
    }
  }
}

void Store::DoGC(word &root) {
  switch (maxGcGen) {
  case STORE_GEN_YOUNGEST:
    DoGC(root, STORE_GEN_YOUNGEST);
    break;
  case STORE_GEN_OLDEST:
    DoGC(root, STORE_GEN_OLDEST);
    break;
  default:
    DoGC(root, maxGcGen);
  }
}

#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
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
      u_int size = (anchor->GetMax() - anchor->GetBottom());

      used  += (size + ((anchor == storeCurChunk) ? storeChunkTop : anchor->GetTop()));
      total += size;
      anchor = anchor->GetNext();
    }

    std::printf("G%d --> Used: %d; Total: %d\n", i, used, total);
  }
  std::printf("---\n");
  std::fflush(stdout);
}

void Store::ForceGCGen(u_int gen) {
  maxGcGen = gen;
}
#endif
