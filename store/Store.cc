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

#if defined(INTERFACE)
#pragma implementation "store/Store.hh"
#endif
#include "store/Store.hh"

#if defined(INTERFACE)
#pragma implementation "store/Memory.hh"
#endif
#include "store/Memory.hh"

#if defined(DEBUG_CHECK)
u_int MemChunk::counter =  0;
#endif

#if defined(INTERFACE)
#pragma implementation "store/HeaderOp.hh"
#endif
#include "store/HeaderOp.hh"

#if defined(INTERFACE)
#pragma implementation "store/PointerOp.hh"
#endif
#include "store/PointerOp.hh"

#if defined(INTERFACE)
#pragma implementation "store/GCHelper.hh"
#endif
#include "store/GCHelper.hh"

#if defined(INTERFACE)
#pragma implementation "store/Value.hh"
#endif
#include "store/Value.hh"

#if defined(INTERFACE)
#pragma implementation "store/Set.hh"
#endif
#include "store/Set.hh"

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

  // Prepare for next GCAlloc
  SwitchToNewChunk(newChunk);
}

void Store::Shrink(MemChunk *list, int threshold) {
  list = list->GetNext();

  while (!list->IsAnchor()) {
    MemChunk *next = list->GetNext();

    if (threshold-- > 0) {
#if defined(DEBUG_CHECK)
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

#if defined(DEBUG_CHECK)
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
  Block *newp = (Block *) Store::GCAlloc((size << 2), dst_gen);
    
  std::memcpy(newp, p, (size * sizeof(word)));
  GCHelper::EncodeGen(newp, cpy_gen);

#if defined(DEBUG_CHECK)
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
      
      // Scan current tuple (if label != CHUNK)
      if (curp->GetLabel() != CHUNK_LABEL) {
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
  intgenSet = Set::New(STORE_INTGEN_SIZE)->ToWord();
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

void Store::SwitchToNewChunk(MemChunk *chunk) {
  storeCurChunk->SetTop(storeChunkTop);
  storeChunkTop = chunk->GetTop();
  storeChunkMax = chunk->GetMax();
  storeCurChunk = chunk;
}

word Store::DoGC(word root) {
  PLACEGENERATIONLIMIT;
  u_int match_gen = gen_limits[gcGen];
  u_int dst_gen   = (gcGen + 1);
  u_int cpy_gen   = ((dst_gen == (STORE_GENERATION_NUM - 1)) ? (dst_gen - 1) : dst_gen);
  Block *root_set = Store::WordToBlock(root);
  Set *intgen_set = Set::FromWord(intgenSet);
  u_int rs_size   = root_set->GetSize();
  Block *new_root_set;
  Set *new_intgen_set;

  // Switch to the new Generation
  SwitchToNewChunk(roots[dst_gen]->GetNext());

  // Copy Root- and Intgen-Set to New Memory (if appropriate)
  new_root_set = ((HeaderOp::GetHeader(root_set) <= match_gen) ?
		  CopyBlockToDst(root_set, dst_gen, cpy_gen) : root_set);
  
  new_intgen_set = ((HeaderOp::GetHeader((Block *) intgen_set) <= match_gen) ?
		    (Set *) CopyBlockToDst((Block *) intgen_set, dst_gen, cpy_gen) : intgen_set);

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
  rs_size = intgen_set->GetSize();
  new_intgen_set->MakeEmpty();

#if defined(DEBUG_CHECK)
  std::printf("intgen_size is %d\n", rs_size);
#endif

  // Traverse intgen_set entries
  for (u_int i = 2; i <= rs_size; i++) {
    word p = PointerOp::Deref(intgen_set->GetArg(i));

    if (!PointerOp::IsInt(p)) {
      word dp       = Store::ForwardBlock(p, dst_gen, cpy_gen, match_gen);
      Block *curp   = PointerOp::RemoveTag(dp);
      u_int cursize = curp->GetSize();

      // Traverse intgen_set entry for references
      for (u_int k = 1; k <= cursize; k++) {
	word fp = PointerOp::Deref(curp->GetArg(k));
	
	if (!PointerOp::IsInt(fp)) {
	  curp->InitArg(k, Store::ForwardBlock(fp, dst_gen, cpy_gen, match_gen));
	}
      }

      // Test for entry removal
      if (HeaderOp::DecodeGeneration(curp) > gcGen) {
	new_intgen_set->Push(dp);
      }
      else {
	HeaderOp::ClearIntgenMark(curp);
      }
    }
  }

  // change to the new intgenset
  Store::intgenSet = new_intgen_set->ToWord();

  // Scan chunks (Tuple::intgen_set amount)
  Store::ScanChunks(dst_gen, cpy_gen, match_gen, anchor, scan);
  
  // Clean up Collected regions
  for (u_int i = 0; i <= gcGen; i++) {
    u_int threshold = ((i == 0) ? 1 : 2);

#if defined(DEBUG_CHECK)    
    std::printf("Shrinking generation: %d\n", i);
#endif
    Store::Shrink(roots[i], threshold);
    memUsage[i] = ComputeUsage(roots[i]);
  }

  // Compute GC Flag
  needGC = 0;
  if (memUsage[dst_gen] > memLimits[dst_gen]) {
    gcGen = cpy_gen;
  }
  else {
    gcGen = 0;
  }

  // Switch Semispaces
  if (dst_gen == (STORE_GENERATION_NUM - 1)) {
    MemChunk *tmp = roots[STORE_GENERATION_NUM - 2];

    roots[STORE_GENERATION_NUM - 2] = roots[STORE_GENERATION_NUM - 1];
    roots[STORE_GENERATION_NUM - 1] = tmp;
  }

  // Switch back to Generation Zero
  SwitchToNewChunk(roots[0]->GetNext());

  return new_root_set->ToWord();
}

#if defined(DEBUG_CHECK)
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
