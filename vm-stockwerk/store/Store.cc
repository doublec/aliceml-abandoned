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
u_int MemChunk::counter =  0;


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

static inline u_int ComputeUsage(MemChunk *chain) {
  u_int used = 0;

  while (chain != NULL) {
    used++;
    chain = chain->GetNext();
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
  else {
    storeCurChunk->SetTop(storeChunkTop);
  }

  return top;
}

void Store::AllocNewMemChunk(u_int size, u_int gen) {
  MemChunk *chain = roots[gen];

  // Adjust ChunkTop
  storeChunkTop -= size;

  // Compute necessary MemChunk Size (size must fit in)
  u_int alloc_size = STORE_MEMCHUNK_SIZE;
  if (alloc_size < size) {
    div_t d    = std::div(size, STORE_MEMCHUNK_SIZE);
    alloc_size = ((d.quot + (d.rem ? 1 : 0)) * STORE_MEMCHUNK_SIZE);
  }

  // Perform the Alloc
  roots[gen] = chain = new MemChunk(NULL, chain, alloc_size);
  memUsage[gen]++;

  // Reset ChunkTop and ChunkMax to allow next FastAlloc
  SwitchToNewChunk(chain);
}

MemChunk *Store::Shrink(MemChunk *list, int threshold) {
  MemChunk *anchor = list;
  
  while (list != NULL) {
    MemChunk *prev = list->GetPrev();
    MemChunk *next = list->GetNext();
    
    if (threshold-- >= 0) {
      std::fprintf(stderr, "clearing...\n");
      list->Clear();
    }
    else {
      if (prev == NULL) {
	anchor = next;
	if (next != NULL) {
	  next->SetPrev(NULL);
	}
      }
      else {
	prev->SetNext(next);
	if (next != NULL) {
	  next->SetPrev(prev);
	}
      }
      delete list;
    }

    list = next;
  }

  return anchor;
}

inline Block *Store::CopyBlockToDst(Block *p, u_int dst_gen, u_int cpy_gen) {
  u_int s     = HeaderOp::DecodeSize(p);
  Block *newp = (Block *) Store::GCAlloc(((s + 1) << 2), dst_gen);
    
  std::memcpy(newp, p, (s + 1) * sizeof(word));
  GCHelper::EncodeGen(newp, cpy_gen);

  return newp;
}

void Store::ScanChunks(u_int dst_gen, u_int cpy_gen,
		       u_int match_gen, MemChunk *anchor, char *scan) {
  while (anchor != NULL) {
    // Scan current chunk
    while (scan < anchor->GetTop()) {
      Block *curp   = (Block *) scan;
      u_int cursize = HeaderOp::DecodeSize(curp);
      
      // Scan current tuple (if label != CHUNK)
      if (curp->GetLabel() != CHUNK_LABEL) {
	for (u_int i = 1; i <= cursize; i++) {
	  word p    = PointerOp::Deref(curp->GetArg(i));
	  Block *sp = PointerOp::RemoveTag(p);

	  if ((!PointerOp::IsInt(p)) && (HeaderOp::GetHeader(sp) <= match_gen)) {
	    if (GCHelper::AlreadyMoved(sp)) {
	      curp->InitArg(i, GCHelper::GetForwardPtr(sp));
	    }
	    else {
	      Block *newsp = CopyBlockToDst(sp, dst_gen, cpy_gen);
	      word newp    = PointerOp::EncodeTag(newsp, PointerOp::DecodeTag(p));
	    
	      GCHelper::MarkMoved(sp, newp);
	      curp->InitArg(i, newp);
	    }
	  }
	}
      }
      scan += ((cursize + 1) * sizeof(word));
    }
    anchor = anchor->GetPrev();
    if (anchor != NULL) {
      scan = anchor->GetBottom();
    }
  }
}

void Store::InitStore(u_int memLimits[STORE_GENERATION_NUM]) {
  for (u_int i = 0; i < STORE_GENERATION_NUM; i++) {
    roots[i] = new MemChunk(NULL, NULL, STORE_MEMCHUNK_SIZE);
    Store::memLimits[i] = memLimits[i];
  }

  // Prepare Fast Memory Allocation
  MemChunk *anchor = roots[0];
  storeChunkTop = anchor->GetTop();
  storeChunkMax = anchor->GetMax();
  storeCurChunk = anchor;

  // Alloc Intgen-Set
  intgenSet = Set::New(STORE_INTGEN_SIZE)->ToWord();
}

void Store::CloseStore() {
  for (u_int i = 0; i < STORE_GENERATION_NUM; i++) {
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

inline void Store::SwitchToNewChunk(MemChunk *chain) {
  storeCurChunk->SetTop(storeChunkTop);
  storeChunkTop = chain->GetTop();
  storeChunkMax = chain->GetMax();
  storeCurChunk = chain;
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
  MemChunk *chain = roots[dst_gen];
  SwitchToNewChunk(chain);

  // Copy Root- and Intgen-Set to New Memory (if appropriate)
  new_root_set = ((HeaderOp::GetHeader(root_set) <= match_gen) ?
		  CopyBlockToDst(root_set, dst_gen, cpy_gen) : root_set);
  
  new_intgen_set = ((HeaderOp::GetHeader((Block *) intgen_set) <= match_gen) ?
		    (Set *) CopyBlockToDst((Block *) intgen_set, dst_gen, cpy_gen) : intgen_set);

  // Obtain scan anchor
  MemChunk *anchor = roots[dst_gen];
  char *scan       = anchor->GetTop(); // First top is scan

  // Copy matching rootset entries
  for (u_int i = 1; i <= rs_size; i++) {
    word p = PointerOp::Deref(root_set->GetArg(i));

    if (!PointerOp::IsInt(p)) {
      Block *sp = PointerOp::RemoveTag(p);

      if (HeaderOp::GetHeader(sp) <= match_gen) {
	Block *newsp = CopyBlockToDst(sp, dst_gen, cpy_gen);
	word newp    = PointerOp::EncodeTag(newsp, PointerOp::DecodeTag(p));
	GCHelper::MarkMoved(sp, newp);
	new_root_set->InitArg(i, newp);
      }
      else {
	new_root_set->InitArg(i, p);
      }
    }
  }
  
  // Scanning chunks (root_set amount)
  Store::ScanChunks(dst_gen, cpy_gen, match_gen, anchor, scan);

  // Reset Anchor and Scan Ptr (for new stuff)
  anchor = roots[dst_gen];
  scan   = anchor->GetTop();

  // Handle InterGenerational Pointers
  rs_size = intgen_set->GetSize();
  new_intgen_set->MakeEmpty();

  for (u_int i = 2; i <= rs_size; i++) {
    word dp = PointerOp::Deref(intgen_set->GetArg(i));

    if (!PointerOp::IsInt(dp)) {
      Block *curp   = PointerOp::RemoveTag(dp);
      u_int cursize = curp->GetSize();

      for (u_int k = 1; k <= cursize; k++) {
	word fp    = PointerOp::Deref(curp->GetArg(k));
	Block *fsp = PointerOp::RemoveTag(fp);
	
	if (!PointerOp::IsInt(fp) && (HeaderOp::GetHeader(fsp) <= match_gen)) {
	  if (GCHelper::AlreadyMoved(fsp)) {
	    curp->InitArg(k, GCHelper::GetForwardPtr(fsp));
	  }
	  else {
	    Block *newfsp = CopyBlockToDst(fsp, dst_gen, cpy_gen);
	    word newfp    = PointerOp::EncodeTag(newfsp, PointerOp::DecodeTag(fp));
	    
	    GCHelper::MarkMoved(fsp, newfp);
	    curp->InitArg(k, newfp);
	  }
	}
      }

      // Test for entry removal
      if (HeaderOp::DecodeGeneration(curp) > gcGen) {
	new_intgen_set->Push(dp); // resize never happens ??
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
  
  // Save current top
  roots[dst_gen]->SetTop(storeChunkTop);

  // Clean up Collected regions
  for (u_int i = 0; i < dst_gen; i++) {
    u_int newUsage = ((i == 0) ? 1 : 2);
    
    roots[i]    = Store::Shrink(roots[i], newUsage);
    memUsage[i] = ComputeUsage(roots[i]);
  }

  // Compute GC Flag
  if (memUsage[dst_gen] > memLimits[dst_gen]) {
    gcGen = cpy_gen;
  }
  else {
    gcGen = 0;
  }

  // Switch Semispaces
  if (dst_gen != cpy_gen) {
    MemChunk *tmp = roots[STORE_GENERATION_NUM - 2];

    roots[STORE_GENERATION_NUM - 2] = roots[STORE_GENERATION_NUM - 1];
    roots[STORE_GENERATION_NUM - 1] = tmp;
  }

  // Adjust Fast Memory Allocation
  SwitchToNewChunk(roots[0]);

  return new_root_set->ToWord();
}

#ifdef DEBUG_CHECK
void Store::MemStat() {
  static const char *val[] = { "no", "yes" };

  std::printf("---\n");
  std::printf("GC necessary: %s \n", val[NeedGC()]);
  std::printf("---\n");
  for (u_int i = 0; i < STORE_GENERATION_NUM; i++) {
    MemChunk *anchor = roots[i];
    u_int used       = 0;
    u_int total      = 0;
    
    while (anchor != NULL) {
      std::printf("Scanning Id: %d\n", anchor->id); 
      used  += (anchor->GetTop() - anchor->GetBottom());
      total += (anchor->GetMax() - anchor->GetBottom());
      anchor = anchor->GetNext();
    }

    std::printf("G%d --> Used: %d; Total: %d\n", i, used, total);
  }
  std::printf("---\n");
  std::fflush(stdout);
}

void Store::StoreTop() {
  storeCurChunk->SetTop(storeChunkTop);
}

void Store::ForceGCGen(u_int gen) {
  gcGen = gen;
}
#endif
