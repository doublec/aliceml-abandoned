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
#pragma implementation "store.hh"
#endif
#include "store.hh"

#if defined(INTERFACE)
#pragma implementation "gchelper.hh"
#endif
#include "gchelper.hh"

//
// Helper Functions
//
static MemChunk *MakeList(u_int l) {
  MemChunk *list = NULL;

  while (l-- > 0) {
    MemChunk *next = list;
    list = new MemChunk(NULL, next, MEMCHUNK_SIZE);
    if (next != NULL) {
      next->SetPrev(list);
    }
  }

  return list;
}

static void ClearList(MemChunk *list) {
  while (list != NULL) {
    MemChunk *tmp = list->GetNext();

    delete list;
    list = tmp;
  }
}
//
// Helper Class Implementation
//
StoreConfig *Store::config = INVALID_POINTER;
MemChain **Store::roots    = INVALID_POINTER;
u_int Store::totalHeapSize = 0;

word Store::intgen_set     = (word) 1;
u_int Store::needGC        = 0;
//
// Method Implementations
//
Block *Store::Alloc(MemChain *chain, u_int size) {
  MemChunk *list;

  Assert(size > 0);
  size <<= 2;

  Assert(chain != NULL);
  list = chain->anchor;

  if (!(list->FitsInChunk(size))) {
    u_int alloc_size = MEMCHUNK_SIZE;
    MemChunk *anchor = list;

    if (alloc_size < size) {
      div_t d    = std::div(size, MEMCHUNK_SIZE);
      alloc_size = (d.quot + (d.rem ? 1 : 0)) * MEMCHUNK_SIZE;
    }
    
    needGC = (chain->total + alloc_size >= config->gen_limits[chain->gen]);

    totalHeapSize += alloc_size;
    chain->total  += alloc_size;
    chain->anchor = list = new MemChunk(NULL, anchor, alloc_size);
    anchor->SetPrev(list);
  }
  chain->used += size;
  
  return (Block *) list->AllocChunkItem(size);
}

void Store::Shrink(MemChain *chain, int threshold) {
  MemChunk *list   = chain->anchor;
  MemChunk *anchor = list;
  
  while (list != NULL) {
    MemChunk *prev = list->GetPrev();
    MemChunk *next = list->GetNext();
    
    if (threshold-- <= 0) {
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
      totalHeapSize -= list->GetSize();
      chain->total  -= list->GetSize();
      delete list;
    }
    else {
      list->MakeEmpty();
    }
    list = next;
  }

  chain->anchor = anchor;
}

Block *Store::CopyBlockToDst(Block *p, MemChain *dst) {
    u_int s = HeaderOp::BlankDecodeSize(p);

    if (p->GetLabel() != STACK) {
      if (s == (u_int) MAX_HBSIZE) {
	Block *newp, *realnp;
	
	p      = (Block *) ((word *) p - 1);
	s      = *((u_int *) p);
	newp   = Store::Alloc(dst, (s + 2));
	realnp = (Block *) ((word *) newp + 1);
      
	std::memcpy(newp, p, (s + 2) * sizeof(word));
	GCHelper::EncodeGen(realnp, dst->gen);
	return realnp;
      }
      else {
	Block *newp = Store::Alloc(dst, (s + 1));
	
	std::memcpy(newp, p, (s + 1) * sizeof(word));
	GCHelper::EncodeGen(newp, dst->gen);
	return newp;
      }
    }
    else {
      u_int rs = (u_int) (Store::WordToInt(p->GetArg(1)) - 1);

      if (s == (u_int) MAX_HBSIZE) {
	Block *newp, *realnp;
	
	p      = (Block *) ((word *) p - 1);
	newp   = Store::Alloc(dst, (rs + 2));
	realnp = (Block *) ((word *) newp + 1);
      
	std::memcpy(newp, p, (rs + 2) * sizeof(word));
	GCHelper::EncodeGen(realnp, dst->gen);
	GCHelper::AdjustBigSize(realnp, rs);
	return realnp;
      }
      else {
	Block *newp = Store::Alloc(dst, (rs + 1));
	
	std::memcpy(newp, p, (rs + 1) * sizeof(word));
	GCHelper::EncodeGen(newp, dst->gen);
	GCHelper::AdjustSmallSize(newp, rs);
	return newp;
      }
    }
}

void Store::ScanChunks(MemChain *dst, u_int match_gen, MemChunk *anchor, char *scan) {
  while (anchor != NULL) {
    // Scan current chunk
    while (scan < anchor->GetTop()) {
      Block *curp    = (Block *) scan;
      word assumed_s = *((word *) curp);
      u_int cursize;
      
      // Find next header
      if (PointerOp::IsInt(assumed_s)) {
	cursize = (u_int) PointerOp::DecodeInt(assumed_s);
	curp    = (Block *) ((word *) scan + 1);
      }
      else {
	cursize = HeaderOp::BlankDecodeSize(curp);
      }
      
      // Scan current tuple (if label != CHUNK)
      if (curp->GetLabel() != CHUNK) {
	for (u_int i = 1; i <= cursize; i++) {
	  word p    = PointerOp::Deref(curp->GetArg(i));
	  Block *sp = PointerOp::RemoveTag(p);
	
	  if ((!PointerOp::IsInt(p)) && (HeaderOp::GetHeader(sp) <= match_gen)) {
	    if (GCHelper::AlreadyMoved(sp)) {
	      curp->InitArg(i, GCHelper::GetForwardPtr(sp));
	    }
	    else {
	      Block *newsp = CopyBlockToDst(sp, dst);
	      word newp    = PointerOp::EncodeTag(newsp, PointerOp::DecodeTag(p));
	    
	      GCHelper::MarkMoved(sp, newp);
	      curp->InitArg(i, newp);
	    }
	  }
	}
      }
      scan += (cursize + ((cursize >= (u_int) MAX_HBSIZE) ? 2 : 1)) << 2;
    }
    anchor = anchor->GetPrev();
    if (anchor != NULL) {
      scan = anchor->GetBottom();
    }
  }
}

void Store::InitStore(StoreConfig *cfg) {
  config = cfg;
  roots  = (MemChain **) std::malloc(sizeof(MemChain) * cfg->max_gen);

  for (u_int i = 0; i < cfg->max_gen; i++) {
    MemChain *chain = new MemChain();

    chain->anchor = MakeList(2);
    chain->total  = MEMCHUNK_SIZE * 2;
    chain->used   = 0;
    chain->gen    = i;
    roots[i]      = chain;

    totalHeapSize += chain->total;
  }
  roots[cfg->max_gen - 1]->gen = cfg->max_gen - 2;
  intgen_set = Set::New(cfg->intgen_size)->ToWord();
}

void Store::CloseStore() {
  for (u_int i = 0; i < config->max_gen; i++) {
    MemChain *chain = roots[i];

    ClearList(chain->anchor);
    std::free(chain);
  }
}

void Store::AddToIntgenSet(Block *v) {
  HeaderOp::SetIntgenMark(v);
  Stack::FromWord(intgen_set)->SlowPush(v->ToWord());
}

word Store::DoGC(word root, u_int gen) {
  PLACEGENERATIONLIMIT;
  u_int match_gen  = gen_limits[gen];
  u_int dst_gen    = (gen + 1);
  MemChain *dst    = roots[dst_gen];
  Block *root_set  = Store::WordToBlock(root);
  Set *intgen_set  = Set::FromWord(Store::intgen_set);
  u_int rs_size    = root_set->GetSize();
  Block *new_root_set;
  Set *new_intgen_set;

  // Copy tuples to new memory
  if (HeaderOp::GetHeader(root_set) <= match_gen) {
    new_root_set = CopyBlockToDst(root_set, dst);
  }
  else {
    new_root_set = root_set;
  }
  if (HeaderOp::GetHeader((Block *) intgen_set) <= match_gen) {
    new_intgen_set = (Set *) CopyBlockToDst((Block *) intgen_set, dst);
  }
  else {
    new_intgen_set = intgen_set;
  }

  // Obtain scan anchor
  MemChunk *anchor = dst->anchor;
  char *scan       = anchor->GetTop(); // First top is scan

  // Copy matching rootset entries
  for (u_int i = 1; i <= rs_size; i++) {
    word p = PointerOp::Deref(root_set->GetArg(i));

    if (!PointerOp::IsInt(p)) {
      Block *sp = PointerOp::RemoveTag(p);

      if (HeaderOp::GetHeader(sp) <= match_gen) {
	Block *newsp = CopyBlockToDst(sp, dst);
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
  Store::ScanChunks(dst, match_gen, anchor, scan);

  // Reset Anchor and Scan Ptr (for new stuff)
  anchor = dst->anchor;
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
	    Block *newfsp = CopyBlockToDst(fsp, dst);
	    word newfp    = PointerOp::EncodeTag(newfsp, PointerOp::DecodeTag(fp));
	    
	    GCHelper::MarkMoved(fsp, newfp);
	    curp->InitArg(k, newfp);
	  }
	}
      }

      // Test for entry removal
      if (HeaderOp::DecodeGeneration(curp) > gen) {
	new_intgen_set->Push(dp);
      }
      else {
	HeaderOp::ClearIntgenMark(curp);
      }
    }
  }

  // change to the new intgenset
  Store::intgen_set = new_intgen_set->ToWord();

  // Scan chunks (Tuple::intgen_set amount)
  Store::ScanChunks(dst, match_gen, anchor, scan);

  // Clean up collected regions
  for (u_int i = 0; i < dst_gen; i++) {
    MemChain *cur = roots[i];
    
    Store::Shrink(cur, 2);
    cur->used = 0;
  }

  // switch semispaces
  if (dst_gen == (config->max_gen - 1)) {
    MemChain *tmp = roots[config->max_gen - 2];
    roots[config->max_gen - 2] = roots[config->max_gen - 1];
    roots[config->max_gen - 1] = tmp;
  }

  return new_root_set->ToWord();
}

#ifdef DEBUG_CHECK
void Store::MemStat() {
  static char *val[] = { "no", "yes" };

  cout << "---\n";
  cout << "GC necessary: " << val[needGC] << "\n";
  cout << "---\n";
  cout << "Overall Memory available: " << totalHeapSize  << " Bytes\n---\n";
  for (u_int i = 0; i < config->max_gen; i++) {
    cout << "G" << i << "--> Used: " << roots[i]->used << "; Total: " << roots[i]->total << "\n";
  }
  cout << "---\n";
}
#endif
