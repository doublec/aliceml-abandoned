#include <iostream.h>
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

DataSet *Store::intgen_set = INVALID_POINTER;
u_int Store::needGC        = 0;
//
// Method Implementations
//
Block *Store::Alloc(MemChain *chain, u_int size) {
  MemChunk *list;
  char *tmp;
  int gen;

  Assert(size > 0);
  size = size << 2;

  Assert(chain != NULL);
  list = chain->anchor;

  if (!(list->FitsInChunk(size))) {
    u_int alloc_size = MEMCHUNK_SIZE;
    MemChunk *anchor = chain->anchor;

    if (alloc_size < size) {
      div_t d    = std::div(size, MEMCHUNK_SIZE);
      alloc_size = (d.quot + (d.rem ? 1 : 0)) * MEMCHUNK_SIZE;
    }
    
    needGC = (chain->total + alloc_size >= config->gen_limits[chain->gen]);

    totalHeapSize += alloc_size;
    chain->total  += alloc_size;
    chain->anchor = list = new MemChunk(NULL, anchor, alloc_size);
    list->InitBlock(alloc_size);
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

  if (s == HeaderDef::MAX_HBSIZE) {
    Block *newp, *realnp;
    
    p      = (Block *) ((char *) p - 4);
    s      = *((u_int *) p);
    newp   = Store::Alloc(dst, (s + 2));
    realnp = (Block *) ((char *) newp + 4);
    
    std::memcpy(newp, p, (s + 2) << 2);
    GCHelper::EncodeGen(realnp, dst->gen);
    return realnp;
  }
  else {
    Block *newp = Store::Alloc(dst, (s + 1));
    
    std::memcpy(newp, p, (s + 1) << 2);
    GCHelper::EncodeGen(newp, dst->gen);
    return newp;
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
	curp    = (Block *) ((char *) scan + 4);
      }
      else {
	cursize = HeaderOp::BlankDecodeSize(curp);
      }
      
      // Scan current tuple (if label != CHUNK)
      if (curp->GetLabel() != BlockLabel::CHUNK) {
	for (u_int i = 1; i <= cursize; i++) {
	  word p    = PointerOp::Deref(curp->GetArg(i));
	  Block *sp = PointerOp::RemoveTag(p);
	
	  if (!PointerOp::IsInt(p) && (HeaderOp::GetHeader(sp) <= match_gen)) {
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
      scan += (cursize + ((cursize > HeaderDef::MAX_HBSIZE) ? 2 : 1)) << 2;
    }
    anchor = anchor->GetPrev();
    if (anchor != NULL) {
      scan = anchor->GetBottom();
    }
  }
}

void Store::InitStore(StoreConfig *cfg) {
  static unsigned int lim[] = { 0x0FFFFFFF, 0x4FFFFFFF, 0x8FFFFFFF };
  config = cfg;
  roots  = (MemChain **) std::malloc(sizeof(MemChain) * cfg->max_gen);

  HeaderDef::CreateHeader(cfg->word_width, cfg->tag_width, cfg->size_width, cfg->generation_width);

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
  intgen_set = new DataSet();
}

void Store::CloseStore() {
  for (u_int i = 0; i < config->max_gen; i++) {
    MemChain *chain = roots[i];

    ClearList(chain->anchor);
    std::free(chain);
  }
}

void Store::DoGC(DataSet *root_set, u_int gen) {
  u_int match_gen  = HeaderDef::GEN_LIMIT[gen];
  u_int dst_gen    = (gen + 1);
  MemChain *dst    = roots[dst_gen];
  u_int rs_size    = root_set->GetSize();
  MemChunk *anchor = dst->anchor;
  char *scan       = anchor->GetTop(); // First top is scan

  // Copy matching root_set entries
  for (u_int i = 0; i < rs_size; i++) {
    word p    = PointerOp::Deref(root_set->GetArg(i));
    Block *sp = PointerOp::RemoveTag(p);

    if (HeaderOp::GetHeader(sp) <= match_gen) {
      Block *newsp = CopyBlockToDst(sp, dst);
      word newp    = PointerOp::EncodeTag(newsp, PointerOp::DecodeTag(p));
      GCHelper::MarkMoved(sp, newp);
      root_set->SetArg(i, newp);
    }
    else {
      root_set->SetArg(i, p);
    }
  }
  
  // Scanning chunks (root_set amount)
  Store::ScanChunks(dst, match_gen, anchor, scan);

  // Reset Anchor and Scan Ptr (for new stuff)
  anchor = dst->anchor;
  scan   = anchor->GetTop();

  // Handle InterGenerational Pointers
  rs_size = intgen_set->GetSize();
  for (u_int i = 0; i < rs_size; i++) {
    Block *curp   = PointerOp::RemoveTag(PointerOp::Deref(intgen_set->GetArg(i)));
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
  }
  intgen_set->Clear();
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
}

#ifdef DEBUG_CHECK
void Store::MemStat() {
  cout << "Store Statistics\n";
  cout << "Overall Memory available: " << totalHeapSize  << " Bytes\n---\n";
  for (u_int i = 0; i < config->max_gen; i++) {
    cout << "G" << i << "--> Used: " << roots[i]->used << "; Total: " << roots[i]->total << "\n";
  }
  cout << "---\n";
}
#endif
