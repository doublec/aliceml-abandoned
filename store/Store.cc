#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include "store.hh"
#include "gchelper.hh"
//
// Helper Class Implementation
//
const unsigned int HeaderDef::GEN_LIMIT[] = { 0x0FFFFFFF, 0x4FFFFFFF, 0x8FFFFFFF };
DynamicTuple *Tuple::intgen_set;
//
// Method Implementations
//
Tuple *Store::CopyBlockToDst(Tuple *p, MemChain *dst) {
  t_size s = HeaderOp::BlankDecodeSize(p);
  
  if (s == HeaderDef::MAX_HBSIZE) {
    Tuple *newp, *realnp;
    
    p      = (Tuple *) ((char *) p - 4);
    s      = *((t_size *) p);
    newp   = MemManager::Alloc(dst, (s + 2));
    realnp = (Tuple *) ((char *) newp + 4);
    
    memcpy(newp, p, (s + 2) << 2);
    GCHelper::EncodeGen(realnp, dst->gen);
    return realnp;
  }
  else {
    Tuple *newp = MemManager::Alloc(dst, (s + 1));
    
    memcpy(newp, p, (s + 1) << 2);
    GCHelper::EncodeGen(newp, dst->gen);
    return newp;
  }
}

void Store::ScanChunks(MemChain *dst, u_int match_gen, MemChunk *anchor, char *scan) {
  while (anchor != NULL) {
    // Scan current chunk
    while (scan < anchor->GetTop()) {
      Tuple *curp    = (Tuple *) scan;
      word assumed_s = *((word *) curp);
      t_size cursize;
      
      // Find next header
      if (PointerOp::IsInt(assumed_s)) {
	cursize = (t_size) PointerOp::DecodeInt(assumed_s);
	curp    = (Tuple *) ((char *) scan + 4);
      }
      else {
	cursize = HeaderOp::BlankDecodeSize(curp);
      }
      
      // Scan current tuple
      for (u_int i = 1; i <= cursize; i++) {
	t_field tf = Store::GenTField(i);
	word p     = PointerOp::Deref(curp->GetArg(tf));
	Tuple *sp  = PointerOp::RemoveTag(p);
	
	if (!PointerOp::IsInt(p) && (HeaderOp::GetHeader(sp) <= match_gen)) {
	  if (GCHelper::AlreadyMoved(sp)) {
	    curp->InitArg(tf, GCHelper::GetForwardPtr(sp));
	  }
	  else {
	    Tuple *newsp = CopyBlockToDst(sp, dst);
	    word newp    = PointerOp::EncodeTag(newsp, PointerOp::DecodeTag(p));
	    
	    GCHelper::MarkMoved(sp, newp);
	    curp->InitArg(tf, newp);
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

void Store::InitStore() {
  MemManager::InitMemManager();
  Tuple::intgen_set = new DynamicTuple(64);
}

void Store::DoGC(DynamicTuple *root_set, u_int gen) {
  u_int match_gen  = HeaderDef::GEN_LIMIT[gen];
  u_int dst_gen    = (gen + 1);
  MemChain *dst    = roots[dst_gen];
  u_int rs_size    = root_set->GetSize();
  MemChunk *anchor = dst->anchor;
  char *scan       = anchor->GetTop(); // First top is scan

  // Copy matching root_set entries
  for (u_int i = 0; i < rs_size; i++) {
    word p    = PointerOp::Deref(root_set->GetArg(i));
    Tuple *sp = PointerOp::RemoveTag(p); 

    if (HeaderOp::GetHeader(sp) <= match_gen) {
      Tuple *newsp = CopyBlockToDst(sp, dst);
      word newp    = PointerOp::EncodeTag(newsp, PointerOp::DecodeTag(p));
      GCHelper::MarkMoved(sp, newp);
      root_set->SetArg(i, newp);
    }
  }
  
  // Scanning chunks (root_set amount)
  Store::ScanChunks(dst, match_gen, anchor, scan);

  // Reset Anchor and Scan Ptr (for new stuff)
  anchor = dst->anchor;
  scan   = anchor->GetTop();

  // Handle InterGenerational Pointers
  rs_size = Tuple::intgen_set->GetSize();
  for (u_int i = 0; i < rs_size; i++) {
    Tuple *curp   = PointerOp::RemoveTag(PointerOp::Deref(Tuple::intgen_set->GetArg(i)));
    u_int cursize = curp->GetSize();
    
    for (u_int k = 1; k <= cursize; k++) {
      t_field tf = Store::GenTField(k);
      word fp    = PointerOp::Deref(curp->GetArg(tf));
      Tuple *fsp = PointerOp::RemoveTag(fp);
      
      if (!PointerOp::IsInt(fp) && (HeaderOp::GetHeader(fsp) <= match_gen)) {
	if (GCHelper::AlreadyMoved(fsp)) {
	  curp->InitArg(tf, GCHelper::GetForwardPtr(fsp));
	}
	else {
	  Tuple *newfsp = CopyBlockToDst(fsp, dst);
	  word newfp    = PointerOp::EncodeTag(newfsp, PointerOp::DecodeTag(fp));

	  GCHelper::MarkMoved(fsp, newfp);
	  curp->InitArg(tf, newfp);
	}
      }
    }
  }
  Tuple::intgen_set->Clear();
  // Scan chunks (Tuple::intgen_set amount)
  Store::ScanChunks(dst, match_gen, anchor, scan);

  // Clean up collected regions
  for (u_int i = 0; i < dst_gen; i++) {
    MemChain *cur = roots[i];

#ifdef DEBUG_CHECK
    {
      MemChunk *cur = roots[i]->anchor;

      while (cur != NULL) {
	cur->FillZero();
	cur = cur->GetNext();
      }
      
    }
#endif
    
    cur->anchor = MemManager::Shrink(cur->anchor, 2);
    cur->used   = 0;
  }

  // switch semispaces
  if (dst_gen == (MEMMAN_MAXGEN - 1)) {
    MemChain *tmp = roots[MEMMAN_MAXGEN - 2];
    roots[MEMMAN_MAXGEN - 2] = roots[MEMMAN_MAXGEN - 1];
    roots[MEMMAN_MAXGEN - 1] = tmp;
  }
}

#ifdef DEBUG_CHECK
void Store::MemStat() {
  cout << "Store Statistics\n";
  cout << "Overall Memory available: " << totalHeapSize  << " Bytes\n";
  cout << "G0 Memory used:           " << roots[0]->used << " Bytes\n";
  cout << "G1 Memory used:           " << roots[1]->used << " Bytes\n";
  cout << "G2 Memory used (src):     " << roots[2]->used << " Bytes\n";
  cout << "G2 Memory used (dst):     " << roots[3]->used << " Bytes\n";
}
#endif
