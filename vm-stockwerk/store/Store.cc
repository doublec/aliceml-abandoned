#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include "store.hh"
#include "tuple.hh"
//
// Field Definitions
//
DynamicTuple *Store::intgen_set;
//
// Helper Class Implementation
//
const unsigned int HeaderDef::GEN_LIMIT[] = { 0x0FFFFFFF, 0x4FFFFFFF, 0x8FFFFFFF };
//
// Method Implementations
//
void Store::ScanChunks(MemChain *dst, u_int match_gen, MemChunk *anchor, char *scan) {
  while (anchor != NULL) {
    // Scan current chunk
    while (scan < anchor->GetTop()) {
      b_pointer curp = (b_pointer) scan;
      word assumed_s = *((word *) curp);
      t_size cursize;
      
      // Find next header
      if (Helper::IsInt(assumed_s)) {
	cursize = (t_size) Helper::DecodeInt(assumed_s);
	curp    = (b_pointer) ((char *) scan + 4);
      }
      else {
	cursize = Helper::BlankDecodeSize(curp);
      }
      
      // Scan current tuple
      for (u_int i = 1; i <= cursize; i++) {
	t_field tf   = Store::GenTField(i);
	word p       = Helper::Deref(Store::GetArg(curp, tf));
	b_pointer sp = Helper::RemoveTag(p);
	
	if (!Helper::IsInt(p) && (Helper::GetHeader(sp) <= match_gen)) {
	  if (Helper::AlreadyMoved(sp)) {
	    Store::SetArg(curp, tf, Helper::GetForwardPtr(sp));
	  }
	  else {
	    b_pointer newsp = Helper::CopyBlockToDst(sp, dst);
	    word newp       = Helper::EncodeTag(newsp, Helper::DecodeTag(p));
	    
	    Helper::MarkMoved(sp, newp);
	    Store::SetArg(curp, tf, newp);
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

void Store::DoGC(DynamicTuple *root_set, u_int gen) {
  u_int match_gen  = HeaderDef::GEN_LIMIT[gen];
  u_int dst_gen    = (gen + 1);
  MemChain *dst    = roots[dst_gen];
  u_int rs_size    = root_set->GetSize();
  MemChunk *anchor = dst->anchor;
  char *scan       = anchor->GetTop(); // First top is scan

  // Copy matching root_set entries
  for (u_int i = 0; i < rs_size; i++) {
    word p       = Helper::Deref(root_set->GetArg(i));
    b_pointer sp = Helper::RemoveTag(p); 

    if (Helper::GetHeader(sp) <= match_gen) {
      b_pointer newsp = Helper::CopyBlockToDst(sp, dst);
      word newp       = Helper::EncodeTag(newsp, Helper::DecodeTag(p));
      Helper::MarkMoved(sp, newp);
      root_set->SetArg(i, newp);
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
    b_pointer curp = Helper::RemoveTag(Helper::Deref(intgen_set->GetArg(i)));
    u_int cursize  = Store::GetSize(curp);
    
    for (u_int k = 1; k <= cursize; k++) {
      t_field tf    = Store::GenTField(k);
      word fp       = Helper::Deref(Store::GetArg(curp, tf));
      b_pointer fsp = Helper::RemoveTag(fp);
      
      if (!Helper::IsInt(fp) && (Helper::GetHeader(fsp) <= match_gen)) {
	if (Helper::AlreadyMoved(fsp)) {
	  Store::SetArg(curp, tf, Helper::GetForwardPtr(fsp));
	}
	else {
	  b_pointer newfsp = Helper::CopyBlockToDst(fsp, dst);
	  word newfp       = Helper::EncodeTag(newfsp, Helper::DecodeTag(fp));

	  Helper::MarkMoved(fsp, newfp);
	  Store::SetArg(curp, tf, newfp);
	}
      }
    }
  }
  intgen_set->Clear();
  // Scan chunks (intgen_set amount)
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
