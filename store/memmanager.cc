#include <iostream.h>
#include "memmanager.hh"
//
// Static Field Definitions
//
MemChain *MemManager::roots[MEMMAN_MAXGEN];
u_int MemManager::totalHeapSize;
u_int MemManager::needGC;
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
// Method Implementations
//
void MemManager::InitMemManager() {
  for (u_int i = 0; i < MEMMAN_MAXGEN; i++) {
    MemChain *chain = (MemChain *) malloc(sizeof(MemChain));

    chain->anchor = MakeList(2);
    chain->used   = 0;
    chain->gen    = i;
    roots[i]      = chain;
  }
  roots[MEMMAN_MAXGEN - 1]->gen = MEMMAN_MAXGEN - 2;
  totalHeapSize = MEMCHUNK_SIZE * 9;
  needGC        = 0;
}

void MemManager::CloseMemManager() {
  for (u_int i = 0; i < MEMMAN_MAXGEN; i++) {
    MemChain *chain = roots[i];

    ClearList(chain->anchor);
    free(chain);
  }
}

Tuple *MemManager::Alloc(MemChain *chain, u_int size) {
  MemChunk *list;

  Assert(size > 0);
  size = size << 2;
  Assert(chain != NULL);
  list = chain->anchor;

  if (!(list->FitsInChunk(size))) {
    u_int alloc_size = MEMCHUNK_SIZE;
    MemChunk *anchor = chain->anchor;

    if (alloc_size < size) {
      div_t d    = div(size, MEMCHUNK_SIZE);
      alloc_size = d.quot + (d.rem ? 1 : 0);
    }
    totalHeapSize += alloc_size;
    chain->anchor = list = new MemChunk(NULL, anchor, alloc_size);
    anchor->SetPrev(list);
    // GC Problem follows
  }
  chain->used += size;

#ifdef DEBUG_CHECK
  {
    char *tmp = list->AllocChunkItem(size);
    memset(tmp, 0, size);
    return (Tuple *) tmp;
  }
#else
  return (Tuple *) list->AllocChunkItem(size);
#endif
}

u_int MemManager::NeedGC() {
  return needGC;
}

MemChunk *MemManager::Shrink(MemChunk *list, int threshold) {
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
      delete list;
    }
    else {
      list->MakeEmpty();
    }
    list = next;
  }

  return anchor;
}
