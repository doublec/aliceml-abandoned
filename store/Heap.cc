//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "store/Heap.hh"
#endif

#include "store/Base.hh"
#include "store/Types.hh"
#include "store/HeaderOp.hh"
#include "store/Heap.hh"
#include "store/StatusWord.hh"

#include <cstring>

#if HAVE_VIRTUALALLOC
#include <windows.h>

void HeapChunk::Alloc(u_int size) {
  block = (char *) VirtualAlloc(NULL, size,
				(MEM_RESERVE | MEM_COMMIT),
				PAGE_READWRITE);
}

void HeapChunk::Free() {
  if (block && VirtualFree(block, 0, MEM_RELEASE) != TRUE) {
    // to be done
  }
}
#else
#include <cstdlib>
#include <unistd.h>
#include <sys/mman.h>

void HeapChunk::Alloc(size_t size) {
#if defined(STORE_ALLOC_MMAP)
  block = (char *) mmap(NULL, size,
			(PROT_READ | PROT_WRITE), MAP_PRIVATE,
			-1, (off_t) 0);
#else
  block = (char *) std::malloc(size);
#endif
}

void HeapChunk::Free() {
#if defined(STORE_ALLOC_MMAP)
  munmap(block, (u_int) (max - base));
#else
  std::free(block);
#endif
}
#endif

HeapChunk::HeapChunk(u_int size, HeapChunk *chain) : prev(NULL) {
  Alloc(size + STORE_MEM_ALIGN);
  AssertStore(block != NULL);
  // Align base pointer
  base = block;
  base += (STORE_MEM_ALIGN - ((u_int) base & (STORE_MEM_ALIGN - 1)));
  max  = (base + size - sizeof(word)); // Header must always fit
  top  = base;
  next = chain;
  if (chain != NULL)
    chain->SetPrev(this);
  memset(base, 1, size);
}

HeapChunk::~HeapChunk() {
  Free();
}

Heap::Heap(u_int chunkSize, u_int limit) {
  chain       = new HeapChunk(chunkSize, NULL);
  size        = chunkSize;
  this->limit = limit;
}

Heap::~Heap() {
  return;
}

void Heap::Enlarge() {
  // Compute itemSize
  Block *p = (Block *) chain->GetTop();
  u_int itemSize = SIZEOF_BLOCK(HeaderOp::DecodeSize(p));
  // Compute required HeapChunk Size
  u_int chunkSize = STORE_MEMCHUNK_SIZE;
  itemSize += sizeof(u_int);
  if (chunkSize < itemSize) {
    div_t d   = div(itemSize, STORE_MEMCHUNK_SIZE);
    chunkSize = ((d.quot + (d.rem ? 1 : 0)) * STORE_MEMCHUNK_SIZE);
  }
  chain = new HeapChunk(chunkSize, chain);
  // Hack alert
  if (size >= limit)
    StatusWord::SetStatus(1);
  size += chunkSize;
}

void Heap::Shrink() {
  HeapChunk *chunks = chain;
  while (chunks != NULL) {
    HeapChunk *next = chunks->GetNext();
    delete chunks;
    chunks = next;
  }
  chain = new HeapChunk(STORE_MEMCHUNK_SIZE, NULL);
  size  = STORE_MEMCHUNK_SIZE;
}

u_int Heap::GetExactSize() {
  HeapChunk *chunks = chain;
  u_int size = 0;
  while (chunks != NULL) {
    size += chunks->GetTop() - chunks->GetBase();
    chunks = chunks->GetNext();
  }
  return size;
}
