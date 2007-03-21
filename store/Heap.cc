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

#include <cstring>

#if HAVE_VIRTUALALLOC
# include <windows.h>
#else
# include <cstdlib>
# include <unistd.h>

#if HAVE_LIGHTNING
# include <sys/mman.h> // mprotect
# include <errno.h>
#endif
#endif

#include "store/Base.hh"
#include "store/Types.hh"
#include "store/HeaderOp.hh"
#include "store/Heap.hh"
#include "store/StatusWord.hh"

#if (!HAVE_VIRTUALALLOC) && HAVE_LIGHTNING
# define PAGE_SIZE 4096
# undef STORE_MEM_ALIGN
# define STORE_MEM_ALIGN PAGE_SIZE
#endif

void HeapChunk::Alloc(u_int size) {
#if HAVE_VIRTUALALLOC
  block = (char *) VirtualAlloc(NULL, size,
				MEM_RESERVE | MEM_COMMIT,
#if HAVE_LIGHTNING
				PAGE_READWRITE
#else
				PAGE_EXECUTE_READWRITE
#endif
				);
#else
  block = (char *) std::malloc(size);
#endif
  Assert(block != NULL);
}

void HeapChunk::Free() {
#if HAVE_VIRTUALALLOC
  if (VirtualFree(block, 0, MEM_RELEASE) != TRUE) {
    Assert(false); // to be done
  }
#else
  std::free(block);
#endif
}

HeapChunk::HeapChunk(u_int size, HeapChunk *chain) : prev(NULL) {
  Alloc(size + STORE_MEM_ALIGN);
  AssertStore(block != NULL);
  // Align base pointer
  base = block;
  base += (STORE_MEM_ALIGN - ((u_int) base & (STORE_MEM_ALIGN - 1)));
  max  = (base + size - sizeof(word)); // Header must always fit
  top  = base;
  next = chain;
#if HAVE_LIGHTNING && !HAVE_VIRTUALALLOC
  // Recent Linux kernels disallow EXEC by default
  if (mprotect(base, (max - base + sizeof(word)),
	       PROT_READ | PROT_WRITE | PROT_EXEC) == -1) {
    switch (errno) {
    case EINVAL:
      Error("mprotect: invalid pointer or not a multiple of PAGESIZE");
    case EFAULT:
      Error("mprotect: the memory cannot be accessed");
    case EACCES:
      Error("mprotect: The memory cannot be given the specified access");
    case ENOMEM:
      Error("mprotect: internal kernel structures could not be allocated");
    default:
      Error("mprotect: unknown error");
    }
  }
#endif
  if (chain != NULL)
    chain->SetPrev(this);
  memset(base, 1, size);
}

HeapChunk::~HeapChunk() {
  Free();
}

u_int Heap::total = 0;

Heap::Heap(u_int chunkSize, u_int limit) {
  chain       = new HeapChunk(chunkSize, NULL);
  size        = chunkSize;
  this->limit = limit;
  total       += size;
}

Heap::~Heap() {
  total -= size;
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
  total += chunkSize;
}

void Heap::Shrink() {
  HeapChunk *chunks = chain;
  while (chunks != NULL) {
    HeapChunk *next = chunks->GetNext();
    delete chunks;
    chunks = next;
  }
  chain = new HeapChunk(STORE_MEMCHUNK_SIZE, NULL);
  total -= size - STORE_MEMCHUNK_SIZE;
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
