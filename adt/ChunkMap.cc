//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "adt/ChunkMap.hh"
#endif

#include <cstring>
#include "adt/ChunkMap.hh"
#include "store/BaseMap.cc"

template class BaseMap<ChunkKey>;

// String hashing function is taken from
// 'Aho, Sethi, Ullman: Compilers..., page 436
static inline u_int HashString(const char *s, u_int len, u_int size) {
  const char *sm = (s + len);
  unsigned h = 0, g;
  for (const char *p = s; p < sm; p++) {
    h = (h << 4) + (*p);
    if ((g = h & 0xf0000000)) {
      h = h ^ (g >> 24);
      h = h ^ g;
    }
  }
  return (h % size);
}

u_int ChunkKey::Hash(word key, u_int size) {
  Chunk *chunk = Store::DirectWordToChunk(key);
  return HashString(static_cast<const char *>(chunk->GetBase()),
		    chunk->GetSize(), size);
}

bool ChunkKey::Equals(word a, word b) {
  Chunk *ac = Store::DirectWordToChunk(a);
  Chunk *bc = Store::DirectWordToChunk(b);
  u_int al = ac->GetSize();
  u_int bl = bc->GetSize();
  return ((al == bl) ?
	  (std::memcmp(ac->GetBase(), bc->GetBase(), al) == 0) : false);
}
