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
#ifndef __ADT__CHUNK_MAP_HH__
#define __ADT__CHUNK_MAP_HH__

#if defined(INTERFACE)
#pragma interface "adt/ChunkMap.hh"
#endif

#include "store/BaseMap.hh"

class SeamDll ChunkKey {
public:
  static u_int Hash(word key, u_int size);
  static bool Equals(word a, word b);
};

class SeamDll ChunkMap: public BaseMap<ChunkKey> {
public:
  static ChunkMap *New(u_int size) {
    BaseMap<ChunkKey> *map = BaseMap<ChunkKey>::New(CHUNK_MAP_LABEL, size);
    return STATIC_CAST(ChunkMap *, map);
  }
  static ChunkMap *FromWord(word x) {
    Block *map = Store::WordToBlock(x);
    Assert(map == INVALID_POINTER || map->GetLabel() == CHUNK_MAP_LABEL);
    return STATIC_CAST(ChunkMap *, map);
  }
  static ChunkMap *FromWordDirect(word x) {
    Block *map = Store::DirectWordToBlock(x);
    Assert(map->GetLabel() == CHUNK_MAP_LABEL);
    return STATIC_CAST(ChunkMap *, map);
  }
};

#endif
