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
#ifndef __STORE__MAP_HH__
#define __STORE__MAP_HH__

#if defined(INTERFACE)
#pragma interface "store/Map.hh"
#endif

#include "store/BaseMap.hh"

class WordKey {
public:
  static u_int Hash(word key, u_int size) {
    Assert(size > 0);
    return ((u_int) key % size);
  }
  static bool Equals(word a, word b) {
    return a == b;
  }
};

class SeamDll Map : public BaseMap<WordKey> {
protected:
  static word mapLs;

  friend class Store;
  void Rehash();
  static void RehashAll(const u_int gen);
public:
  static void Init();
  static Map *New(u_int size);

  static Map *FromWord(word x) {
    Block *map = Store::WordToBlock(x);
    Assert(map == INVALID_POINTER || map->GetLabel() == MAP_LABEL);
    return STATIC_CAST(Map *, map);
  }
  static Map *FromWordDirect(word x) {
    Block *map = Store::DirectWordToBlock(x);
    Assert(map->GetLabel() == MAP_LABEL);
    return STATIC_CAST(Map *, map);
  }
};

#endif
