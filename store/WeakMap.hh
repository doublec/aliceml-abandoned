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
#ifndef __STORE__WEAK_MAP_HH__
#define __STORE__WEAK_MAP_HH__

#if defined(INTERFACE)
#pragma interface "store/WeakMap.hh"
#endif

#include "store/BaseMap.hh"

class TokenKey {
public:
  static u_int Hash(word key, u_int size) {
    Assert(PointerOp::IsInt(key));
    return ((u_int) key % size);
  }
  static bool Equals(word a, word b) {
    return a == b;
  }
};

class SeamDll Finalization {
public:
  virtual void Finalize(word value) = 0;
};

class SeamDll WeakMap : public BaseMap<TokenKey> {
protected:
  friend class Store;

  word GetHandler() {
    return STATIC_CAST(Block *, this)->GetArg(RESERVED_POS);
  }
public:
  static WeakMap *New(u_int size, Finalization *handler) {
    BaseMap<TokenKey> *map = BaseMap<TokenKey>::New(WEAK_MAP_LABEL, size);
    STATIC_CAST(Block *, map)
      ->InitArg(RESERVED_POS, Store::UnmanagedPointerToWord(handler));
    Store::RegisterWeakDict(STATIC_CAST(WeakMap *, map));
    return STATIC_CAST(WeakMap *, map);
  }
  static WeakMap *FromWord(word x) {
    Block *map = Store::WordToBlock(x);
    Assert(map == INVALID_POINTER || map->GetLabel() == WEAK_MAP_LABEL);
    return STATIC_CAST(WeakMap *, map);
  }
  static WeakMap *FromWordDirect(word x) {
    Block *map = Store::DirectWordToBlock(x);
    Assert(map->GetLabel() == WEAK_MAP_LABEL);
    return STATIC_CAST(WeakMap *, map);
  }
};

#endif
