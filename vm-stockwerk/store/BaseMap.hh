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
#ifndef __STORE__BASE_MAP_HH__
#define __STORE__BASE_MAP_HH__

#include "store/Store.hh"

typedef void (*item_apply)(word key, word item);

class MapNode;

template <class T>
class BaseMap : public Block {
protected:
  enum { COUNTER_POS, PERCENT_POS, TABLE_POS, RESERVED_POS, SIZE };

  // Adjust these two values to optimize runtime behaviour
  static const u_int INC_STEP    = 5;
  static const double FILL_RATIO = 0.75;

  u_int GetCounter() {
    return static_cast<u_int>(Store::DirectWordToInt(GetArg(COUNTER_POS)));
  }
  void SetCounter(u_int counter) {
    ReplaceArg(COUNTER_POS, counter);
  }
  u_int GetPercent() {
    return static_cast<u_int>(Store::DirectWordToInt(GetArg(PERCENT_POS)));
  }
  void SetPercent(u_int percent) {
    ReplaceArg(PERCENT_POS, percent);
  }
  Block *GetTable() {
    return Store::DirectWordToBlock(GetArg(TABLE_POS));
  }
  void SetTable(word t) {
    ReplaceArg(TABLE_POS, t);
  }
  u_int GetTableSize() {
    return static_cast<u_int>(Store::DirectWordToBlock(GetArg(TABLE_POS))
			      ->GetSize());
  }
  word GetEntry(u_int i) {
    Assert(i < GetTableSize());
    return GetTable()->GetArg(i);
  }
  void SetEntry(u_int i,  word entry) {
    Assert(i < GetTableSize());
    GetTable()->ReplaceArg(i, entry);
  }
  MapNode *FindKey(word key, word nodes, word & prev);
  void RemoveEntry(u_int i, word prev, MapNode *node);
  SeamDll void Resize();
public:
  void Put(word key, word value);
  void Remove(word key);

  bool IsMember(word key);
  word Get(word key);
  word CondGet(word key, word alternative); 

  u_int GetSize() {
    return (u_int) Store::WordToInt(GetArg(COUNTER_POS));
  }
  void Clear() {
    Block *arr = GetTable();
    u_int size = arr->GetSize();
    
    SetCounter(0);
    for (u_int i = size; i--;)
      arr->InitArg(i, 0);
  }

  bool IsEmpty() {
    return (GetCounter() == 0);
  }

  void Apply(item_apply func);

  static BaseMap<T> *New(BlockLabel l, u_int size);
};

// Hack Alert
#include "BaseMap.cc"

#endif
