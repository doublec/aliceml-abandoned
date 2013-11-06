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
#include "store/MapNode.hh"

typedef void (*item_apply)(word key, word item);
typedef word (*item_map)(word key, word item);

class MapNode;

template <typename T>
class SeamDll BaseMap : public Block {
protected:
  enum { COUNTER_POS, PERCENT_POS, TABLE_POS, RESERVED_POS, SIZE };

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
    return static_cast<u_int>(Store::DirectWordToBlock(GetArg(TABLE_POS))->GetSize());
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
  SeamMemberDll void Resize();
public:
  SeamMemberDll void Put(word key, word value);
  SeamMemberDll void Remove(word key);

  SeamMemberDll bool IsMember(word key);
  SeamMemberDll word Get(word key);
  SeamMemberDll word CondGet(word key, word alternative = INVALID_POINTER); 

  u_int GetSize() {
    return static_cast<u_int>(Store::WordToInt(GetArg(COUNTER_POS)));
  }
  void Clear() {
    Block *arr = GetTable();
    u_int size = arr->GetSize();
    
    SetCounter(0);
    for (u_int i = size; i--;)
      arr->InitArg(i, static_cast<s_int>(0));
  }

  bool IsEmpty() {
    return (GetCounter() == 0);
  }

  SeamMemberDll void Apply(item_apply func);
  
  /**
   * In-place partial map: if func returns INVALID_POINTER then the key is removed.
   */
  SeamMemberDll void PartialMap(item_map func);
  
  /**
   * Type-safe fold (iteration) over elements of the map.
   */
  template <typename A>
  A Fold(A (*func)(word key, word item, A accum), A accum){
    Block *table = GetTable();
    for (u_int i=table->GetSize(); i--; ) {
      
      word nodes = table->GetArg(i);
      while (nodes != Store::IntToWord(0)) {
	MapNode *node = MapNode::FromWordDirect(nodes);
	accum = func(node->GetKey(), node->GetValue(), accum);
	nodes = node->GetNext();
      }
    }
    return accum;
  }

  static SeamMemberDll BaseMap<T> *New(BlockLabel l, u_int size);
};

#endif

