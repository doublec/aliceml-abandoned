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

#include "store/BaseMap.hh"
#include "store/MapNode.hh"

// Adjust this value to optimize runtime behaviour
static const double MAP_FILL_RATIO = 0.75;

template <typename T>
MapNode *BaseMap<T>::FindKey(word key, word nodes, word & prev) {
  while (nodes != Store::IntToWord(0)) {
    MapNode *node = MapNode::FromWordDirect(nodes);
    if (T::Equals(key, node->GetKey()))
      return node;
    prev  = nodes;
    nodes = node->GetNext();
  }
  return NULL;
}

template <typename T>
void BaseMap<T>::RemoveEntry(u_int i, word prev, MapNode *node) {
  if (prev == Store::IntToWord(0))
    SetEntry(i, node->GetNext());
  else
    MapNode::FromWordDirect(prev)->SetNext(node->GetNext());
  SetCounter(GetCounter() - 1);
}

template <typename T>
void BaseMap<T>::Resize() {
  u_int oldsize = GetTableSize();
  u_int newsize =  (3 * oldsize) >> 1;
  Assert(newsize > oldsize);
  Block *oldp   = GetTable();
  Block *newp   =
    Store::AllocMutableBlock((BlockLabel) HASHNODEARRAY_LABEL, newsize);
  // Correct possibly blown up size
  newsize = newp->GetSize();
  u_int percent = (u_int) (1 + (newsize * MAP_FILL_RATIO));
  SetPercent(percent);
  SetTable(newp->ToWord());
  // init the new table with zero
  for (u_int i = newsize; i--;)
    newp->InitArg(i, 0);
  // reinsert the items
  for (u_int i = oldsize; i--;) {
    word nodes = oldp->GetArg(i);
    while (nodes != Store::IntToWord(0)) {
      MapNode *node    = MapNode::FromWordDirect(nodes);
      word key         = node->GetKey();
      u_int hashedKey  = T::Hash(key, newsize);
      word keyNodes    = newp->GetArg(hashedKey);
      word value       = node->GetValue(); 
      word newKeyNodes = MapNode::New(key, value, keyNodes)->ToWord();
      newp->InitArg(hashedKey, newKeyNodes);
      nodes = node->GetNext();
    }
  }
}

template <typename T>
void BaseMap<T>::Put(word key, word value) {
  Assert(PointerOp::Deref(key) == key && !PointerOp::IsTransient(key));
  u_int counter = GetCounter();
  u_int percent = GetPercent();
  if (counter > percent)
    Resize();
  u_int hashedKey = T::Hash(key, GetTableSize());
  word nodes      = GetEntry(hashedKey);
  word prev       = Store::IntToWord(0);
  MapNode *entry  = FindKey(key, nodes, prev);
  if (entry != NULL)
    entry->Fill(key, value);
  else {
    SetEntry(hashedKey, MapNode::New(key, value, nodes)->ToWord());
    SetCounter(GetCounter() + 1);
  }
}

template <typename T>
void BaseMap<T>::Remove(word key) {
  Assert(PointerOp::Deref(key) == key && !PointerOp::IsTransient(key));
  u_int hashedKey = T::Hash(key, GetTableSize());
  word nodes      = GetEntry(hashedKey);
  word prev       = Store::IntToWord(0);
  MapNode *entry  = FindKey(key, nodes, prev);
  if (entry != NULL)
    RemoveEntry(hashedKey, prev, entry);
}

template <typename T>
bool BaseMap<T>::IsMember(word key) {
  Assert(PointerOp::Deref(key) == key && !PointerOp::IsTransient(key));
  u_int hashedKey = T::Hash(key, GetTableSize());
  word nodes      = GetEntry(hashedKey);
  word prev       = Store::IntToWord(0);
  return (FindKey(key, nodes, prev) != NULL);
}

template <typename T>
word BaseMap<T>::Get(word key) {
  Assert(PointerOp::Deref(key) == key && !PointerOp::IsTransient(key));
  u_int hashedKey = T::Hash(key, GetTableSize());
  word nodes      = GetEntry(hashedKey);
  word prev       = Store::IntToWord(0);
  MapNode *entry  = FindKey(key, nodes, prev);
  Assert(entry != NULL);
  return entry->GetValue();
}

template <typename T>
word BaseMap<T>::CondGet(word key, word alternative) {
  Assert(PointerOp::Deref(key) == key && !PointerOp::IsTransient(key));
  u_int hashedKey = T::Hash(key, GetTableSize());
  word nodes      = GetEntry(hashedKey);
  word prev       = Store::IntToWord(0);
  MapNode *entry  = FindKey(key, nodes, prev);
  return ((entry == NULL) ? alternative : entry->GetValue());
}

template <typename T>
void BaseMap<T>::Apply(item_apply func) {
  Block *table = GetTable();
  for (u_int i = table->GetSize(); i--;) {
    word nodes = table->GetArg(i);
    while (nodes != Store::IntToWord(0)) {
      MapNode *node = MapNode::FromWordDirect(nodes);
      func(node->GetKey(), node->GetValue());
      nodes = node->GetNext();
    }
  }
}

template <typename T>
BaseMap<T> *BaseMap<T>::New(BlockLabel l, u_int size) {
  size = ((size < 2) ? 2 : size); // Enforce Invariant: size must be > 1
  Block *map    = Store::AllocMutableBlock(l, SIZE);
  Block *arr    = Store::AllocMutableBlock(HASHNODEARRAY_LABEL, size);
  u_int percent = STATIC_CAST(u_int, size * MAP_FILL_RATIO);
  map->InitArg(COUNTER_POS, 0);
  map->InitArg(PERCENT_POS, percent);
  map->InitArg(TABLE_POS, arr->ToWord());
  for (u_int i = size; i--;)
    arr->InitArg(i, 0);
  return STATIC_CAST(BaseMap<T> *, map);
}
