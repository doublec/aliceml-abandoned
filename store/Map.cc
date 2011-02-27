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
#pragma implementation "store/Map.hh"
#pragma implementation "store/MapNode.hh"
#endif

#include "store/Map.hh"
#include "store/BaseMap.cc"
#include "store/GCHelper.hh"

template class BaseMap<WordKey>;

class ListNode : private Block {
protected:
  enum { MAP_POS, NEXT_POS, SIZE };
public:
  using Block::ToWord;

  Block *GetMap() {
    // This might be an forward ptr; therefore we cannot use Map::FromWordDirect
    return Store::DirectWordToBlock(GetArg(MAP_POS));
  }
  word GetNext() {
    return GetArg(NEXT_POS);
  }
  static const u_int GetSize() {
    return SIZE;
  }
  void Init(Map *map, word next) {
    InitArg(MAP_POS, map->ToWord());
    InitArg(NEXT_POS, next);
  }
  static ListNode *New(Map *map, word next) {
    Block *p = Store::AllocBlock(MIN_DATA_LABEL, SIZE);
    p->InitArg(MAP_POS, map->ToWord());
    p->InitArg(NEXT_POS, next);
    return static_cast<ListNode *>(p);
  }
  static ListNode *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert(p->GetLabel() == MIN_DATA_LABEL);
    return static_cast<ListNode *>(p);
  }
};

word Map::mapLs;

void Map::Rehash() {
  Block *table = GetTable();
  u_int size   = table->GetSize();
  for (u_int i = size; i--;) {
    word nodes = table->GetArg(i);
    word prev  = Store::IntToWord(0);
    while (nodes != Store::IntToWord(0)) {
      MapNode *node   = MapNode::FromWordDirect(nodes);
      word key        = node->GetKey();
      u_int hashedKey = reinterpret_cast<u_int>(key) % size;
      if (hashedKey != i) {
	nodes = node->GetNext(); // Order is important
	// Remove from old chain
	if (prev == Store::IntToWord(0))
	  table->ReplaceArg(i, nodes);
	else
	  MapNode::FromWordDirect(prev)->SetNext(nodes);
	// Insert into new chain
	node->SetNext(table->GetArg(hashedKey));
	table->ReplaceArg(hashedKey, node->ToWord());
      }
      else {
	prev = nodes;
	nodes = node->GetNext();
      }
    }
  }
}

void Map::RehashAll(const u_int gen) {
  word oldMapLs = mapLs;
  mapLs = Store::IntToWord(0);
  while (oldMapLs != Store::IntToWord(0)) {
    ListNode *node = ListNode::FromWordDirect(oldMapLs);
    Block *map     = node->GetMap();
    // Get current map ptr
    if (GCHelper::AlreadyMoved(map))
      map = GCHelper::GetForwardPtr(map);
    else if (HeaderOp::DecodeGeneration(map) < gen)
      map = INVALID_POINTER;
    // Do rehash only if map is alive
    if (map != INVALID_POINTER) {
      Map *newMap = static_cast<Map *>(map);
      newMap->Rehash();
      ListNode *node =
        reinterpret_cast<ListNode *>(Store::Alloc(gen, MIN_DATA_LABEL, ListNode::GetSize()));
      node->Init(newMap, mapLs);
      mapLs = node->ToWord();
    }
    oldMapLs = node->GetNext();
  }
}

void Map::Init() {
  mapLs = Store::IntToWord(0);
}

Map *Map::New(u_int size) {
  Map *map = static_cast<Map *>(BaseMap<WordKey>::New(MAP_LABEL, size));
  mapLs = ListNode::New(map, mapLs)->ToWord();
  return map;
}
