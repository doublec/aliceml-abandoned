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
#ifndef __STORE__MAP_NODE_HH__
#define __STORE__MAP_NODE_HH__

#if defined(INTERFACE)
#pragma interface "store/MapNode.hh"
#endif

#include "store/Store.hh"

class MapNode : private Block {
protected:
  enum { KEY_POS, VALUE_POS, NEXT_POS, SIZE };
public:
  using Block::ToWord;

  word GetKey() {
    return GetArg(KEY_POS);
  }
  word GetValue() {
    return GetArg(VALUE_POS);
  }
  word GetNext() {
    return GetArg(NEXT_POS);
  }
  void SetValue(word value) {
    ReplaceArg(VALUE_POS, value);
  }
  void SetNext(word next) {
    ReplaceArg(NEXT_POS, next);
  }
  void SetNextDirect(word next) {
    InitArg(NEXT_POS, next);
  }
  void Fill(word key, word value) {
    ReplaceArg(KEY_POS, key);
    ReplaceArg(VALUE_POS, value);
  }

  static MapNode *New(word key, word value, word next) {
    Block *p = Store::AllocMutableBlock(HASHNODE_LABEL, SIZE);
    p->InitArg(KEY_POS, key);
    p->InitArg(VALUE_POS, value);
    p->InitArg(NEXT_POS, next);
    return STATIC_CAST(MapNode *, p);
  }
  static MapNode *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert(p->GetLabel() == HASHNODE_LABEL);
    return STATIC_CAST(MapNode *, p);
  }
};

#endif
