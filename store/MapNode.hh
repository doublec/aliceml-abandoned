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

  void SetLabel(BlockLabel l) {
    HeaderOp::EncodeLabel(reinterpret_cast<Transient *>(this), l);
  }
  BlockLabel GetLabel() {
    return HeaderOp::DecodeLabel(this);
  }
public:
  using Block::ToWord;

  bool IsHandled() {
    return (GetLabel() == HANDLEDHASHNODE_LABEL);
  }
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
  void MarkHandled() {
    SetLabel(HANDLEDHASHNODE_LABEL);
  }
  void MarkNormal() {
    SetLabel(HASHNODE_LABEL);
  }

  static MapNode *New(word key, word value, word next) {
    Block *p = Store::AllocBlock(HASHNODE_LABEL, SIZE);
    p->InitArg(KEY_POS, key);
    p->InitArg(VALUE_POS, value);
    p->InitArg(NEXT_POS, next);
    return STATIC_CAST(MapNode *, p);
  }
  static MapNode *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert((p->GetLabel() == HASHNODE_LABEL) ||
	   (p->GetLabel() == HANDLEDHASHNODE_LABEL));
    return STATIC_CAST(MapNode *, p);
  }
};

#endif
