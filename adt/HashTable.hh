//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#ifndef __ADT__HASHTABLE_HH__
#define __ADT__HASHTABLE_HH__

#if defined(INTERFACE)
#pragma interface "adt/HashTable.hh"
#endif
#include "adt/HashTable.hh"

#include "store/Store.hh"

class HashNode : private Block {
private:
  static const u_int SIZE    = 2;
  static const u_int KEY_POS = 1;
  static const u_int VAL_POS = 2;
public:
  using Block::ToWord;

  void MakeEmpty() {
    InitArg(KEY_POS, Store::IntToWord(-1));
  }
  int IsEmpty() {
    return (Store::WordToInt(GetArg(KEY_POS)) == - 1);
  }
  word GetKey() {
    return GetArg(KEY_POS);
  }
  void SetKey(word key) {
    ReplaceArg(KEY_POS, key);
  }
  word GetValue() {
    return GetArg(VAL_POS);
  }
  void SetValue(word value) {
    ReplaceArg(VAL_POS, value);
  }

  static HashNode *FromBlock(Block *x) {
    return (HashNode *) x;
  }
  static HashNode *New() {
    Block *p = Store::AllocBlock(HASHNODE_LABEL, SIZE);

    p->InitArg(KEY_POS, Store::IntToWord(-1));
    p->InitArg(VAL_POS, Store::IntToWord(0));

    return FromBlock(p);
  }
  static HashNode *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == HASHNODE_LABEL));
    return FromBlock(p);
  }
};

class HashTable : private Block {
public:
  enum hashkeytype {
    INT_KEY,
    BLOCK_KEY
  };
private:
  static const u_int SIZE        = 4;
  static const u_int COUNTER_POS = 1;
  static const u_int PERCENT_POS = 2;
  static const u_int TYPE_POS    = 3;
  static const u_int TABLE_POS   = 4;
  //
  // Adjust these two values to optimize runtime behaviour
  //
  static const u_int INC_STEP    = 5;
  static const double FILL_RATIO = 0.75;
protected:
  static u_int IncKey(u_int key, u_int size);
  static u_int NextPrime(u_int p);

  u_int FindKey(u_int i);
  u_int FindKey(Block *b);
  u_int FindKey(word key);
  void Resize();

  u_int GetCounter() {
    return (u_int) Store::WordToInt(GetArg(COUNTER_POS));
  }
  void SetCounter(u_int counter) {
    InitArg(COUNTER_POS, Store::IntToWord(counter));
  }
  u_int GetPercent() {
    return (u_int) Store::WordToInt(GetArg(PERCENT_POS));
  }
  hashkeytype GetKeyType() {
    return (hashkeytype) Store::WordToInt(GetArg(TYPE_POS));
  }
  HashNode *GetEntry(u_int i) {
    Assert(i >= 1);
    Assert(i <= GetTableSize());
    return HashNode::FromWord(Store::WordToBlock(GetArg(TABLE_POS))->GetArg(i));
  }
public:
  using Block::ToWord;

  void InsertItem(word key, word value);
  void DeleteItem(word key);
  int IsMember(word key);
  word GetItem(word key); // must be member

  u_int GetSize() {
    return (u_int) Store::WordToInt(GetArg(COUNTER_POS));
  }
  u_int GetTableSize() {
    return (u_int) Store::WordToBlock(GetArg(TABLE_POS))->GetSize();
  }
  void Clear() {
    Block *arr = Store::WordToBlock(GetArg(TABLE_POS));
    u_int size = arr->GetSize();
    
    InitArg(COUNTER_POS, Store::IntToWord(0));
    for (u_int i = 1; i <= size; i++) {
      HashNode::FromWord(GetArg(i))->MakeEmpty();
    }
  }

  static HashTable *New(hashkeytype type, u_int size);
  static HashTable *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == HASHTABLE_LABEL));
    return (HashTable *) p;
  }
};

#endif __ADT__HASHTABLE_HH__
