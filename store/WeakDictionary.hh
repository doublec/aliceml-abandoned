//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#ifndef __STORE__WEAKDICTIONARY_HH__
#define __STORE__WEAKDICTIONARY_HH__

#if defined(INTERFACE)
#pragma interface "store/WeakDictionary.hh"
#endif

#include "store/Store.hh"

class DllExport Finalization {
public:
  virtual void Finalize(word value) = 0;
};

class DllExport HashNode : private Block {
private:
  static const u_int KEY_POS   = 0;
  static const u_int VALUE_POS = 1;
  static const u_int NEXT_POS  = 2;
  static const u_int SIZE      = 3;

  void SetLabel(BlockLabel l) {
    HeaderOp::EncodeLabel((Transient *) this, l);
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

  static HashNode *New(word key, word value, word next) {
    Block *p = Store::AllocBlock(HASHNODE_LABEL, SIZE);
    p->InitArg(KEY_POS, key);
    p->InitArg(VALUE_POS, value);
    p->InitArg(NEXT_POS, next);
    return (HashNode *) p;
  }
  static HashNode *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert((p->GetLabel() == HASHNODE_LABEL) ||
	   (p->GetLabel() == HANDLEDHASHNODE_LABEL));
    return (HashNode *) p;
  }
};

class DllExport BTListNode : private Block {
private:
  static const u_int TABLE_POS = 0;
  static const u_int NEXT_POS  = 1;
  static const u_int SIZE      = 2;
public:
  using Block::ToWord;

  word GetTable() {
    return GetArg(TABLE_POS);
  }
  word GetNext() {
    return GetArg(NEXT_POS);
  }
  static const u_int GetSize() {
    return SIZE;
  }
  void Init(word table, word next) {
    InitArg(TABLE_POS, table);
    InitArg(NEXT_POS, next);
  }
  static BTListNode *New(word table, word next) {
    Block *p = Store::AllocBlock(MIN_DATA_LABEL, SIZE);
    p->InitArg(TABLE_POS, table);
    p->InitArg(NEXT_POS, next);
    return (BTListNode *) p;
  }
  static BTListNode *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert(p->GetLabel() == MIN_DATA_LABEL);
    return (BTListNode *) p;
  }
};

typedef void (*item_apply)(word key, word item);

class DllExport WeakDictionary : private Block {
public:
  enum hashkeytype {
    INT_KEY,
    BLOCK_KEY,
    WORD_KEY
  };
  // this is to allow inlining
  u_int GetTableSize() {
    return (u_int) Store::DirectWordToBlock(GetArg(TABLE_POS))->GetSize();
  }
protected:
  static const u_int HANDLER_POS = 0;
  static const u_int COUNTER_POS = 1;
  static const u_int PERCENT_POS = 2;
  static const u_int TYPE_POS    = 3;
  static const u_int TABLE_POS   = 4;
  static const u_int SIZE        = 5;
  //
  // Adjust these two values to optimize runtime behaviour
  //
  static const u_int INC_STEP    = 5;
  static const double FILL_RATIO = 0.75;

  friend class Store;

  u_int HashInt(u_int i);
  u_int HashBlock(Block *b);
  u_int HashKey(word key);
  HashNode *FindKey(word key, word nodes, word & prev);
  void Resize();

  word GetHandler() {
    return GetArg(HANDLER_POS);
  }
  u_int GetCounter() {
    return (u_int) Store::DirectWordToInt(GetArg(COUNTER_POS));
  }
  void SetCounter(u_int counter) {
    InitArg(COUNTER_POS, counter);
  }
  u_int GetPercent() {
    return (u_int) Store::DirectWordToInt(GetArg(PERCENT_POS));
  }
  void SetPercent(u_int percent) {
    InitArg(PERCENT_POS, percent);
  }
  hashkeytype GetKeyType() {
    return (hashkeytype) Store::DirectWordToInt(GetArg(TYPE_POS));
  }
  Block *GetTable() {
    return Store::DirectWordToBlock(GetArg(TABLE_POS));
  }
  void SetTable(word t) {
    ReplaceArg(TABLE_POS, t);
  }
  word GetEntry(u_int i) {
    Assert(i < GetTableSize());
    return GetTable()->GetArg(i);
  }
  void SetEntry(u_int i,  word entry) {
    Assert(i < GetTableSize());
    GetTable()->ReplaceArg(i, entry);
  }

  void InsertItem(word key, word value);
  void RemoveEntry(u_int i, word prev, HashNode *node);
  void DeleteItem(word key);

  bool IsMember(word key);
  word GetItem(word key); // must be member
  // returns alternative in case of failure
  word CondGetItem(word key, word alternative); 

  static WeakDictionary *New(hashkeytype type, BlockLabel l, u_int size,
			     Finalization *handler);
public:
  using Block::ToWord;

  void InsertItem(s_int key, word value) {
    InsertItem(Store::IntToWord(key), value);
  }
  void DeleteItem(s_int key) {
    DeleteItem(Store::IntToWord(key));
  }
  bool IsMember(s_int key) {
    return IsMember(Store::IntToWord(key));
  }
  word GetItem(s_int key) {
    return GetItem(Store::IntToWord(key));
  }
  word CondGetItem(s_int key, word alternative) {
    return CondGetItem(Store::IntToWord(key), alternative);
  }

  u_int GetSize() {
    return (u_int) Store::WordToInt(GetArg(COUNTER_POS));
  }
  void Clear() {
    Block *arr = GetTable();
    u_int size = arr->GetSize();
    
    SetCounter(0);
    for (u_int i = size; i--;) {
      arr->InitArg(i, 0);
    }
  }
  bool IsEmpty() {
    return GetCounter() == 0;
  }
  void Apply(item_apply func);

  static WeakDictionary *New(u_int size, Finalization *handler) {
    WeakDictionary *d =
      WeakDictionary::New(INT_KEY, WEAK_DICT_LABEL, size, handler);
    Store::RegisterWeakDict(d);
    return d;
  } 
  static WeakDictionary *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert((p == INVALID_POINTER) || (p->GetLabel() == WEAK_DICT_LABEL));
    return (WeakDictionary *) p;
  }
  static WeakDictionary *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert((p == INVALID_POINTER) || (p->GetLabel() == WEAK_DICT_LABEL));
    return (WeakDictionary *) p;
  }
};

class DllExport BlockHashTableFinalizer : public Finalization {
public:
  BlockHashTableFinalizer() {}

  virtual void Finalize(word value);
};

class DllExport BlockHashTable : public WeakDictionary {
protected:
  friend class Store;
  static word tables;
  static Finalization *handler;
  void Rehash();
public:

  void InsertItem(word key, word value) {
    WeakDictionary::InsertItem(key, value);
  }
  void DeleteItem(word key) {
    WeakDictionary::DeleteItem(key);
  }
  bool IsMember(word key) {
    return WeakDictionary::IsMember(key);
  }
  word GetItem(word key) {
    return WeakDictionary::GetItem(key); // must be member
  }
  word CondGetItem(word key, word alternative) {
    return WeakDictionary::CondGetItem(key, alternative);
  }

  static void Init() {
    tables  = Store::IntToWord(0);
    handler = new BlockHashTableFinalizer();
  }

  static BlockHashTable *New(u_int size) {
    BlockHashTable *t = (BlockHashTable *)
      WeakDictionary::New(WORD_KEY, BLOCKHASHTABLE_LABEL, size, handler);
    tables = BTListNode::New(t->ToWord(), tables)->ToWord();
    return t;
  }
  static BlockHashTable *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert((p == INVALID_POINTER) || (p->GetLabel() == BLOCKHASHTABLE_LABEL));
    return (BlockHashTable *) p;
  }
  static BlockHashTable *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert(p->GetLabel() == BLOCKHASHTABLE_LABEL);
    return (BlockHashTable *) p;
  }
};

#endif

