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

#if defined(INTERFACE)
#pragma implementation "store/WeakDictionary.hh"
#endif
#include "store/WeakDictionary.hh"

#include <cstdio>
#include <cstring>

//
// Internal Helper Functions
//

// String hashing function is taken from
// 'Aho, Sethi, Ullman: Compilers..., page 436
static inline u_int HashString(const char *s, u_int len, u_int size) {
  const char *sm = (s + len);
  unsigned h = 0, g;
  for (const char *p = s; p < sm; p++) {
    h = (h << 4) + (*p);
    if ((g = h & 0xf0000000)) {
      h = h ^ (g >> 24);
      h = h ^ g;
    }
  }
  return (h % size);
}

static inline bool EqualBlocks(Block *a, Block *b) {
  u_int al = (a->GetSize() * sizeof(word));
  u_int bl = (b->GetSize() * sizeof(word));
  if (al == bl)
    return (!std::memcmp(a->GetBase(), b->GetBase(), al));
  return false;
}

//
// Internal Helper Methods
//
inline u_int WeakDictionary::HashInt(u_int i) {
  u_int size = GetTableSize();
  return (i % size);
}

inline u_int WeakDictionary::HashBlock(Block *b) {
  u_int size = b->GetSize();
  u_int len  = size * sizeof(word);
  return HashString((const char *) b->GetBase(), len, GetTableSize());
}

inline u_int WeakDictionary::HashKey(word key) {
  switch (GetKeyType()) {
  case INT_KEY:
    return HashInt(static_cast<s_int>(Store::DirectWordToInt(key)));
  case BLOCK_KEY:
    return HashBlock(Store::DirectWordToBlock(key));
  case WORD_KEY:
    return HashInt((u_int) key);
  }
  Assert(0);
  return 0; // unsafe
}

inline HashNode *WeakDictionary::FindKey(word key, word nodes, word & prev) {
  switch (GetKeyType()) {
  case INT_KEY:
    {
      s_int v = Store::DirectWordToInt(key);
      while (nodes != Store::IntToWord(0)) {
	HashNode *node = HashNode::FromWordDirect(nodes);
	if (Store::DirectWordToInt(node->GetKey()) == v)
	  return node;
	prev  = nodes;
	nodes = node->GetNext();
      }
    }
    break;
  case BLOCK_KEY:
    {
      Block *v = Store::DirectWordToBlock(key);
      while (nodes != Store::IntToWord(0)) {
	HashNode *node = HashNode::FromWordDirect(nodes);
	if (EqualBlocks(Store::DirectWordToBlock(node->GetKey()), v))
	  return node;
	prev  = nodes;
	nodes = node->GetNext();
      }
    }
    break;
  case WORD_KEY:
    {
      u_int v = (u_int) key;
      while (nodes != Store::IntToWord(0)) {
	HashNode *node = HashNode::FromWordDirect(nodes);
	if ((u_int) node->GetKey() == v)
	  return node;
	prev  = nodes;
	nodes = node->GetNext();
      }
    }
    break;
  }
  return NULL;
}

void WeakDictionary::Resize() {
  u_int oldsize = GetTableSize();
  u_int newsize = oldsize << 1;
  Block *oldp   = GetTable();
  Block *newp   = Store::AllocBlock((BlockLabel) HASHNODEARRAY_LABEL, newsize);
  // Correct possibly blown up size
  newsize = newp->GetSize();
  u_int percent = (u_int) (1 + (newsize * FILL_RATIO));
  SetPercent(percent);
  SetTable(newp->ToWord());
  // init the new table with zero
  for (u_int i = newsize; i--;) {
    newp->InitArg(i, Store::IntToWord(0));
  }
  // reinsert the new items
  for (u_int i = oldsize; i--;) {
    word nodes = oldp->GetArg(i);
    while (nodes != Store::IntToWord(0)) {
      HashNode *node   = HashNode::FromWordDirect(nodes);
      word key         = node->GetKey();
      u_int hashed_key = HashKey(key);
      word keyNodes    = newp->GetArg(hashed_key);
      word value       = node->GetValue(); 
      word newKeyNodes = HashNode::New(key, value, keyNodes)->ToWord();
      newp->InitArg(hashed_key, newKeyNodes);
      nodes = node->GetNext();
    }
  }
}

//
// Public Interface Methods
//

WeakDictionary *
WeakDictionary::New(hashkeytype type, BlockLabel l,
		    u_int size, Finalization *handler) {
  Block *p      = Store::AllocBlock(l, SIZE);
  Block *arr    = Store::AllocBlock(HASHNODEARRAY_LABEL, size);
  u_int percent = (u_int) (size * FILL_RATIO);

  p->InitArg(HANDLER_POS, Store::UnmanagedPointerToWord(handler));
  p->InitArg(COUNTER_POS, Store::IntToWord(0));
  p->InitArg(PERCENT_POS, Store::IntToWord(percent));
  p->InitArg(TYPE_POS, Store::IntToWord(type));
  p->InitArg(TABLE_POS, arr->ToWord());
  
  for (u_int i = size; i--;)
    arr->InitArg(i, Store::IntToWord(0));

  return (WeakDictionary *) p;
}

void WeakDictionary::InsertItem(word key, word value) {
  u_int counter = GetCounter();
  u_int percent = GetPercent();
  if (counter > percent)
    Resize();

  u_int hashed_key = HashKey(key);
  word nodes       = GetEntry(hashed_key);
  word prev        = Store::IntToWord(0);
  HashNode *entry  = FindKey(key, nodes, prev);
  if (entry != NULL)
    entry->Fill(key, value);
  else {
    SetEntry(hashed_key, HashNode::New(key, value, nodes)->ToWord());
    SetCounter(GetCounter() + 1);
  }
}

void WeakDictionary::RemoveEntry(u_int i, word prev, HashNode *node) {
  if (prev == Store::IntToWord(0))
    SetEntry(i, node->GetNext());
  else
    HashNode::FromWordDirect(prev)->SetNext(node->GetNext());
  SetCounter(GetCounter() - 1);
}

void WeakDictionary::DeleteItem(word key) {
  u_int hashed_key = HashKey(key);
  word nodes       = GetEntry(hashed_key);
  word prev        = Store::IntToWord(0);
  HashNode *entry  = FindKey(key, nodes, prev);
  if (entry != NULL)
    RemoveEntry(hashed_key, prev, entry);
}

bool WeakDictionary::IsMember(word key) {
  u_int hashed_key = HashKey(key);
  word nodes       = GetEntry(hashed_key);
  word prev        = Store::IntToWord(0);
  return (FindKey(key, nodes, prev) != NULL);
}

word WeakDictionary::GetItem(word key) {
  u_int hashed_key = HashKey(key);
  word nodes       = GetEntry(hashed_key);
  word prev        = Store::IntToWord(0);
  HashNode *entry  = FindKey(key, nodes, prev);
  Assert(entry != NULL);
  return entry->GetValue(); // should raise invalid_key something
}

word WeakDictionary::CondGetItem(word key, word alternative) {
  u_int hashed_key = HashKey(key);
  word nodes       = GetEntry(hashed_key);
  word prev        = Store::IntToWord(0);
  HashNode *entry  = FindKey(key, nodes, prev);
  return ((entry == NULL) ? alternative : entry->GetValue());
}

void WeakDictionary::Apply(item_apply func) {
  Block *table = GetTable();
  for (u_int i = table->GetSize(); i--;) {
    word nodes = table->GetArg(i);
    while (nodes != Store::IntToWord(0)) {
      HashNode *node = HashNode::FromWordDirect(nodes);
      func(node->GetKey(), node->GetValue());
      nodes = node->GetNext();
    }
  }
}

//
// BlockHashTable Methods
//
word BlockHashTable::tables;
Finalization *BlockHashTable::handler;

void BlockHashTable::Rehash() {
  Block *table = GetTable();
  u_int size   = table->GetSize();
  for (u_int i = size; i--;) {
    word nodes = table->GetArg(i);
    word prev  = Store::IntToWord(0);
    while (nodes != Store::IntToWord(0)) {
      HashNode *node   = HashNode::FromWordDirect(nodes);
      word key         = node->GetKey();
      u_int hashed_key = ((u_int) key % size);
      if (hashed_key != i) {
	nodes = node->GetNext(); // Order is important
	// Remove from old chain
	if (prev == Store::IntToWord(0))
	  table->ReplaceArg(i, nodes);
	else
	  HashNode::FromWordDirect(prev)->SetNext(nodes);
	// Insert into new chain
	node->SetNext(table->GetArg(hashed_key));
	table->ReplaceArg(hashed_key, node->ToWord());
      }
      else {
	prev = nodes;
	nodes = node->GetNext();
      }
    }
  }
}

void BlockHashTableFinalizer::Finalize(word) {
  return; // always drop values
}
