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

#include "store/Store.hh"

class HashTable : private WeakDictionary {
public:
  enum hashkeytype {
    INT_KEY,
    BLOCK_KEY
  };

  using Block::ToWord;
  using WeakDictionary::GetSize;
  using WeakDictionary::Clear;

  void InsertItem(word key, word value) {
    WeakDictionary::InsertItem(key, value);
  }
  void DeleteItem(word key) {
    WeakDictionary::DeleteItem(key);
  }
  int IsMember(word key) {
    return WeakDictionary::IsMember(key);
  }
  word GetItem(word key) {
    return WeakDictionary::GetItem(key); // must be member
  }

  static HashTable *New(hashkeytype type, u_int size) {
    return (HashTable *) WeakDictionary::New((WeakDictionary::hashkeytype) type,
					     HASHTABLE_LABEL, size);
  }
  static HashTable *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == HASHTABLE_LABEL));
    return (HashTable *) p;
  }
  static HashTable *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);

    Assert(p->GetLabel() == HASHTABLE_LABEL);
    return (HashTable *) p;
  }
};

#endif __ADT__HASHTABLE_HH__
