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

class DllExport HashTable : public BlockHashTable {
public:
  enum hashkeytype {
    INT_KEY,
    BLOCK_KEY
  };

  static HashTable *New(hashkeytype type, u_int size) {
    return (HashTable *) WeakDictionary::New((WeakDictionary::hashkeytype) type,
					     HASHTABLE_LABEL, size, handler);
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

#endif
