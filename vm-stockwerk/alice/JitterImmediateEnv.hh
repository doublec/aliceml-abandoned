//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE_JITTER_IMMEDIATE_ENV_HH__
#define __ALICE_JITTER_IMMEDIATE_ENV_HH__

#include "store/Store.hh"
#include "adt/HashTable.hh"
#include "generic/Tuple.hh"

class ImmediateEnv {
protected:
  static const u_int INITITIAL_TABLE_SIZE = 512;

  static u_int index;
  static HashTable *valueTable;
public:
  // ImmediateEnv Static Constructor
  static void Init() {
    index      = 0;
    valueTable = HashTable::New(HashTable::INT_KEY, INITITIAL_TABLE_SIZE);
  }
  // ImmediateEnv Methods
  static u_int Register(word item) {
    u_int intKey = index++;
    word key = Store::IntToWord(intKey);
    valueTable->InsertItem(key, item);
    return intKey;
  }
  static word ExportEnv() {
    Tuple *tuple = Tuple::New(index);
    for (u_int i = index; i--;) {
      word value = valueTable->GetItem(Store::IntToWord(i));
      tuple->Init(i, value);
    }
    return tuple->ToWord();
  }
};

#endif
