//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "generic/UniqueString.hh"
#endif

#include "adt/HashTable.hh"
#include "generic/RootSet.hh"
#include "generic/UniqueString.hh"

static const u_int initialTableSize = 16; // to be checked

static word uniqueStringTable;

void UniqueString::Init() {
  uniqueStringTable =
    HashTable::New(HashTable::BLOCK_KEY, initialTableSize)->ToWord();
  RootSet::Add(uniqueStringTable);
}

UniqueString *UniqueString::New(String *string) {
  HashTable *hashTable = HashTable::FromWordDirect(uniqueStringTable);
  word key = string->ToWord();
  if (hashTable->IsMember(key)) {
    return UniqueString::FromWordDirect(hashTable->GetItem(key));
  } else {
    Block *b = Store::AllocBlock(UNIQUESTRING_LABEL, SIZE);
    b->InitArg(STRING_POS, key);
    b->InitArg(HASH_VALUE_POS, Store::IntToWord(string->Hash()));
    hashTable->InsertItem(key, b->ToWord());
    return static_cast<UniqueString *>(b);
  }
}
