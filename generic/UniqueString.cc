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

#include "adt/ChunkMap.hh"
#include "generic/RootSet.hh"
#include "generic/UniqueString.hh"

static const u_int initialTableSize = 16; // to be checked

static word uniqueStringTable;

void UniqueString::Init() {
  uniqueStringTable = ChunkMap::New(initialTableSize)->ToWord();
  RootSet::Add(uniqueStringTable);
}

UniqueString *UniqueString::New(String *string) {
  ChunkMap *map = ChunkMap::FromWordDirect(uniqueStringTable);
  word key = string->ToWord();
  if (map->IsMember(key)) {
    return UniqueString::FromWordDirect(map->Get(key));
  } else {
    Block *b = Store::AllocBlock(UNIQUESTRING_LABEL, SIZE);
    b->InitArg(STRING_POS, key);
    b->InitArg(HASH_VALUE_POS, Store::IntToWord(string->Hash()));
    map->Put(key, b->ToWord());
    return static_cast<UniqueString *>(b);
  }
}
