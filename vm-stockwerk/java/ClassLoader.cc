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
#pragma implementation "java/ClassLoader.hh"
#endif

#include "adt/HashTable.hh"
#include "java/ClassLoader.hh"

ClassLoader *ClassLoader::New() {
  Block *b = Store::AllocBlock(JavaLabel::ClassLoader, SIZE);
  b->InitArg(CLASS_TABLE_POS,
	     HashTable::New(HashTable::BLOCK_KEY, initialTableSize)->ToWord());
  return static_cast<ClassLoader *>(b);
}
