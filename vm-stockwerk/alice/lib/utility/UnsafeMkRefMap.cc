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

#include "emulator/Authoring.hh"
#include "adt/HashTable.hh"

DEFINE0(UnsafeMkRefMap_new) {
  RETURN(HashTable::New(HashTable::INT_KEY, 8)->ToWord());
} END

DEFINE1(UnsafeMkRefMap_clone) {
  DECLARE_HASH_TABLE(table, x0);
  // to be done
  RETURN(table->ToWord());
} END

DEFINE3(UnsafeMkRefMap_insert) {
  DECLARE_HASH_TABLE(table, x0);
  DECLARE_INT(key, x1);
  table->InsertItem(x1, x2);
  RETURN_UNIT;
} END

DEFINE2(UnsafeMkRefMap_delete) {
  DECLARE_HASH_TABLE(table, x0);
  DECLARE_INT(key, x1);
  table->DeleteItem(x1);
  RETURN_UNIT;
} END

DEFINE1(UnsafeMkRefMap_deleteAll) {
  DECLARE_HASH_TABLE(table, x0);
  // to be done
  RETURN_UNIT;
} END

DEFINE2(UnsafeMkRefMap_lookup) {
  DECLARE_HASH_TABLE(table, x0);
  DECLARE_INT(key, x1);
  RETURN(table->GetItem(x1));
} END

DEFINE2(UnsafeMkRefMap_member) {
  DECLARE_HASH_TABLE(table, x0);
  DECLARE_INT(key, x1);
  RETURN_BOOL(table->IsMember(x1));
} END

DEFINE1(UnsafeMkRefMap_isEmpty) {
  DECLARE_HASH_TABLE(table, x0);
  // to be done
  RETURN_BOOL(false);
} END

word UnsafeMkRefMap(void) {
  Tuple *t = Tuple::New(8);
  t->Init(0, Primitive::MakeFunction(UnsafeMkRefMap_clone, 1));
  t->Init(1, Primitive::MakeFunction(UnsafeMkRefMap_delete, 2));
  t->Init(2, Primitive::MakeFunction(UnsafeMkRefMap_deleteAll, 1));
  t->Init(3, Primitive::MakeFunction(UnsafeMkRefMap_insert, 3));
  t->Init(4, Primitive::MakeFunction(UnsafeMkRefMap_isEmpty, 1));
  t->Init(5, Primitive::MakeFunction(UnsafeMkRefMap_lookup, 2));
  t->Init(6, Primitive::MakeFunction(UnsafeMkRefMap_member, 2));
  t->Init(7, Primitive::MakeFunction(UnsafeMkRefMap_new, 0));
  return t->ToWord();
}
