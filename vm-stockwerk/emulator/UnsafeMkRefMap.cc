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
  Error("UnsafeMkRefMap.clone not implemented"); //--** to be done
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
  table->Clear();
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
  Error("UnsafeMkRefMap.isEmpty not implemented"); //--** to be done
  RETURN_BOOL(false);
} END

word UnsafeMkRefMap(void) {
  Tuple *t = Tuple::New(8);
  t->Init(0, Primitive::MakeClosure("UnsafeMkRefMap_clone",
				    UnsafeMkRefMap_clone, 1, true));
  t->Init(1, Primitive::MakeClosure("UnsafeMkRefMap_delete",
				    UnsafeMkRefMap_delete, 2, true));
  t->Init(2, Primitive::MakeClosure("UnsafeMkRefMap_deleteAll",
				    UnsafeMkRefMap_deleteAll, 1, true));
  t->Init(3, Primitive::MakeClosure("UnsafeMkRefMap_insert",
				    UnsafeMkRefMap_insert, 3, true));
  t->Init(4, Primitive::MakeClosure("UnsafeMkRefMap_isEmpty",
				    UnsafeMkRefMap_isEmpty, 1, true));
  t->Init(5, Primitive::MakeClosure("UnsafeMkRefMap_lookup",
				    UnsafeMkRefMap_lookup, 2, true));
  t->Init(6, Primitive::MakeClosure("UnsafeMkRefMap_member",
				    UnsafeMkRefMap_member, 2, true));
  t->Init(7, Primitive::MakeClosure("UnsafeMkRefMap_new",
				    UnsafeMkRefMap_new, 0, true));
  RETURN_STRUCTURE(t);
}
