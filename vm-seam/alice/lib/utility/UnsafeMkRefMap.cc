//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
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
  if (table->IsMember(x1)) {
    TagVal *some = TagVal::New(1, 1); // SOME ...
    some->Init(0, table->GetItem(x1));
    RETURN(some->ToWord());
  } else {
    RETURN_INT(0); // NONE
  }
} END

DEFINE2(UnsafeMkRefMap_member) {
  DECLARE_HASH_TABLE(table, x0);
  DECLARE_INT(key, x1);
  RETURN_BOOL(table->IsMember(x1));
} END

DEFINE1(UnsafeMkRefMap_isEmpty) {
  DECLARE_HASH_TABLE(table, x0);
  RETURN_BOOL(table->IsEmpty());
} END

word UnsafeMkRefMap(void) {
  Tuple *t = Tuple::New(8);
  t->Init(0, Primitive::MakeClosure("UnsafeMkRefMap.clone",
				    UnsafeMkRefMap_clone, 1, true));
  t->Init(1, Primitive::MakeClosure("UnsafeMkRefMap.delete",
				    UnsafeMkRefMap_delete, 2, true));
  t->Init(2, Primitive::MakeClosure("UnsafeMkRefMap.deleteAll",
				    UnsafeMkRefMap_deleteAll, 1, true));
  t->Init(3, Primitive::MakeClosure("UnsafeMkRefMap.insert",
				    UnsafeMkRefMap_insert, 3, true));
  t->Init(4, Primitive::MakeClosure("UnsafeMkRefMap.isEmpty",
				    UnsafeMkRefMap_isEmpty, 1, true));
  t->Init(5, Primitive::MakeClosure("UnsafeMkRefMap.lookup",
				    UnsafeMkRefMap_lookup, 2, true));
  t->Init(6, Primitive::MakeClosure("UnsafeMkRefMap.member",
				    UnsafeMkRefMap_member, 2, true));
  t->Init(7, Primitive::MakeClosure("UnsafeMkRefMap.new",
				    UnsafeMkRefMap_new, 0, true));
  RETURN_STRUCTURE(t);
}
