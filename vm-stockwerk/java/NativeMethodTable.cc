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
#pragma implementation "java/NativeMethodTable.hh"
#endif

#include "java/NativeMethodTable.hh"

static word MakeKey(JavaString *className, JavaString *name,
		    JavaString *descriptor) {
  return className->Concat("#")->Concat(name)->Concat(descriptor)->ToWord();
}

word NativeMethodTable::wTable;

void NativeMethodTable::Init() {
  wTable = HashTable::New(HashTable::BLOCK_KEY, initialSize)->ToWord();
  RootSet::Add(wTable);
  java_lang_Object();
}

void NativeMethodTable::Register(JavaString *className, JavaString *name,
				 JavaString *descriptor, Closure *closure) {
  HashTable *table = HashTable::FromWordDirect(wTable);
  word key = MakeKey(className, name, descriptor);
  Assert(!table->IsMember(key));
  table->InsertItem(key, closure->ToWord());
}

Closure *NativeMethodTable::Lookup(JavaString *className, JavaString *name,
				   JavaString *descriptor) {
  HashTable *table = HashTable::FromWordDirect(wTable);
  word key = MakeKey(className, name, descriptor);
  if (!table->IsMember(key)) return INVALID_POINTER;
  return Closure::FromWordDirect(table->GetItem(key));
}
