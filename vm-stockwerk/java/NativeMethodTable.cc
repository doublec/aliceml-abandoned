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

word NativeMethodTable::wTable;

void NativeMethodTable::Register(JavaString *className, JavaString *name,
				 JavaString *descriptor, Closure *closure) {
  HashTable *table = HashTable::FromWordDirect(wTable);
  word key = MakeKey(className, name, descriptor);
  Assert(!table->IsMember(key));
  table->InsertItem(key, closure->ToWord());
}
