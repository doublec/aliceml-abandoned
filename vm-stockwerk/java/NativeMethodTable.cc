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

#include "generic/Primitive.hh"
#include "java/NativeMethodTable.hh"

static word MakeKey(JavaString *className, JavaString *name,
		    JavaString *descriptor) {
  JavaString *s = className->Concat("#")->Concat(name)->Concat(descriptor);
  return s->ToArray()->ToWord();
}

word NativeMethodTable::wTable;

void NativeMethodTable::Init() {
  wTable = HashTable::New(HashTable::BLOCK_KEY, initialSize)->ToWord();
  RootSet::Add(wTable);
  java_lang_Object(JavaString::New("java/lang/Object"));
}

void NativeMethodTable::Register(JavaString *className, JavaString *name,
				 JavaString *descriptor, Closure *closure,
				 bool /*--** isVirtual */) {
  HashTable *table = HashTable::FromWordDirect(wTable);
  word key = MakeKey(className, name, descriptor);
  Assert(!table->IsMember(key));
  table->InsertItem(key, closure->ToWord());
}

void NativeMethodTable::Register(JavaString *className, const char *name,
				 const char *descriptor, Closure *closure,
				 bool isVirtual) {
  Register(className, JavaString::New(name),
	   JavaString::New(descriptor), closure, isVirtual);
}

void NativeMethodTable::Register(JavaString *className, const char *name,
				 const char *descriptor,
				 Interpreter::function value, u_int arity,
				 bool isVirtual) {
  //--** support abstract representations
  word function =
    Primitive::MakeFunction(name, value, arity, INVALID_POINTER);
  Closure *closure = Closure::New(function, 0);
  Register(className, name, descriptor, closure, isVirtual);
}

Closure *NativeMethodTable::Lookup(JavaString *className, JavaString *name,
				   JavaString *descriptor) {
  HashTable *table = HashTable::FromWordDirect(wTable);
  word key = MakeKey(className, name, descriptor);
  if (!table->IsMember(key)) return INVALID_POINTER;
  return Closure::FromWordDirect(table->GetItem(key));
}
