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

#if defined(INTERFACE)
#pragma implementation "emulator/PrimitiveTable.hh"
#endif

#include <cstdio>
#include "adt/HashTable.hh"
#include "emulator/RootSet.hh"
#include "emulator/PrimitiveTable.hh"
#include "emulator/Closure.hh"
#include "emulator/Alice.hh"

word PrimitiveTable::valueTable;
word PrimitiveTable::functionTable;

word PrimitiveTable::Future_Future;
word PrimitiveTable::Future_await;
word PrimitiveTable::General_Chr;
word PrimitiveTable::General_Div;
word PrimitiveTable::General_Overflow;
word PrimitiveTable::General_Size;
word PrimitiveTable::General_Subscript;
word PrimitiveTable::Hole_Cyclic;
word PrimitiveTable::Hole_Hole;

void PrimitiveTable::Init() {
  valueTable    = HashTable::New(HashTable::BLOCK_KEY, 19)->ToWord();
  functionTable = HashTable::New(HashTable::BLOCK_KEY, 19)->ToWord();
  RootSet::Add(functionTable);
  RootSet::Add(valueTable);
  RegisterUnqualified();
  RegisterArray();
  RegisterChar();
  RegisterFuture();
  RegisterGeneral();
  RegisterGlobalStamp();
  RegisterHole();
  RegisterInt();
  RegisterLargeWord();
  RegisterList();
  RegisterMath();
  RegisterOption();
  RegisterReal();
  RegisterRef();
  RegisterString();
  RegisterThread();
  RegisterUnsafe();
  RegisterVector();
  RegisterWord(); 
}

void PrimitiveTable::Register(const char *name, word value) {
  HashTable::FromWordDirect(valueTable)->
    InsertItem(String::New(name)->ToWord(), value);
}

void PrimitiveTable::Register(const char *name,
			      Primitive::function value, u_int arity,
			      u_int frameSize) {
  word function = Primitive::MakeFunction(name, value, arity);
  word closure  = Closure::New(function, 0)->ToWord();
  Register(name, closure);
  HashTable::FromWordDirect(functionTable)->
    InsertItem(String::New(name)->ToWord(), function);
}

void PrimitiveTable::Register(const char *name,
			      Primitive::function value, u_int arity) {
  Register(name, value, arity, 0);
}

void PrimitiveTable::RegisterUniqueConstructor(const char *name) {
  Register(name, UniqueConstructor::New(String::New(name))->ToWord());
}



word PrimitiveTable::Lookup(word table, Chunk *name) {
  Assert(name != INVALID_POINTER);
  word key = name->ToWord();
  HashTable *t = HashTable::FromWordDirect(table);
  if (!t->IsMember(key)) {
    char message[80 + name->GetSize()];
    sprintf(message, "PrimitiveTable::Lookup: unknown primitive `%.*s'",
	    static_cast<int>(name->GetSize()), name->GetBase());
    Error(message);
  }
  return t->GetItem(key);
}

word PrimitiveTable::LookupValue(Chunk *name) {
  return Lookup(valueTable, name);
}

word PrimitiveTable::LookupFunction(Chunk *name) {
  return Lookup(functionTable, name);
}
