//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "alice/primitives/PrimitiveTable.hh"
#endif

#include <cstdio>
#include "adt/HashTable.hh"
#include "generic/RootSet.hh"
#include "generic/TaskStack.hh"
#include "generic/NativeTaskManager.hh"
#include "alice/Data.hh"
#include "alice/primitives/PrimitiveTable.hh"

word PrimitiveTable::table;

word PrimitiveTable::Future_Future;
word PrimitiveTable::Future_await;
word PrimitiveTable::General_Chr;
word PrimitiveTable::General_Div;
word PrimitiveTable::General_Overflow;
word PrimitiveTable::General_Size;
word PrimitiveTable::General_Subscript;
word PrimitiveTable::Hole_Cyclic;
word PrimitiveTable::Hole_Hole;
word PrimitiveTable::Vector_tabulate_cont;

void PrimitiveTable::Init() {
  table = HashTable::New(HashTable::BLOCK_KEY, 19)->ToWord();
  RootSet::Add(table);
  RegisterUnqualified();
  RegisterArray();
  RegisterChar();
  RegisterFuture();
  RegisterGeneral();
  RegisterGlobalStamp();
  RegisterHole();
  RegisterInt();
  RegisterList();
  RegisterMath();
  RegisterOption();
  RegisterReal();
  RegisterString();
  RegisterThread();
  RegisterUnsafe();
  RegisterVector();
  RegisterWord();
  //--** copy to class variables
}

void PrimitiveTable::Register(const char *name, word value) {
  HashTable::FromWordDirect(table)->
    InsertItem(String::New(name)->ToWord(), value);
}

void PrimitiveTable::Register(const char *name, function value, u_int arity,
			      u_int frameSize) {
  ConcreteCode *concreteCode =
    ConcreteCode::New(new NativeTaskManager(value, arity, frameSize), 0);
  Closure *closure = Closure::New(concreteCode, 0);
  Register(name, closure->ToWord());
}

void PrimitiveTable::Register(const char *name, function value, u_int arity) {
  Register(name, value, arity, 0);
}

void PrimitiveTable::RegisterUniqueConstructor(const char *name) {
  Register(name, UniqueConstructor::New(String::New(name))->ToWord());
}

word PrimitiveTable::Lookup(Chunk *name) {
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
