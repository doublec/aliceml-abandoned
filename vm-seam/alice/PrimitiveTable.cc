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
#pragma implementation "alice/PrimitiveTable.hh"
#endif

#include <cstdio>
#include "adt/HashTable.hh"
#include "generic/RootSet.hh"
#include "generic/Closure.hh"
#include "alice/Data.hh"
#include "alice/PrimitiveTable.hh"

word PrimitiveTable::valueTable;
word PrimitiveTable::functionTable;

word PrimitiveTable::Future_Future;
word PrimitiveTable::General_Chr;
word PrimitiveTable::General_Div;
word PrimitiveTable::General_Overflow;
word PrimitiveTable::General_Size;
word PrimitiveTable::General_Subscript;
word PrimitiveTable::Hole_Cyclic;
word PrimitiveTable::Hole_Hole;
word PrimitiveTable::Thread_Terminated;

word PrimitiveTable::inlineTable;

void PrimitiveTable::Init() {
  // The following values have been derived from the count of
  // 158 functions, 186 values (see the fprintf below)
  // with a fill ratio of ca. 2/3
  valueTable    = HashTable::New(HashTable::BLOCK_KEY, 280)->ToWord();
  functionTable = HashTable::New(HashTable::BLOCK_KEY, 240)->ToWord();
  RootSet::Add(valueTable);
  RootSet::Add(functionTable);
  RootSet::Add(PrimitiveTable::Future_Future);
  RootSet::Add(PrimitiveTable::General_Chr);
  RootSet::Add(PrimitiveTable::General_Div);
  RootSet::Add(PrimitiveTable::General_Overflow);
  RootSet::Add(PrimitiveTable::General_Size);
  RootSet::Add(PrimitiveTable::General_Subscript);
  RootSet::Add(PrimitiveTable::Hole_Cyclic);
  RootSet::Add(PrimitiveTable::Hole_Hole);
  RootSet::Add(PrimitiveTable::Thread_Terminated);
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
  RegisterUniqueString();
  RegisterUnsafe();
  RegisterVector();
  RegisterWord();
  InitInlines();
  //  fprintf(stderr, "%d functions, %d values\n",
  //	  HashTable::FromWordDirect(functionTable)->GetSize(),
  //	  HashTable::FromWordDirect(valueTable)->GetSize());
}

void PrimitiveTable::Register(const char *name, word value) {
  HashTable::FromWordDirect(valueTable)->
    InsertItem(String::New(name)->ToWord(), value);
}

void PrimitiveTable::Register(const char *name,
			      Primitive::function value, u_int arity) {
  word function = Primitive::MakeFunction(name, value, arity);
  word closure  = Closure::New(function, 0)->ToWord();
  Register(name, closure);
  HashTable::FromWordDirect(functionTable)->
    InsertItem(String::New(name)->ToWord(), function);
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

static const char *inlineNames[] = {
  "Future.byneed",
  "Char.ord",
  "Int.+",
  "Int.*",
  "Int.<",
  NULL};

void PrimitiveTable::InitInlines() {
  BlockHashTable *table = BlockHashTable::New(10);
  u_int i = 0;
  do {
    Chunk *name = static_cast<Chunk *>(String::New(inlineNames[i]));
    word value  = LookupValue(name);
    table->InsertItem(value, Store::IntToWord(i++));
  } while (inlineNames[i] != NULL);
  inlineTable = table->ToWord();
  RootSet::Add(inlineTable);
}
