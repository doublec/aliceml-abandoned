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
#include "alice/Data.hh"
#include "alice/PrimitiveTable.hh"
#include "alice/AliceLanguageLayer.hh"

word PrimitiveTable::valueTable;
word PrimitiveTable::functionTable;

word PrimitiveTable::Future_Cyclic;
word PrimitiveTable::General_Chr;
word PrimitiveTable::General_Div;
word PrimitiveTable::General_Domain;
word PrimitiveTable::General_Overflow;
word PrimitiveTable::General_Size;
word PrimitiveTable::General_Subscript;
word PrimitiveTable::Hole_Hole;
word PrimitiveTable::Thread_Terminated;

void PrimitiveTable::Init() {
  // The following values have been derived from the count of
  // 190 functions, 217 values (see the fprintf below)
  // with a fill ratio of ca. 2/3
  valueTable    = ChunkMap::New(325)->ToWord();
  functionTable = ChunkMap::New(285)->ToWord();
  RootSet::Add(valueTable);
  RootSet::Add(functionTable);
  RootSet::Add(PrimitiveTable::Future_Cyclic);
  RootSet::Add(PrimitiveTable::General_Chr);
  RootSet::Add(PrimitiveTable::General_Div);
  RootSet::Add(PrimitiveTable::General_Domain);
  RootSet::Add(PrimitiveTable::General_Overflow);
  RootSet::Add(PrimitiveTable::General_Size);
  RootSet::Add(PrimitiveTable::General_Subscript);
  RootSet::Add(PrimitiveTable::Hole_Hole);
  RootSet::Add(PrimitiveTable::Thread_Terminated);
  RegisterUnqualified();
  RegisterArray();
  RegisterByte();
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
  RegisterRef();
  RegisterRemote();
  RegisterString();
  RegisterThread();
  RegisterUniqueString();
  RegisterUnsafe();
  RegisterVector();
  RegisterWord8();
  RegisterWord8Array();
  RegisterWord8Vector();
  RegisterWord31();
  //  std::fprintf(stderr, "%d functions, %d values\n",
  //	       ChunkMap::FromWordDirect(functionTable)->GetSize(),
  //	       ChunkMap::FromWordDirect(valueTable)->GetSize());
}

void PrimitiveTable::Register(const char *name, word value) {
  ChunkMap::FromWordDirect(valueTable)->Put(String::New(name)->ToWord(), value);
}

void PrimitiveTable::Register(const char *name,
			      Interpreter::function value, u_int arity) {
  word transformName = AliceLanguageLayer::TransformNames::primitiveFunction;
  Transform *abstract =
    Transform::New(Store::DirectWordToChunk(transformName),
		   String::New(name)->ToWord());
  word function = Primitive::MakeFunction(name, value, arity, abstract);
  word closure  = Closure::New(function, 0)->ToWord();
  Register(name, closure);
  ChunkMap::FromWordDirect(functionTable)->
    Put(String::New(name)->ToWord(), function);
}

void PrimitiveTable::RegisterUniqueConstructor(const char *name,
					       const char *id) {
  Register(id, UniqueConstructor::New(name, id)->ToWord());
}

word PrimitiveTable::Lookup(word table, Chunk *name) {
  Assert(name != INVALID_POINTER);
  word key = name->ToWord();
  ChunkMap *t = ChunkMap::FromWordDirect(table);
  if (!t->IsMember(key)) {
    char message[80 + name->GetSize()];
    sprintf(message, "PrimitiveTable::Lookup: unknown primitive `%.*s'",
	    static_cast<int>(name->GetSize()), name->GetBase());
    Error(message);
  }
  return t->Get(key);
}

word PrimitiveTable::LookupValue(Chunk *name) {
  return Lookup(valueTable, name);
}

word PrimitiveTable::LookupFunction(Chunk *name) {
  return Lookup(functionTable, name);
}
