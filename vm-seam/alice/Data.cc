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
#pragma implementation "alice/Data.hh"
#endif

#include "adt/HashTable.hh"
#include "generic/RootSet.hh"
#include "generic/Tuple.hh"
#include "generic/Transform.hh"
#include "generic/ConcreteRepresentationHandler.hh"
#include "alice/Data.hh"
#include "alice/Guid.hh"
#include "alice/AliceLanguageLayer.hh"

static const u_int initialTableSize = 16; // to be checked

//
// ConstructorHandler
//

class ConstructorHandler: public ConcreteRepresentationHandler {
public:
  virtual Block *GetAbstractRepresentation(Block *blockWithHandler);
};

Block *ConstructorHandler::GetAbstractRepresentation(Block *blockWithHandler) {
  Constructor *constructor = static_cast<Constructor *>(blockWithHandler);
  return static_cast<Block *>(constructor->GetTransform());
}

//
// Constructor
//

static word constructorTable;

ConcreteRepresentationHandler *Constructor::handler;

void Constructor::Init() {
  handler = new ConstructorHandler();
  constructorTable =
    HashTable::New(HashTable::BLOCK_KEY, initialTableSize)->ToWord();
  RootSet::Add(constructorTable);
}

static Transform *MakeConstructorTransform(word name, word key) {
  Tuple *tuple = Tuple::New(2);
  tuple->Init(0, name);
  tuple->Init(1, key);
  Chunk *transformName = static_cast<Chunk *>
    (String::FromWordDirect(AliceLanguageLayer::TransformNames::constructor));
  return Transform::New(transformName, tuple->ToWord());
}

Constructor *Constructor::New(word name, Block *guid) {
  Assert(guid != INVALID_POINTER);
  HashTable *hashTable = HashTable::FromWordDirect(constructorTable);
  word key = guid->ToWord();
  if (hashTable->IsMember(key)) {
    return Constructor::FromWordDirect(hashTable->GetItem(key));
  } else {
    ConcreteRepresentation *b = ConcreteRepresentation::New(handler, SIZE);
    b->Init(NAME_POS, name);
    b->Init(TRANSFORM_POS, MakeConstructorTransform(name, key)->ToWord());
    hashTable->InsertItem(key, b->ToWord());
    return static_cast<Constructor *>(b);
  }
}

Transform *Constructor::GetTransform() {
  word transformWord = Get(TRANSFORM_POS);
  if (transformWord == Store::IntToWord(0)) {
    Transform *transform =
      MakeConstructorTransform(Get(NAME_POS), Guid::New()->ToWord());
    Replace(TRANSFORM_POS, transform->ToWord());
    return transform;
  } else {
    return Transform::FromWordDirect(transformWord);
  }
}

//
// UniqueStringHandler
//

class UniqueStringHandler: public ConcreteRepresentationHandler {
public:
  virtual Block *GetAbstractRepresentation(Block *blockWithHandler);
};

Block *
UniqueStringHandler::GetAbstractRepresentation(Block *blockWithHandler) {
  UniqueString *uniqueString = static_cast<UniqueString *>(blockWithHandler);
  return static_cast<Block *>(uniqueString->GetTransform());
}

//
// UniqueString
//

static word uniqueStringTable;

ConcreteRepresentationHandler *UniqueString::handler;

void UniqueString::Init() {
  handler = new UniqueStringHandler();
  uniqueStringTable =
    HashTable::New(HashTable::BLOCK_KEY, initialTableSize)->ToWord();
  RootSet::Add(uniqueStringTable);
}

static Transform *MakeUniqueStringTransform(word string) {
  Chunk *transformName = static_cast<Chunk *>
    (String::FromWordDirect(AliceLanguageLayer::TransformNames::uniqueString));
  return Transform::New(transformName, string);
}

UniqueString *UniqueString::New(String *string) {
  HashTable *hashTable = HashTable::FromWordDirect(uniqueStringTable);
  word key = string->ToWord();
  if (hashTable->IsMember(key)) {
    return UniqueString::FromWordDirect(hashTable->GetItem(key));
  } else {
    ConcreteRepresentation *b = ConcreteRepresentation::New(handler, SIZE);
    b->Init(STRING_POS, key);
    b->Init(HASH_VALUE_POS, Store::IntToWord(string->Hash()));
    b->Init(TRANSFORM_POS, MakeUniqueStringTransform(key)->ToWord());
    hashTable->InsertItem(key, b->ToWord());
    return static_cast<UniqueString *>(b);
  }
}

Transform *UniqueString::GetTransform() {
  word transformWord = Get(TRANSFORM_POS);
  if (transformWord == Store::IntToWord(0)) {
    Transform *transform = MakeUniqueStringTransform(Get(STRING_POS));
    Replace(TRANSFORM_POS, transform->ToWord());
    return transform;
  } else {
    return Transform::FromWordDirect(transformWord);
  }
}
