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
#include "generic/Transform.hh"
#include "alice/Data.hh"
#include "alice/Guid.hh"
#include "alice/AliceLanguageLayer.hh"

//
// ConstructorHandler
//

class ConstructorHandler: public Handler {
public:
  virtual void PrepareForGC(Block *p);
  virtual Block *GetAbstractRepresentation(Block *blockWithHandler);
};

void ConstructorHandler::PrepareForGC(Block *) {
  // nothing to do
}

Block *ConstructorHandler::GetAbstractRepresentation(Block *blockWithHandler) {
  Constructor *constructor = static_cast<Constructor *>(blockWithHandler);
  return static_cast<Block *>(constructor->GetTransform());
}

//
// Constructor
//

word globalTable;
static const u_int initialSize = 16; // to be checked

Handler *Constructor::handler;

void Constructor::Init() {
  handler = new ConstructorHandler();
  globalTable = HashTable::New(HashTable::BLOCK_KEY, initialSize)->ToWord();
  RootSet::Add(globalTable);
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
  HashTable *hashTable = HashTable::FromWordDirect(globalTable);
  word key = guid->ToWord();
  if (hashTable->IsMember(key)) {
    return Constructor::FromWordDirect(hashTable->GetItem(key));
  } else {
    Block *b = Store::AllocBlockWithHandler(SIZE, handler);
    b->InitArg(NAME_POS, name);
    b->InitArg(TRANSFORM_POS, MakeConstructorTransform(name, key)->ToWord());
    hashTable->InsertItem(key, b->ToWord());
    return static_cast<Constructor *>(b);
  }
}

Transform *Constructor::GetTransform() {
  word transformWord = GetArg(TRANSFORM_POS);
  if (transformWord == Store::IntToWord(0)) {
    Transform *transform =
      MakeConstructorTransform(GetArg(NAME_POS), Guid::New()->ToWord());
    ReplaceArg(TRANSFORM_POS, transform->ToWord());
    return transform;
  } else {
    return Transform::FromWordDirect(transformWord);
  }
}
