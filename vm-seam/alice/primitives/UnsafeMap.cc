//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2004
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/Authoring.hh"
#include "store/Map.hh"

// TODO: This needs a design
#define DECLARE_UNSAFE_MAP_KEY(value, x)				\
  word value = PointerOp::Deref(x);					\
  if (PointerOp::IsTransient(value)) {					\
    REQUEST(value);							\
  }									\
  Assert(Store::DirectWordToBlock(value) != INVALID_POINTER);

//
// UnsafeMapHandler
//
class UnsafeMapHandler : public ConcreteRepresentationHandler {
public:
  static UnsafeMapHandler *self;
  
  UnsafeMapHandler () : ConcreteRepresentationHandler () {}
  virtual Transform *GetAbstractRepresentation(ConcreteRepresentation *);
  
  static void Init() {
    self = new UnsafeMapHandler();
  }
};

UnsafeMapHandler *UnsafeMapHandler::self;

class UnsafeMap : public ConcreteRepresentation {
protected:
  enum { MAP_POS, SIZE };

public:
  static UnsafeMap *New(u_int size) {
    ConcreteRepresentation *unsafeMap =
      ConcreteRepresentation::New(UnsafeMapHandler::self, SIZE);
    unsafeMap->Init(MAP_POS, Map::New(size)->ToWord());
    return static_cast<UnsafeMap *>(unsafeMap);
  }

  // Map Accessors
  Map *GetMap()                    { return Map::FromWordDirect(Get(MAP_POS)); }
  void Put(word key, word value)   { GetMap()->Put(key, value); }
  void Remove(word key)            { GetMap()->Remove(key); }
  bool IsMember(word key)          { return GetMap()->IsMember(key); }
  word GetNoCheck(word key)        { return GetMap()->Get(key); }
  word CondGet(word key, word alt) { return GetMap()->CondGet(key, alt); }
  u_int GetMapSize()               { return GetMap()->GetSize(); }
  void Clear()                     { GetMap()->Clear(); }
  void Apply(item_apply func)      { GetMap()->Apply(func); }

  static UnsafeMap *FromWord(word x) {
    ConcreteRepresentation *unsafeMap = ConcreteRepresentation::FromWord(x);
    Assert(unsafeMap->GetHandler() == UnsafeMapHandler::self);
    return static_cast<UnsafeMap *>(unsafeMap);
  }
  static UnsafeMap *FromWordDirect(word x) {
    ConcreteRepresentation *unsafeMap =
      ConcreteRepresentation::FromWordDirect(x);
    Assert(unsafeMap->GetHandler() == UnsafeMapHandler::self);
    return static_cast<UnsafeMap *>(unsafeMap);
  }
};

#define DECLARE_UNSAFE_MAP(unsafeMap, x) \
  DECLARE_BLOCKTYPE(UnsafeMap, unsafeMap, x);

DEFINE1(UnsafeMap_new) {
  DECLARE_INT(size, x0);
  RETURN(UnsafeMap::New(size)->ToWord());
} END

DEFINE3(UnsafeMap_put) {
  DECLARE_UNSAFE_MAP(map, x0);
  DECLARE_UNSAFE_MAP_KEY(key, x1);
  word value = x2;
  map->Put(key, value);
  RETURN_UNIT;
} END

DEFINE2(UnsafeMap_remove) {
  DECLARE_UNSAFE_MAP(map, x0);
  DECLARE_UNSAFE_MAP_KEY(key, x1);
  map->Remove(key);
  RETURN_UNIT;
} END

DEFINE2(UnsafeMap_isMember) {
  DECLARE_UNSAFE_MAP(map, x0);
  DECLARE_UNSAFE_MAP_KEY(key, x1);
  RETURN_BOOL(map->IsMember(key));
} END

DEFINE2(UnsafeMap_get) {
  DECLARE_UNSAFE_MAP(map, x0);
  DECLARE_UNSAFE_MAP_KEY(key, x1);
  if (map->IsMember(key)) {
    RETURN(map->GetNoCheck(key));
  } else {
    RAISE(PrimitiveTable::UnsafeMap_IllegalKey);
  }
} END

DEFINE1(UnsafeMap_getSize) {
  DECLARE_UNSAFE_MAP(map, x0);
  RETURN_INT(map->GetMapSize());
} END

DEFINE3(UnsafeMap_condGet) {
  DECLARE_UNSAFE_MAP(map, x0);
  DECLARE_UNSAFE_MAP_KEY(key, x1);
  word alternative = x2;
  RETURN(map->CondGet(key, alternative));
} END

DEFINE1(UnsafeMap_clear) {
  DECLARE_UNSAFE_MAP(map, x0);
  map->Clear();
  RETURN_UNIT;
} END

static word itemList;

static void CreateItemList(word key, word item) {
  Tuple *keyValuePair = Tuple::New(2);
  TagVal *cons        = TagVal::New(Types::cons, 2);
  keyValuePair->Init(0, key);
  keyValuePair->Init(1, item);
  cons->Init(0, keyValuePair->ToWord());
  cons->Init(1, itemList);
  itemList = cons->ToWord();
}

DEFINE1(UnsafeMap_toList) {
  DECLARE_UNSAFE_MAP(map, x0);
  itemList = Store::IntToWord(Types::nil);
  map->Apply(CreateItemList);
  RETURN(itemList);
} END

static Vector *itemVector;
static u_int itemIndex;

static void PopulateItemVector(word key, word item) {
  Tuple *keyValuePair = Tuple::New(2);
  keyValuePair->Init(0, key);
  keyValuePair->Init(1, item);
  itemVector->Init(itemIndex++, keyValuePair->ToWord());
}

DEFINE1(UnsafeMap_toVector) {
  DECLARE_UNSAFE_MAP(map, x0);
  itemVector = Vector::New(map->GetMapSize());
  itemIndex  = 0;
  map->Apply(PopulateItemVector);
  RETURN(itemVector->ToWord());
} END

Transform *
UnsafeMapHandler::GetAbstractRepresentation(ConcreteRepresentation *concRep) {
  UnsafeMap *unsafeMap = static_cast<UnsafeMap *>(concRep);
  itemVector = Vector::New(unsafeMap->GetMapSize());
  itemIndex  = 0;
  unsafeMap->Apply(PopulateItemVector);
  Chunk *name =
    Chunk::FromWordDirect(AliceLanguageLayer::TransformNames::unsafeMap);
  return Transform::New(name, itemVector->ToWord());
}

word AliceLanguageLayer::TransformNames::unsafeMap;

static word UnsafeMapInstantationHandler(word wItems) {
  Vector *items        = Vector::FromWordDirect(wItems);
  u_int nbItems        = items->GetLength();
  UnsafeMap *unsafeMap = UnsafeMap::New(nbItems * 3 / 2);
  Map *map             = unsafeMap->GetMap();
  for (u_int i = nbItems; i--;) {
    Tuple *keyValuePair = Tuple::FromWordDirect(items->Sub(i));
    map->Put(keyValuePair->Sel(0), keyValuePair->Sel(1));
  }
  return unsafeMap->ToWord();
}

void PrimitiveTable::RegisterUnsafeMap() {
  UnsafeMapHandler::Init();
  String *unsafeMapName = String::New("Alice.UnsafeMap");
  AliceLanguageLayer::TransformNames::unsafeMap = unsafeMapName->ToWord();
  RootSet::Add(AliceLanguageLayer::TransformNames::unsafeMap);
  Unpickler::RegisterHandler(unsafeMapName, UnsafeMapInstantationHandler);

  PrimitiveTable::UnsafeMap_IllegalKey =
    UniqueConstructor::New("IllegalKey", "UnsafeMap.IllegalKey")->ToWord();
  Register("UnsafeMap.IllegalKey", PrimitiveTable::UnsafeMap_IllegalKey);
  Register("UnsafeMap.condGet", UnsafeMap_condGet, 3);
  Register("UnsafeMap.clear", UnsafeMap_clear, 1);
  Register("UnsafeMap.get", UnsafeMap_get, 2);
  Register("UnsafeMap.getSize", UnsafeMap_getSize, 1);
  Register("UnsafeMap.isMember", UnsafeMap_isMember, 2);
  Register("UnsafeMap.new", UnsafeMap_new, 1);
  Register("UnsafeMap.put", UnsafeMap_put, 3);
  Register("UnsafeMap.remove", UnsafeMap_remove, 2);
  Register("UnsafeMap.toList", UnsafeMap_toList, 1);
  Register("UnsafeMap.toVector", UnsafeMap_toVector, 1);
}
