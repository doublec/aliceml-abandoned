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
#pragma implementation "alice/AliceLanguageLayer.hh"
#endif

#include "generic/RootSet.hh"
#include "generic/Transients.hh"
#include "generic/Unpickler.hh"
#include "alice/AliceLanguageLayer.hh"
#include "alice/PrimitiveTable.hh"
#include "alice/Guid.hh"
#include "alice/LazySelInterpreter.hh"
#include "alice/AbstractCodeInterpreter.hh"
#include "alice/AliceConcreteCode.hh"
#if LIGHTNING
#include "alice/NativeCodeJitter.hh"
#include "alice/NativeCodeInterpreter.hh"
#endif

word AliceLanguageLayer::TransformNames::primitiveValue;
word AliceLanguageLayer::TransformNames::primitiveFunction;
word AliceLanguageLayer::TransformNames::function;
word AliceLanguageLayer::TransformNames::constructor;

concrete_constructor AliceLanguageLayer::concreteCodeConstructor;

static word AlicePrimitiveValueHandler(word x) {
  TagVal *tagVal = TagVal::FromWordDirect(x);
  Assert(tagVal->GetTag() == 0);
  tagVal->AssertWidth(1);
  return PrimitiveTable::LookupValue(Chunk::FromWordDirect(tagVal->Sel(0)));
}

static word AlicePrimitiveFunctionHandler(word x) {
  TagVal *tagVal = TagVal::FromWordDirect(x);
  Assert(tagVal->GetTag() == 0);
  tagVal->AssertWidth(1);
  return PrimitiveTable::LookupFunction(Chunk::FromWordDirect(tagVal->Sel(0)));
}

static word AliceFunctionHandler(word x) {
  TagVal *abstractCode = TagVal::FromWordDirect(x);
  return AliceLanguageLayer::concreteCodeConstructor(abstractCode);
}

static word AliceConstructorHandler(word x) {
  Tuple *tuple = Tuple::FromWordDirect(x);
  tuple->AssertWidth(2);
  String *name = String::FromWordDirect(tuple->Sel(0));
  Block *guid = Store::DirectWordToBlock(tuple->Sel(1));
  Constructor *constructor = Constructor::New(name, guid);
  return constructor->ToWord();
}

void AliceLanguageLayer::Init() {
  String *alicePrimitiveValue = String::New("Alice.primitive.value");
  TransformNames::primitiveValue = alicePrimitiveValue->ToWord();
  RootSet::Add(TransformNames::primitiveValue);
  Unpickler::RegisterHandler(alicePrimitiveValue, AlicePrimitiveValueHandler);

  String *alicePrimitiveFunction = String::New("Alice.primitive.function");
  TransformNames::primitiveFunction = alicePrimitiveFunction->ToWord();
  RootSet::Add(TransformNames::primitiveFunction);
  Unpickler::RegisterHandler(alicePrimitiveFunction,
			     AlicePrimitiveFunctionHandler);

  String *aliceFunction = String::New("Alice.function");
  TransformNames::function = aliceFunction->ToWord();
  RootSet::Add(TransformNames::function);
  Unpickler::RegisterHandler(aliceFunction, AliceFunctionHandler);

  String *aliceConstructor = String::New("Alice.constructor");
  TransformNames::constructor = aliceConstructor->ToWord();
  RootSet::Add(TransformNames::constructor);
  Unpickler::RegisterHandler(aliceConstructor, AliceConstructorHandler);

  Constructor::Init();
  Hole::InitExceptions(); //--** should not be here
  Guid::Init();
  LazySelInterpreter::Init();
  AbstractCodeInterpreter::Init();
  PrimitiveTable::Init();
#if LIGHTNING
  NativeCodeInterpreter::Init();
  // to be done: Memory should be enlarged dynamically
  NativeCodeJitter::Init(20 * STORE_MEMCHUNK_SIZE);

  const char *jitMode = std::getenv("ALICE_JIT_MODE");
  if (jitMode != NULL && !strcmp(jitMode, "0"))
    concreteCodeConstructor = AliceConcreteCode::New;
  else
    concreteCodeConstructor = NativeConcreteCode::New;
#else
  concreteCodeConstructor = AliceConcreteCode::New;
#endif
}
