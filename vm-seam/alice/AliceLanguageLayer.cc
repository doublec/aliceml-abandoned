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

word AliceLanguageLayer::TransformNames::primitiveValue;
word AliceLanguageLayer::TransformNames::primitiveFunction;
word AliceLanguageLayer::TransformNames::function;
word AliceLanguageLayer::TransformNames::constructor;

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
  return AliceConcreteCode::New(TagVal::FromWordDirect(x))->ToWord();
}

static word AliceConstructorHandler(word x) {
  Tuple *tuple = Tuple::FromWordDirect(x);
  tuple->AssertWidth(2);
  Constructor *constructor =
    Constructor::New(tuple->Sel(0), Store::DirectWordToBlock(tuple->Sel(1)));
  return constructor->ToWord();
}

void AliceLanguageLayer::Init() {
  String *alicePrimitiveValue = String::New("Alice.primitive.value");
  TransformNames::primitiveValue = alicePrimitiveValue->ToWord();
  RootSet::Add(TransformNames::primitiveValue);
  Unpickler::RegisterHandler(static_cast<Chunk *>(alicePrimitiveValue),
			     AlicePrimitiveValueHandler);

  String *alicePrimitiveFunction = String::New("Alice.primitive.function");
  TransformNames::primitiveFunction = alicePrimitiveFunction->ToWord();
  RootSet::Add(TransformNames::primitiveFunction);
  Unpickler::RegisterHandler(static_cast<Chunk *>(alicePrimitiveFunction),
			     AlicePrimitiveFunctionHandler);

  String *aliceFunction = String::New("Alice.function");
  TransformNames::function = aliceFunction->ToWord();
  RootSet::Add(TransformNames::function);
  Unpickler::RegisterHandler(static_cast<Chunk *>(aliceFunction),
			     AliceFunctionHandler);

  String *aliceConstructor = String::New("Alice.constructor");
  TransformNames::constructor = aliceConstructor->ToWord();
  RootSet::Add(TransformNames::constructor);
  Unpickler::RegisterHandler(static_cast<Chunk *>(aliceConstructor),
			     AliceConstructorHandler);

  Constructor::Init();
  Guid::Init();
  LazySelInterpreter::Init();
  AbstractCodeInterpreter::Init();

  Hole::InitExceptions(); //--** should not be here
  PrimitiveTable::Init();
}
