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
#pragma implementation "alice/AliceConcreteCode.hh"
#endif

#include "generic/RootSet.hh"
#include "generic/Transform.hh"
#include "alice/AliceConcreteCode.hh"

static word aliceFunctionTransformName;

void AliceConcreteCode::Init() {
  aliceFunctionTransformName = String::New("Alice.function")->ToWord();
  RootSet::Add(aliceFunctionTransformName);
}

AliceConcreteCode *AliceConcreteCode::New(TagVal *abstractCode) {
  ConcreteCode *concreteCode =
    ConcreteCode::New(AbstractCodeInterpreter::self, SIZE);
  Chunk *name = Store::DirectWordToChunk(aliceFunctionTransformName);
  Transform *transform = Transform::New(name, abstractCode->ToWord());
  concreteCode->Init(ABSTRACT_CODE_POS, abstractCode->ToWord());
  concreteCode->Init(TRANSFORM_POS, transform->ToWord());
  return static_cast<AliceConcreteCode *>(concreteCode);
}
