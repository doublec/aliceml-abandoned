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
#include "generic/Unpickler.hh"
#include "alice/AliceLanguageLayer.hh"
#include "alice/PrimitiveTable.hh"
#include "alice/Guid.hh"
#include "alice/LazySelInterpreter.hh"
#include "alice/AbstractCodeInterpreter.hh"
#include "alice/AliceConcreteCode.hh"

word AliceLanguageLayer::functionTransformName;

word AliceFunctionHandler(word x) {
  return AliceConcreteCode::New(TagVal::FromWordDirect(x))->ToWord();
}

void AliceLanguageLayer::Init() {
  PrimitiveTable::Init();
  Guid::Init();
  LazySelInterpreter::Init();
  AbstractCodeInterpreter::Init();
  String *name = String::New("Alice.function");
  functionTransformName = name->ToWord();
  RootSet::Add(functionTransformName);
  Unpickler::RegisterHandler(static_cast<Chunk *>(name), AliceFunctionHandler);
}
