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

#include "alice/AbstractCode.hh"
#include "alice/AliceConcreteCode.hh"
#include "alice/AliceLanguageLayer.hh"

word AliceConcreteCode::New(TagVal *abstractCode) {
  abstractCode->AssertWidth(AbstractCode::functionWidth);
  ConcreteCode *concreteCode =
    ConcreteCode::New(AbstractCodeInterpreter::self, SIZE);
  Chunk *name =
    Store::DirectWordToChunk(AliceLanguageLayer::TransformNames::function);
  Transform *transform = Transform::New(name, abstractCode->ToWord());
  concreteCode->Init(ABSTRACT_CODE_POS, abstractCode->ToWord());
  concreteCode->Init(TRANSFORM_POS, transform->ToWord());
  return concreteCode->ToWord();
}

void AliceConcreteCode::Disassemble(std::FILE *file) {
  Transform *transform = Transform::FromWordDirect(Get(TRANSFORM_POS));
  TagVal *abstractCode = TagVal::FromWordDirect(transform->GetArgument());
  Tuple *coord = Tuple::FromWordDirect(abstractCode->Sel(0));
  std::fprintf(file, "Disassembling function at %s:%d.%d\n\n",
	       String::FromWordDirect(coord->Sel(0))->ExportC(),
	       Store::DirectWordToInt(coord->Sel(1)),
	       Store::DirectWordToInt(coord->Sel(2)));
  TagVal *pc = TagVal::FromWordDirect(abstractCode->Sel(4));
  AbstractCode::Disassemble(file, pc);
}
