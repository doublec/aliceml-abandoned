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

#include "emulator/Authoring.hh"
#include "emulator/Unpickler.hh"
#include "emulator/RootSet.hh"
#include "emulator/BootLinker.hh"

static word SitedConstructor;

DEFINE0(UnsafeComponent_getInitialTable) {
  // to be done
  RETURN_UNIT;
} END

DEFINE1(UnsafeComponent_load) {
  // to be done
  RETURN_UNIT;
} END

DEFINE2(UnsafeComponent_save) {
  // to be done
  RETURN_UNIT;
} END

word UnsafeComponent(void) {
  SitedConstructor =
    UniqueConstructor::New(String::New("Component.Sited"))->ToWord();
  RootSet::Add(SitedConstructor);

  HashTable *moduleTable = BootLinker::GetModuleTable();

  Tuple *t = Tuple::New(8);
  t->Init(0, Unpickler::Corrupt);
  t->Init(1, SitedConstructor);
  t->Init(2, Unpickler::Corrupt);
  t->Init(3, SitedConstructor);
  t->Init(4, String::New("stc")->ToWord());
  t->Init(5, Primitive::MakeFunction(UnsafeComponent_getInitialTable, 0));
  t->Init(6, Primitive::MakeFunction(UnsafeComponent_load, 1));
  t->Init(7, Primitive::MakeFunction(UnsafeComponent_save, 2));
  return t->ToWord();
}
