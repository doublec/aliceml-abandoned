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
#include "emulator/RootSet.hh"
#include "emulator/BootLinker.hh"
#include "emulator/Unpickler.hh"

static word SitedConstructor;

DEFINE0(UnsafeComponent_getInitialTable) {
  static word result = Store::IntToWord(0);
  if (result == Store::IntToWord(0)) {
    u_int numberOfEntries = BootLinker::GetNumberOfEntries();
    Queue *keyQueue = BootLinker::GetKeyQueue();
    Vector *vector = Vector::New(numberOfEntries);
    while (numberOfEntries--) {
      Chunk *key = Store::DirectWordToChunk(keyQueue->Dequeue());
      Component *component = BootLinker::LookupComponent(key);
      Assert(component != INVALID_POINTER);
      Tuple *triple = Tuple::New(3);
      triple->Init(0, key->ToWord());
      triple->Init(1, component->GetSign());
      triple->Init(2, component->GetStr());
      vector->Init(numberOfEntries, triple->ToWord());
    }
    Assert(keyQueue->IsEmpty());
    result = vector->ToWord();
  }
  RETURN(result);
} END

DEFINE1(UnsafeComponent_load) {
  DECLARE_STRING(s, x0);
  Chunk *filename = BootLinker::MakeFileName((Chunk *) s);
  return Unpickler::Load(filename->GetBase(), taskStack);
} END

DEFINE2(UnsafeComponent_save) {
  DECLARE_STRING(s, x0);
  Error("UnsafeComponent.load not implemented"); //--** to be done
} END

word UnsafeComponent(void) {
  SitedConstructor =
    UniqueConstructor::New(String::New("Component.Sited"))->ToWord();
  RootSet::Add(SitedConstructor);

  Tuple *t = Tuple::New(8);
  t->Init(0, Unpickler::Corrupt);
  t->Init(1, SitedConstructor);
  t->Init(2, Unpickler::Corrupt);
  t->Init(3, SitedConstructor);
  t->Init(4, String::New("stc")->ToWord());
  t->Init(5, Primitive::MakeClosure(UnsafeComponent_getInitialTable, 0));
  t->Init(6, Primitive::MakeClosure(UnsafeComponent_load, 1));
  t->Init(7, Primitive::MakeClosure(UnsafeComponent_save, 2));
  RETURN_STRUCTURE(t);
}
