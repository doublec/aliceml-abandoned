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

#include "generic/RootSet.hh"
#include "generic/BootLinker.hh"
#include "generic/Unpickler.hh"
#include "generic/Pickler.hh"
#include "alice/primitives/Authoring.hh"

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
  taskStack->PushFrame(prim_self);
  return Unpickler::Load(static_cast<Chunk *>(s), taskStack);
} END

DEFINE2(UnsafeComponent_save) {
  DECLARE_STRING(s, x0);
  return Pickler::Save(static_cast<Chunk *>(s), x1, taskStack);
} END

word UnsafeComponent(void) {
  Tuple *t = Tuple::New(8);
  t->Init(0, Unpickler::Corrupt);
  t->Init(1, Pickler::Sited);
  t->Init(2, Unpickler::Corrupt);
  t->Init(3, Pickler::Sited);
  t->Init(4, String::New("stc")->ToWord());
  t->Init(5, Primitive::MakeClosure("UnsafeComponent.getInitialTable",
				    UnsafeComponent_getInitialTable, 0, true));
  t->Init(6, Primitive::MakeClosure("UnsafeComponent.load",
				    UnsafeComponent_load, 1, true));
  t->Init(7, Primitive::MakeClosure("UnsafeComponent.save",
				    UnsafeComponent_save, 2, true));
  RETURN_STRUCTURE(t);
}
