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

#include "generic/Tuple.hh"
#include "generic/RootSet.hh"
#include "generic/BootLinker.hh"
#include "generic/Unpickler.hh"
#include "generic/Pickler.hh"
#include "alice/Authoring.hh"

DEFINE0(UnsafeComponent_getInitialTable) {
  static word result = Store::IntToWord(0);
  if (result == Store::IntToWord(0)) {
    u_int numberOfEntries = BootLinker::GetNumberOfEntries();
    Queue *keyQueue = BootLinker::GetKeyQueue();
    Vector *vector = Vector::New(numberOfEntries);
    while (numberOfEntries--) {
      String *key = String::FromWordDirect(keyQueue->Dequeue());
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

DEFINE2(UnsafeComponent_save) {
  DECLARE_STRING(filename, x0);
  Scheduler::PushFrameNoCheck(prim_self);
  return Pickler::Save(filename, x1);
} END

DEFINE1(UnsafeComponent_load) {
  DECLARE_STRING(filename, x0);
  Scheduler::PushFrameNoCheck(prim_self);
  return Unpickler::Load(filename);
} END

DEFINE1(UnsafeComponent_pack_) {
  Scheduler::PushFrameNoCheck(prim_self);
  return Pickler::Pack(x0);
} END

DEFINE1(UnsafeComponent_unpack_) {
  DECLARE_STRING(string, x0);
  Scheduler::PushFrameNoCheck(prim_self);
  return Unpickler::Unpack(string);
} END

word UnsafeComponent() {
  Record *record = Record::New(10);
  record->Init("'Corrupt", Unpickler::Corrupt);
  record->Init("'Sited", Pickler::Sited);
  record->Init("Corrupt", Unpickler::Corrupt);
  record->Init("Sited", Pickler::Sited);
  record->Init("extension", String::New("stc")->ToWord());
  INIT_STRUCTURE(record, "UnsafeComponent", "getInitialTable",
		 UnsafeComponent_getInitialTable, 0, true);
  INIT_STRUCTURE(record, "UnsafeComponent", "save",
		 UnsafeComponent_save, 2, true);
  INIT_STRUCTURE(record, "UnsafeComponent", "load",
		 UnsafeComponent_load, 1, true);
  INIT_STRUCTURE(record, "UnsafeComponent", "pack_",
		 UnsafeComponent_pack_, 1, true);
  INIT_STRUCTURE(record, "UnsafeComponent", "unpack_",
		 UnsafeComponent_unpack_, 1, true);
  RETURN_STRUCTURE("UnsafeComponent$", record);
}
