//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Contributor:
//   Andreas Rossberg <rossberg@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstdio>
#include <cstring>

#include "alice/Authoring.hh"
#include "alice/BootLinker.hh"

static word SitedConstructor;
static word CorruptConstructor;
static word NotFoundConstructor;
static word MismatchConstructor;
static word EvalConstructor;
static word FailureConstructor;
static word NativeConstructor;

//
// Error Handling
//
static word MakeNativeError() {
  String *s = DllLoader::GetLastError();
  ConVal *conVal =
    ConVal::New(Store::DirectWordToBlock(NativeConstructor), 1);
  conVal->Init(0, s->ToWord());
  return conVal->ToWord();
}

//
// Primitives
//
DEFINE3(UnsafeComponent_Mismatch) {
  ConVal *conVal =
    ConVal::New(Store::DirectWordToBlock(MismatchConstructor), 3);
  conVal->Init(0, x0);
  conVal->Init(1, x1);
  conVal->Init(2, x2);
  RETURN(conVal->ToWord());
} END

DEFINE1(UnsafeComponent_Eval) {
  ConVal *conVal =
    ConVal::New(Store::DirectWordToBlock(EvalConstructor), 1);
  conVal->Init(0, x0);
  RETURN(conVal->ToWord());
} END

DEFINE2(UnsafeComponent_Failure) {
  ConVal *conVal =
    ConVal::New(Store::DirectWordToBlock(FailureConstructor), 2);
  conVal->Init(0, x0);
  conVal->Init(1, x1);
  RETURN(conVal->ToWord());
} END

DEFINE1(UnsafeComponent_Native) {
  ConVal *conVal =
    ConVal::New(Store::DirectWordToBlock(NativeConstructor), 1);
  conVal->Init(0, x0);
  RETURN(conVal->ToWord());
} END

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
    RootSet::Add(result);
  }
  RETURN(result);
} END

DEFINE2(UnsafeComponent_save) {
  DECLARE_STRING(filename, x0);
  PUSH_PRIM_SELF();
  return Pickler::Save(filename, x1);
} END

DEFINE1(UnsafeComponent_load) {
  DECLARE_STRING(filename, x0);
  PUSH_PRIM_SELF();
  return Unpickler::Load(filename);
} END

DEFINE1(UnsafeComponent_unzip) {
    DECLARE_STRING(str, x0);
    String *res = Unpickler::Unzip (str);
    if (res == NULL) {
        RAISE(Unpickler::Corrupt);
    } else {
        RETURN(res->ToWord ());
    }
} END


DEFINE1(UnsafeComponent_linkNative) {
  DECLARE_STRING(filename, x0);
  DllLoader::libhandle handle = DllLoader::OpenLibrary(filename);
  if (handle == NULL) RAISE(MakeNativeError());

  word (*InitComponent)() = (word (*)())
    DllLoader::GetSymbol(handle, String::New("InitComponent"));

  if (InitComponent == NULL) {
    DllLoader::CloseLibrary(handle);
    RAISE(Unpickler::Corrupt);
  }
  TagVal *component = TagVal::New(Types::EVALUATED, 2);        // EVALUATED {
  component->Init(Types::inf1, Store::IntToWord(Types::NONE)); //   inf = NONE,
  component->Init(Types::mod, InitComponent());                //   mod = ... }
  RETURN(component->ToWord());
} END

DEFINE1(UnsafeComponent_pack_) {
  PUSH_PRIM_SELF();
  return Pickler::Pack(x0);
} END

DEFINE1(UnsafeComponent_unpack_) {
  DECLARE_STRING(string, x0);
  PUSH_PRIM_SELF();
  return Unpickler::Unpack(string);
} END

AliceDll word UnsafeComponent() {
  SitedConstructor =
    UniqueConstructor::New("Sited", "Component.Sited")->ToWord();
  RootSet::Add(SitedConstructor);
  CorruptConstructor =
    UniqueConstructor::New("Corrupt", "Component.Corrupt")->ToWord();
  RootSet::Add(CorruptConstructor);
  NotFoundConstructor =
    UniqueConstructor::New("NotFound", "Component.NotFound")->ToWord();
  RootSet::Add(NotFoundConstructor);
  MismatchConstructor =
    UniqueConstructor::New("Mismatch", "Component.Mismatch")->ToWord();
  RootSet::Add(MismatchConstructor);
  EvalConstructor =
    UniqueConstructor::New("Eval", "Component.Eval")->ToWord();
  RootSet::Add(EvalConstructor);
  FailureConstructor =
    UniqueConstructor::New("Failure", "Component.Failure")->ToWord();
  RootSet::Add(FailureConstructor);
  NativeConstructor =
    UniqueConstructor::New("Native", "Component.Native")->ToWord();
  RootSet::Add(NativeConstructor);

  Record *record = Record::New(26);
  record->Init("'SitedInternal", Pickler::Sited);
  record->Init("SitedInternal", Pickler::Sited);
  record->Init("'CorruptInternal", Unpickler::Corrupt);
  record->Init("CorruptInternal", Unpickler::Corrupt);
  record->Init("'Sited", SitedConstructor);
  record->Init("Sited", SitedConstructor);
  record->Init("'Corrupt", CorruptConstructor);
  record->Init("Corrupt", CorruptConstructor);
  record->Init("'NotFound", NotFoundConstructor);
  record->Init("NotFound", NotFoundConstructor);
  record->Init("'Mismatch", MismatchConstructor);
  INIT_STRUCTURE(record, "UnsafeComponent", "Mismatch",
		 UnsafeComponent_Mismatch, 3);
  record->Init("'Eval", EvalConstructor);
  INIT_STRUCTURE(record, "UnsafeComponent", "Eval",
		 UnsafeComponent_Eval, 1);
  record->Init("'Failure", FailureConstructor);
  INIT_STRUCTURE(record, "UnsafeComponent", "Failure",
		 UnsafeComponent_Failure, 2);
  record->Init("'Native", NativeConstructor);
  INIT_STRUCTURE(record, "UnsafeComponent", "Native",
		 UnsafeComponent_Native, 1);
  record->Init("extension", String::New("alc")->ToWord());
  INIT_STRUCTURE(record, "UnsafeComponent", "getInitialTable",
		 UnsafeComponent_getInitialTable, 0);
  INIT_STRUCTURE(record, "UnsafeComponent", "save",
		 UnsafeComponent_save, 2);
  INIT_STRUCTURE(record, "UnsafeComponent", "load",
		 UnsafeComponent_load, 1);
  INIT_STRUCTURE(record, "UnsafeComponent", "unzip",
                 UnsafeComponent_unzip, 1);
  INIT_STRUCTURE(record, "UnsafeComponent", "linkNative",
		 UnsafeComponent_linkNative, 1);
  INIT_STRUCTURE(record, "UnsafeComponent", "pack_",
		 UnsafeComponent_pack_, 1);
  INIT_STRUCTURE(record, "UnsafeComponent", "unpack_",
		 UnsafeComponent_unpack_, 1);
  RETURN_STRUCTURE("UnsafeComponent$", record);
}
