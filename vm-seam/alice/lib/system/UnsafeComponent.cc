//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
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
#if defined(__MINGW32__) || defined(_MSC_VER)
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#include "alice/Authoring.hh"
#include "alice/BootLinker.hh"

static word NotFoundConstructor;
static word MismatchConstructor;
static word EvalConstructor;
static word FailureConstructor;
static word NativeConstructor;

//
// Error Handling
//
static word MakeNativeError() {
#if defined(__MINGW32__) || defined(_MSC_VER)
  DWORD errorCode = GetLastError();
  char *lpMsgBuf;
  int n = FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
			FORMAT_MESSAGE_IGNORE_INSERTS |
			FORMAT_MESSAGE_FROM_SYSTEM |
			FORMAT_MESSAGE_MAX_WIDTH_MASK, NULL, errorCode,
			MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
			(LPTSTR) &lpMsgBuf, 0, NULL);
  String *s;
  if (!n) {
    static char buffer[32];
    std::sprintf(buffer, "Error code %ld", errorCode);
    s = String::New(buffer);
  } else
    s = String::New(lpMsgBuf, n);
  LocalFree(lpMsgBuf);
#else
  char *msg = dlerror();
  String *s = String::New(msg? msg: "no error");
#endif
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

DEFINE1(UnsafeComponent_linkNative) {
  DECLARE_STRING(filename, x0);
#if defined(__MINGW32__) || defined(_MSC_VER)
  //--** this can produce the error message "... is not a valid Windows image."
  HMODULE hModule = LoadLibrary(filename->ExportC());
  if (hModule == NULL) RAISE(MakeNativeError());
  word (*InitComponent)() =
    reinterpret_cast<word (*)()>(GetProcAddress(hModule, "InitComponent"));
  if (InitComponent == NULL) {
    FreeLibrary(hModule);
    RAISE(Unpickler::Corrupt);
  }
#else
  void *handle = dlopen(filename->ExportC(), RTLD_NOW | RTLD_GLOBAL);
  if (handle == NULL) RAISE(MakeNativeError());
  word (*InitComponent)() = (word (*)()) dlsym(handle, "InitComponent");
  if (InitComponent == NULL) {
    dlclose(handle);
    RAISE(Unpickler::Corrupt);
  }
#endif
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
  NotFoundConstructor =
    UniqueConstructor::New("NotFound", "UnsafeComponent.NotFound")->ToWord();
  RootSet::Add(NotFoundConstructor);
  MismatchConstructor =
    UniqueConstructor::New("Mismatch", "UnsafeComponent.Mismatch")->ToWord();
  RootSet::Add(MismatchConstructor);
  EvalConstructor =
    UniqueConstructor::New("Eval", "UnsafeComponent.Eval")->ToWord();
  RootSet::Add(EvalConstructor);
  FailureConstructor =
    UniqueConstructor::New("Failure", "UnsafeComponent.Failure")->ToWord();
  RootSet::Add(FailureConstructor);
  NativeConstructor =
    UniqueConstructor::New("Native", "UnsafeComponent.Native")->ToWord();
  RootSet::Add(NativeConstructor);

  Record *record = Record::New(21);
  record->Init("'Sited", Pickler::Sited);
  record->Init("Sited", Pickler::Sited);
  record->Init("'Corrupt", Unpickler::Corrupt);
  record->Init("Corrupt", Unpickler::Corrupt);
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
  record->Init("extension", String::New("stc")->ToWord());
  INIT_STRUCTURE(record, "UnsafeComponent", "getInitialTable",
		 UnsafeComponent_getInitialTable, 0);
  INIT_STRUCTURE(record, "UnsafeComponent", "save",
		 UnsafeComponent_save, 2);
  INIT_STRUCTURE(record, "UnsafeComponent", "load",
		 UnsafeComponent_load, 1);
  INIT_STRUCTURE(record, "UnsafeComponent", "linkNative",
		 UnsafeComponent_linkNative, 1);
  INIT_STRUCTURE(record, "UnsafeComponent", "pack_",
		 UnsafeComponent_pack_, 1);
  INIT_STRUCTURE(record, "UnsafeComponent", "unpack_",
		 UnsafeComponent_unpack_, 1);
  RETURN_STRUCTURE("UnsafeComponent$", record);
}
