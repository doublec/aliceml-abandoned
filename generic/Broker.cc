//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "generic/Broker.hh"
#endif

#if defined(__MINGW32__) || defined(_MSC_VER)
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#include "generic/RootSet.hh"
#include "generic/Backtrace.hh"
#include "generic/Broker.hh"

static const u_int initialLanguageLayerTableSize = 4; // to be done
static const u_int initialNameValueTableSize = 16; // to be done

static word wLanguageLayerTable, wNameValueTable;

word Broker::BrokerError;

void Broker::Init() {
  wLanguageLayerTable = ChunkMap::New(initialLanguageLayerTableSize)->ToWord();
  RootSet::Add(wLanguageLayerTable);
  wNameValueTable = ChunkMap::New(initialNameValueTableSize)->ToWord();
  RootSet::Add(wNameValueTable);
  BrokerError = UniqueString::New(String::New("Broker.Error"))->ToWord();
  RootSet::Add(BrokerError);
}

#define RAISE(w) {					\
  Scheduler::currentData = w;				\
  word wFrame = Scheduler::GetFrame()->Clone();		\
  Scheduler::PopFrame();				\
  Scheduler::currentBacktrace = Backtrace::New(wFrame);	\
  return Worker::RAISE;					\
}

static void *LoadLanguageLayer(String *languageId) {
  ChunkMap *languageLayerTable = ChunkMap::FromWordDirect(wLanguageLayerTable);
  word wLanguageId = languageId->ToWord();
  if (languageLayerTable->IsMember(wLanguageId)) {
    word w = languageLayerTable->Get(wLanguageId);
    return Store::DirectWordToUnmanagedPointer(w);
  } else {
    void *handle =
#if defined(__MINGW32__) || defined(_MSC_VER)
      (void *) LoadLibrary(languageId->ExportC());
#else
      dlopen(languageId->ExportC(), RTLD_NOW | RTLD_GLOBAL);
#endif
    if (handle != NULL)
      languageLayerTable->Put(wLanguageId,
			      Store::UnmanagedPointerToWord(handle));
    return handle;
  }
}

void Broker::Start(String *languageId, int argc, char *argv[]) {
  void *handle = LoadLanguageLayer(languageId);
  if (handle == NULL) {
    Error("could not link language layer library");
  }
  void (*Start)(int, char *[]) =
#if defined(__MINGW32__) || defined(_MSC_VER)
    reinterpret_cast<void (*)(int, char *[])>
    (GetProcAddress((HMODULE) handle, "Start"));
#else
    (void (*)(int, char *[])) dlsym(handle, "Start");
#endif
  if (Start == NULL) {
    Error("could not start language layer");
  }
  Start(argc, argv);
}

Worker::Result Broker::Load(String *languageId, String *key) {
  void *handle = LoadLanguageLayer(languageId);
  if (handle == NULL) RAISE(BrokerError);
  Worker::Result (*Load)(String *) =
#if defined(__MINGW32__) || defined(_MSC_VER)
    reinterpret_cast<Worker::Result (*)(String *)>
    (GetProcAddress((HMODULE) handle, "Load"));
#else
    (Worker::Result (*)(String *)) dlsym(handle, "Load");
#endif
  if (Load == NULL) RAISE(BrokerError);
  return Load(key);
}

void Broker::Register(String *name, word value) {
  ChunkMap *nameValueTable = ChunkMap::FromWordDirect(wNameValueTable);
  nameValueTable->Put(name->ToWord(), value);
}

word Broker::Lookup(String *name) {
  ChunkMap *nameValueTable = ChunkMap::FromWordDirect(wNameValueTable);
  if (nameValueTable->IsMember(name->ToWord()))
    return nameValueTable->Get(name->ToWord());
  return (word) 0; //--**
}
