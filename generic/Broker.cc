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

#include "generic/DllLoader.hh"

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
  DllLoader::Init();
}

#define RAISE(w) {						\
  Scheduler::SetCurrentData(w);					\
  word wFrame = Scheduler::GetFrame()->Clone();			\
  Scheduler::PopFrame();					\
  Scheduler::SetCurrentBacktrace(Backtrace::New(wFrame));	\
  return Worker::RAISE;						\
}

static DllLoader::libhandle LoadLanguageLayer(String *languageId) {
  ChunkMap *languageLayerTable = ChunkMap::FromWordDirect(wLanguageLayerTable);
  word wLanguageId = languageId->ToWord();
  if (languageLayerTable->IsMember(wLanguageId)) {
    word w = languageLayerTable->Get(wLanguageId);
    return (DllLoader::libhandle) Store::DirectWordToUnmanagedPointer(w);
  } else {
    u_int n = languageId->GetSize();
    String *filename = String::New(n + 4);
    std::memcpy(filename->GetValue(), languageId->GetValue(), n);
    std::memcpy(filename->GetValue() + n, ".dll", 4);
    DllLoader::libhandle handle =
      DllLoader::OpenLibrary(filename);
    if (handle != NULL)
      languageLayerTable->Put(wLanguageId,
			      Store::UnmanagedPointerToWord(handle));
    else {
      std::fprintf(stderr, "OpenLibrary(%s) failed: %s\n",
 		   filename->ExportC(), DllLoader::GetLastError()->ExportC());
    }
    return handle;
  }
}

void Broker::Start(String *languageId, int argc, char *argv[]) {
  DllLoader::libhandle handle = LoadLanguageLayer(languageId);
  if (handle == NULL) {
    //--** improve error handling
    Error("could not link language layer library");
  }
  void (*Start)(int, char *[]) =
    (void (*)(int, char *[])) DllLoader::GetSymbol(handle, String::New("Start"));
  if (Start == NULL) {
    Error("could not start language layer");
  }
  Start(argc, argv);
}

Worker::Result Broker::Load(String *languageId, String *key) {
  DllLoader::libhandle handle = LoadLanguageLayer(languageId);
  if (handle == NULL) RAISE(BrokerError);
  Worker::Result (*Load)(String *) =
    (Worker::Result (*)(String *)) DllLoader::GetSymbol(handle, 
							String::New("Load"));
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
