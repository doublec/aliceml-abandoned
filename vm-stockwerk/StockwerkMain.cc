//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstdio>
#include "generic/RootSet.hh"
#include "generic/Transients.hh"
#include "generic/TaskStack.hh"
#include "generic/IOHandler.hh"
#include "generic/Scheduler.hh"
#include "generic/Primitive.hh"
#include "generic/Unpickler.hh"
#include "generic/Pickler.hh"
#include "generic/BootLinker.hh"
#include "generic/Properties.hh"
#include "generic/PushCallInterpreter.hh"
#include "generic/ByneedInterpreter.hh"

#include "alice/Data.hh"
#include "alice/PrimitiveTable.hh"
#include "alice/Guid.hh"
#include "alice/LazySelInterpreter.hh"
#include "alice/AbstractCodeInterpreter.hh"

extern word UnsafeConfig(void);
extern word UnsafeIO(void);
extern word UnsafeOS(void);
//extern word UnsafeUnix(void); //--** missing
extern word UnsafeCommandLine(void);
extern word UnsafeComponent(void);
extern word UnsafeDebug(void);
extern word UnsafeSocket(void);
extern word UnsafeRand(void);
extern word UnsafeReflect(void);
extern word UnsafeMkRefMap(void);
//extern word UnsafeAddr(void); //--** missing
extern word UnsafeRemote(void);

static NativeComponent nativeComponents[] = {
  {"lib/system/UnsafeConfig",       UnsafeConfig},
  {"lib/system/UnsafeIO",           UnsafeIO},
  {"lib/system/UnsafeOS",           UnsafeOS},
//{"lib/system/UnsafeUnix",         UnsafeUnix}, //--** missing
  {"lib/system/UnsafeCommandLine",  UnsafeCommandLine},
  {"lib/system/UnsafeComponent",    UnsafeComponent},
  {"lib/system/UnsafeDebug",        UnsafeDebug},
  {"lib/system/UnsafeSocket",       UnsafeSocket},
  {"lib/system/UnsafeRand",         UnsafeRand},
  {"lib/system/UnsafeReflect",      UnsafeReflect},
  {"lib/utility/UnsafeMkRefMap",    UnsafeMkRefMap},
//{"lib/utility/UnsafeAddr",        UnsafeAddr}, //--** missing
  {"lib/distribution/UnsafeRemote", UnsafeRemote},
  {NULL, NULL}
};

static inline Chunk *NewChunk(const char *s) {
  u_int len  = strlen(s);
  Chunk *p   = Store::AllocChunk(len);
  char *base = p->GetBase();
  std::memcpy(base, s, len);
  return p;
}

int main(int argc, char *argv[]) {
  // Setup the store
  u_int memLimits[STORE_GENERATION_NUM];
  for (u_int i = 0; i < STORE_GENERATION_NUM; i++) {
    memLimits[i] = (i + 1) * STORE_MEMCHUNK_SIZE;
  }
  Store::InitStore(memLimits, 75, 20);
  // Setup Datastructures
  RootSet::Init();
  Properties::Init();
  Hole::Init();
  TaskStack::Init();
  IOHandler::Init();
  Scheduler::Init();
  // Setup Interpreters and Services
  Primitive::Init();
  PushCallInterpreter::Init();
  ByneedInterpreter::Init();
  Unpickler::Init();
  Pickler::Init();
  BootLinker::Init(nativeComponents);
  // Setup Alice Layer
  PrimitiveTable::Init();
  Guid::Init();
  LazySelInterpreter::Init();
  AbstractCodeInterpreter::Init();
  // Parse command line
  if (argc < 2) {
    fprintf(stderr, "usage: %s component\n", argv[0]);
    exit(0);
  }
  else {
    Chunk *rootUrl      = NewChunk(argv[1]);
    word urlWord        = rootUrl->ToWord();
    argv++; argc--;
    Properties::rootUrl = urlWord;
    Chunk *bootUrl      = NewChunk("lib/system/Boot"); // to be done
    // Initialize Properties::commandLineArguments:
    word tail = Store::IntToWord(1); // nil
    argv++; argc--;
    for (u_int i = argc; i--; ) {
      TagVal *cons = TagVal::New(0, 2); // ::
      cons->Init(0, String::New(argv[i])->ToWord());
      cons->Init(1, tail);
      tail = cons->ToWord();
    }
    Properties::commandLineArguments = tail;
    // Link and Execute Component
    word module = BootLinker::Link(bootUrl); // might yield GC
    if (module != Store::IntToWord(0)) {
      Tuple *tuple    = Tuple::FromWord(module);
      tuple->AssertWidth(1);
      Scheduler::NewThread(tuple->Sel(0), // Module Closure
			   Scheduler::ONE_ARG, Properties::rootUrl,
			   TaskStack::New());
      // Restart Scheduler to execute module
      Scheduler::Run();
      exit(0);
    }
    exit(1);
  }
}
