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
#include "emulator/RootSet.hh"
#include "emulator/Transients.hh"
#include "emulator/TaskStack.hh"
#include "emulator/Scheduler.hh"
#include "emulator/Primitive.hh"
#include "emulator/PrimitiveTable.hh"
#include "emulator/Unpickler.hh"
#include "emulator/Pickler.hh"
#include "emulator/BootLinker.hh"
#include "emulator/Properties.hh"
#include "emulator/Alice.hh"

// Make Interpreter visible
#include "emulator/PushCallInterpreter.hh"
#include "emulator/LazySelectionInterpreter.hh"
#include "emulator/ByneedInterpreter.hh"
#include "emulator/AbstractCodeInterpreter.hh"

extern word UnsafeConfig(void);
extern word UnsafeIO(void);
extern word UnsafeOS(void);
//extern word UnsafeUnix(void); //--** missing
extern word UnsafeCommandLine(void);
extern word UnsafeComponent(void);
extern word UnsafeDebug(void);
//extern word UnsafeSocket(void); //--** missing
extern word UnsafeRand(void); //--** missing
extern word UnsafeReflect(void);
extern word UnsafeMkRefMap(void);
//extern word UnsafeAddr(void); //--** missing

static NativeComponent nativeComponents[] = {
  {"lib/system/UnsafeConfig",      UnsafeConfig},
  {"lib/system/UnsafeIO",          UnsafeIO},
  {"lib/system/UnsafeOS",          UnsafeOS},
//{"lib/system/UnsafeUnix",        UnsafeUnix}, //--** missing
  {"lib/system/UnsafeCommandLine", UnsafeCommandLine},
  {"lib/system/UnsafeComponent",   UnsafeComponent},
  {"lib/system/UnsafeDebug",       UnsafeDebug},
//{"lib/system/UnsafeSocket",      UnsafeSocket}, //--** missing
  {"lib/system/UnsafeRand",        UnsafeRand},
  {"lib/system/UnsafeReflect",     UnsafeReflect},
  {"lib/utility/UnsafeMkRefMap",   UnsafeMkRefMap},
//{"lib/utility/UnsafeAddr",       UnsafeAddr},
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
  Scheduler::Init();
  Primitive::Init();
  PrimitiveTable::Init();
  // Setup Interpreters
  PushCallInterpreter::Init();
  LazySelectionInterpreter::Init();
  ByneedInterpreter::Init();
  AbstractCodeInterpreter::Init();
  Unpickler::Init();
  Pickler::Init();
  BootLinker::Init(nativeComponents);
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
			   Interpreter::OneArg(urlWord),
			   TaskStack::New());
      // Restart Scheduler to execute module
      Scheduler::Run();
      exit(0);
    }
    exit(1);
  }
}
