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
#include "emulator/PrimitiveTable.hh"
#include "emulator/Unpickler.hh"
#include "emulator/BootLinker.hh"
#include "emulator/Properties.hh"

// Make Interpreter visible
#include "emulator/PushCallInterpreter.hh"
#include "emulator/LazySelectionInterpreter.hh"
#include "emulator/ByneedInterpreter.hh"
#include "emulator/VectorTabulateInterpreter.hh"
#include "emulator/AbstractCodeInterpreter.hh"

extern word UnsafeMkRefMap(void);
extern word UnsafeReflect(void);
extern word UnsafeIO(void);
extern word UnsafeComponent(void);
extern word UnsafeOS(void);
extern word UnsafeConfig(void);
extern word UnsafeDebug(void);

static prim_table builtins[] =
{ { "lib/utility/UnsafeMkRefMap", UnsafeMkRefMap},
  { "lib/system/UnsafeReflect", UnsafeReflect},
  { "lib/system/UnsafeIO", UnsafeIO},
  { "lib/system/UnsafeComponent", UnsafeComponent},
  { "lib/system/UnsafeOS", UnsafeOS},
  { "lib/system/UnsafeConfig", UnsafeConfig},
  { "lib/system/UnsafeDebug", UnsafeDebug},
  {NULL, NULL} };

static inline
Chunk *NewChunk(const char *s) {
  u_int len  = strlen(s);
  Chunk *p   = Store::AllocChunk(len);
  char *base = p->GetBase();
  memcpy(base, s, len);
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
  Future::Init();
  TaskStack::Init();
  Scheduler::Init();
  PrimitiveTable::Init();
  // Setup Interpreters
  PushCallInterpreter::Init();
  LazySelectionInterpreter::Init();
  ByneedInterpreter::Init();
  VectorTabulateInterpreter::Init();
  AbstractCodeInterpreter::Init();
  Unpickler::Init();
  BootLinker::Init(builtins);
  BootLinker::SetTraceMode(1);
  if (argc < 2) {
    fprintf(stderr, "usage: %s component\n", argv[0]);
    exit(0);
  }
  else {
    // Link and Execute Component
    // to be done: Argument transfer to app
    Chunk *rootUrl  = NewChunk(argv[1]);
    Chunk *bootUrl  = NewChunk("lib/system/Boot");
    word urlWord    = rootUrl->ToWord();
    RootSet::Add(urlWord);
    word module = BootLinker::Link(bootUrl); // might yield GC
    RootSet::Remove(urlWord);
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
