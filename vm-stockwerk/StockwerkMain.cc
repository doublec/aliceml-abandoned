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
#include "generic/UniqueString.hh"
#include "generic/Transients.hh"
#include "generic/TaskStack.hh"
#include "generic/IOHandler.hh"
#include "generic/SignalHandler.hh"
#include "generic/Scheduler.hh"
#include "generic/Primitive.hh"
#include "generic/Unpickler.hh"
#include "generic/Pickler.hh"
#include "generic/BootLinker.hh"
#include "generic/Properties.hh"
#include "generic/PushCallInterpreter.hh"
#include "generic/ByneedInterpreter.hh"

#include "alice/Data.hh" //--**
#include "alice/AliceLanguageLayer.hh"

#if PROFILE
#include "generic/Profiler.hh"
#endif

extern word UnsafeConfig(void);
extern word UnsafeIO(void);
extern word UnsafeOS(void);
//extern word UnsafeUnix(void); //--** missing
extern word UnsafeCommandLine(void);
extern word UnsafeComponent(void);
extern word UnsafeDebug(void);
extern word UnsafeSocket(void);
extern word UnsafeRand(void);
extern word UnsafeValue(void);
extern word UnsafeReflect(void);
extern word UnsafeCell(void);
extern word UnsafeAddr(void);
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
  {"lib/system/UnsafeValue",        UnsafeValue},
  {"lib/system/UnsafeReflect",      UnsafeReflect},
  {"lib/utility/UnsafeCell",        UnsafeCell},
  {"lib/utility/UnsafeAddr",        UnsafeAddr},
  {"lib/distribution/UnsafeRemote", UnsafeRemote},
  {NULL, NULL}
};

static u_int mb(u_int n) {
  return n << 20;
}

int main(int argc, char *argv[]) {
  // Setup the store
  u_int memLimits[STORE_GENERATION_NUM];
  memLimits[0] = mb(16);
  memLimits[1] = mb(15);
  memLimits[2] = mb(35);
  Store::InitStore(memLimits, 67, 20);
  // Setup Datastructures
  RootSet::Init();
  UniqueString::Init();
  Properties::Init();
  TaskStack::Init();
  IOHandler::Init();
  SignalHandler::Init();
  Scheduler::Init();
#if PROFILE
  Profiler::Init();
#endif
  // Setup Interpreters and Services
  PushCallInterpreter::Init();
  ByneedInterpreter::Init();
  Unpickler::Init();
  Pickler::Init();
  // Setup Alice Layer
  AliceLanguageLayer::Init();
  // Setup Alice Exceptions used in lower Layers
  //--** should not be here
  Unpickler::InitExceptions();
  Pickler::InitExceptions();
  BootLinker::Init(nativeComponents);
  // Parse command line
  if (argc < 2) {
    fprintf(stderr, "usage: %s component\n", argv[0]);
    exit(2);
  }
  else {
    String *rootUrl     = String::New(argv[1]);
    word urlWord        = rootUrl->ToWord();
    argv++; argc--;
    Properties::rootUrl = urlWord;
    String *bootUrl     = String::New("lib/system/Boot"); // to be done
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
    BootLinker::Link(bootUrl);
    Scheduler::Run();
    exit(0);
  }
}
