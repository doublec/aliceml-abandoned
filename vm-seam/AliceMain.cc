//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstdio>
#include "alice/AliceLanguageLayer.hh"
#include "alice/BootLinker.hh"

extern word UnsafeConfig();
extern word UnsafeIODesc();
extern word UnsafeOS();
extern word UnsafeUnix();
extern word UnsafeCommandLine();
extern word UnsafeComponent();
extern word UnsafeDebug();
#if DEBUGGER
extern word UnsafeDebugger();
#endif
extern word UnsafeForeign();
extern word UnsafeSignal();
extern word UnsafeSocket();
extern word UnsafeRand();
extern word UnsafeValue();
extern word UnsafeReflect();
extern word UnsafeCell();
extern word UnsafeAddr();
extern word UnsafeRemote();

static NativeComponent nativeComponents[] = {
  {"lib/system/UnsafeConfig",       UnsafeConfig},
  {"lib/system/UnsafeIODesc",       UnsafeIODesc},
  {"lib/system/UnsafeOS",           UnsafeOS},
  {"lib/system/UnsafeUnix",         UnsafeUnix},
  {"lib/system/UnsafeCommandLine",  UnsafeCommandLine},
  {"lib/system/UnsafeComponent",    UnsafeComponent},
  {"lib/system/UnsafeDebug",        UnsafeDebug},
#if DEBUGGER
  {"lib/system/UnsafeDebugger",     UnsafeDebugger},
#endif
  {"lib/system/UnsafeForeign",      UnsafeForeign},
  {"lib/system/UnsafeSignal",       UnsafeSignal},
  {"lib/system/UnsafeSocket",       UnsafeSocket},
  {"lib/system/UnsafeRand",         UnsafeRand},
  {"lib/system/UnsafeValue",        UnsafeValue},
  {"lib/system/UnsafeReflect",      UnsafeReflect},
  {"lib/utility/UnsafeCell",        UnsafeCell},
  {"lib/utility/UnsafeAddr",        UnsafeAddr},
  {"lib/distribution/UnsafeRemote", UnsafeRemote},
  {NULL, NULL}
};

static void InitAlice(int argc, const char *argv[]) {
  static bool initialized = false;
  if (!initialized) {
    char *home = std::getenv("ALICE_HOME");
    if (home == NULL) {
      Error("could not determine installation directory");
    }
    AliceLanguageLayer::Init(home, argc, argv);
    BootLinker::Init(nativeComponents);
    initialized = true;
  }
}

void Start(int argc, const char *argv[]) {
  if (argc < 2) {
    std::fprintf(stderr, "usage: %s <component> <args...>\n", argv[0]);
    std::exit(2);
  }
  InitAlice(argc, argv);
  Thread *thread = Scheduler::NewThread(0, Store::IntToWord(0));
  String *url = String::FromWordDirect(AliceLanguageLayer::rootUrl);
  BootLinker::Link(thread, url);
}

Worker::Result Load(String *name) {
  //--** this can be called multiple times, concurrently
  const char *argv[] = {""};
  InitAlice(1, argv);
  if (name != NULL)
    BootLinker::Link(name);
  return Worker::CONTINUE;
}
