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
//extern word UnsafeUnix(); //--** missing
extern word UnsafeCommandLine();
extern word UnsafeComponent();
extern word UnsafeDebug();
extern word UnsafeForeign();
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
//{"lib/system/UnsafeUnix",         UnsafeUnix}, //--** missing
  {"lib/system/UnsafeCommandLine",  UnsafeCommandLine},
  {"lib/system/UnsafeComponent",    UnsafeComponent},
  {"lib/system/UnsafeDebug",        UnsafeDebug},
  {"lib/system/UnsafeForeign",      UnsafeForeign},
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
    initialized = true;
  }
}

void Start(int argc, const char *argv[]) {
  if (argc < 2) {
    std::fprintf(stderr, "usage: %s <component> <args...>\n", argv[0]);
    std::exit(2);
  }
  InitAlice(argc, argv);
  BootLinker::Init(nativeComponents);
  BootLinker::Link(String::New("lib/system/Boot")); //--** to be done
}

Worker::Result Load(String *name) {
  const char *argv[] = {""};
  InitAlice(1, argv);
  Error("alice.dll: Load not implemented");   //--** to be done
}
