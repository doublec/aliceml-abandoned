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

#if defined(__MINGW32__) || defined(_MSC_VER)
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <winsock.h>
#endif

#include "generic/SignalHandler.hh"
#include "generic/RootSet.hh"
#include "generic/UniqueString.hh"
#include "generic/Transients.hh"
#include "generic/TaskStack.hh"
#include "generic/IOHandler.hh"
#include "generic/IODesc.hh"
#include "generic/Scheduler.hh"
#include "generic/Primitive.hh"
#include "generic/Unpickler.hh"
#include "generic/Pickler.hh"
#include "generic/Properties.hh"
#include "generic/PushCallWorker.hh"
#include "generic/BindFutureWorker.hh"
#if PROFILE
#include "generic/Profiler.hh"
#endif
#include "alice/AliceLanguageLayer.hh"
#include "alice/BootLinker.hh"

#if !(defined(__MINGW32__) || defined(_MSC_VER))
#include <cstdio>
#include <cstdlib>
#include <cstring>
#endif

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

static u_int mb(u_int n) {
  return n << 20;
}

DllExport int AliceMain(char *home, u_int argc, char *argv[]) {
#if defined(__MINGW32__) || defined(_MSC_VER)
  WSADATA wsa_data;
  WORD req_version = MAKEWORD(1, 1);
  if (WSAStartup(req_version, &wsa_data) != 0)
    Error("no usable WinSock DLL found");
#endif

  // Set up the store:
  u_int memLimits[STORE_GENERATION_NUM];
  memLimits[0] = mb(16);
  memLimits[1] = mb(15);
  memLimits[2] = mb(35);
  Store::InitStore(memLimits, 67, 20);
  // Set up datastructures:
  RootSet::Init();
  UniqueString::Init();
  Properties::Init(home, argc, argv);
  if (Properties::rootUrl == Store::IntToWord(0)) {
    std::fprintf(stderr, "usage: %s component\n", argv[0]);
    return 2;
  }
  TaskStack::Init();
  IOHandler::Init();
  IODesc::Init();
  SignalHandler::Init();
  Scheduler::Init();
#if PROFILE
  Profiler::Init();
#endif
  // Set up interpreters and services:
  PushCallWorker::Init();
  BindFutureWorker::Init();
  Unpickler::Init();
  Pickler::Init();
  Hole::Init();
  // Set up Alice Language Layer:
  AliceLanguageLayer::Init();
  BootLinker::Init(nativeComponents);
  // Link and execute boot component:
  BootLinker::Link(String::New("lib/system/Boot")); //--** to be done
  return Scheduler::Run();
}
