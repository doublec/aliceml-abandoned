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
#include "generic/BootLinker.hh"
#include "generic/Properties.hh"
#include "generic/PushCallWorker.hh"
#include "generic/BindFutureWorker.hh"
#if PROFILE
#include "generic/Profiler.hh"
#endif
#include "alice/AliceLanguageLayer.hh"
#include "java/JavaLanguageLayer.hh"
#include "java/Startup.hh"

#if !(defined(__MINGW32__) || defined(_MSC_VER))
#include <cstdio>
#include <cstdlib>
#include <cstring>
#endif

static u_int mb(u_int n) {
  return n << 20;
}

static int JavaMain(char *home, u_int argc, char *argv[]) {
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
  // Set up Java Language Layer:
  AliceLanguageLayer::Init();
  JavaLanguageLayer::Init();
  // Setup Alice exceptions used in lower Layers:
  //--** should not be here
  Unpickler::InitExceptions();
  Pickler::InitExceptions();
  // Link and execute boot component:
  Startup();
  return Scheduler::Run();
}

int main(int argc, char *argv[]) {
  char *home = std::getenv("JAVA_HOME");
  if (home == NULL) {
    Error("could not determine installation directory");
  }
  std::exit(JavaMain(home, argc, argv));
}
