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
#include "generic/PushCallWorker.hh"
#include "generic/BindFutureWorker.hh"
#if PROFILE
#include "generic/Profiler.hh"
#endif
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

static int JavaMain(u_int argc, char *argv[]) {
  if (argc < 2) {
    std::fprintf(stderr, "usage: %s <classfile> <args...>\n", argv[0]);
    return 2;
  }

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
  JavaLanguageLayer::Init();
  // Link and execute boot component:
  Startup(argc, argv);
  return Scheduler::Run();
}

int main(int argc, char *argv[]) {
  std::exit(JavaMain(argc, argv));
}
