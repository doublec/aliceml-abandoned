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
#include "generic/Time.hh"
#if PROFILE
#include "generic/Profiler.hh"
#endif
#include "generic/Broker.hh"
#if DEBUGGER
#include "generic/DebugWorker.hh"
#include "generic/Debugger.hh"
#include "generic/GenericDebuggerEvent.hh"
#endif

static u_int mb(u_int n) {
  return n << 20;
}

extern "C" SeamDll void InitSeam() {
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
  Time::Init();
#if PROFILE
  Profiler::Init();
#endif
  // Set up interpreters and services:
  PushCallWorker::Init();
  BindFutureWorker::Init();
  Unpickler::Init();
  Pickler::Init();
  Hole::Init();
  Broker::Init();
#if DEBUGGER
  DebugWorker::Init();
  Debugger::Init();
  GenericEventAccessor::Init();
#endif
}
