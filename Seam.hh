//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __SEAM_HH__
#define __SEAM_HH__

#define SEAM_FOREIGN

#include "Base.hh"
#include "generic/SignalHandler.hh"
#include "store/Store.hh"
#include "store/Map.hh"
#include "store/WeakMap.hh"
#include "store/JITStore.hh"
#include "adt/IntMap.hh"
#include "adt/ChunkMap.hh"
#include "adt/Queue.hh"
#include "adt/Stack.hh"
#include "generic/FinalizationSet.hh"
#include "generic/Transform.hh"
#include "generic/ConcreteRepresentationHandler.hh"
#include "generic/ConcreteRepresentation.hh"
#include "generic/ConcreteCode.hh"
#include "generic/Closure.hh"
#include "generic/Thread.hh"
#include "generic/ThreadQueue.hh"
#include "generic/Tuple.hh"
#include "generic/String.hh"
#include "generic/StackFrame.hh"
#include "generic/Backtrace.hh"
#include "generic/Pickle.hh"
#include "generic/Float.hh"
#include "generic/Double.hh"
#include "generic/Debug.hh"
#include "generic/RootSet.hh"
#include "generic/UniqueString.hh"
#include "generic/TaskStack.hh"
#include "generic/IOHandler.hh"
#include "generic/IODesc.hh"
#include "generic/Scheduler.hh"
#include "generic/Transients.hh"
#include "generic/Worker.hh"
#include "generic/Interpreter.hh"
#include "generic/Primitive.hh"
#include "generic/PushCallWorker.hh"
#include "generic/BindFutureWorker.hh"
#include "generic/Unpickler.hh"
#include "generic/Pickler.hh"
#include "generic/Profiler.hh"
#include "generic/Broker.hh"
#include "generic/Authoring.hh"
#include "generic/JitterGenericData.hh"

SeamDll void InitSeam();

// These must be extern "C" because the symbols are accessed
// via GetProcAddress/dlsym.  We cannot use the SeamDll macro
// because it would expand to __declspec(dllexport) here.
#if defined(__MINGW32__) || defined(_MSC_VER)
extern "C" __declspec(dllexport) void Start(int argc, const char *argv[]);
extern "C" __declspec(dllexport) Worker::Result Load(String *name);
#else
extern "C" void Start(int argc, const char *argv[]);
extern "C" Worker::Result Load(String *name);
#endif

#endif
