//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "adt/IntMap.hh"
#pragma implementation "adt/Queue.hh"
#pragma implementation "adt/Stack.hh"
#pragma implementation "generic/FinalizationSet.hh"
#pragma implementation "generic/Transform.hh"
#pragma implementation "generic/ConcreteRepresentationHandler.hh"
#pragma implementation "generic/ConcreteRepresentation.hh"
#pragma implementation "generic/ConcreteCode.hh"
#pragma implementation "generic/Closure.hh"
#pragma implementation "generic/Thread.hh"
#pragma implementation "generic/ThreadQueue.hh"
#pragma implementation "generic/Tuple.hh"
#pragma implementation "generic/String.hh"
#pragma implementation "generic/Backtrace.hh"
#pragma implementation "generic/Pickle.hh"
#pragma implementation "generic/Float.hh"
#pragma implementation "generic/Double.hh"
#if DEBUGGER
#pragma implementation "generic/DebuggerEvent.hh"
#pragma implementation "generic/DebugFrame.hh"
#endif
#endif

#include "adt/IntMap.hh"
#include "store/BaseMap.cc"
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
#if DEBUGGER
#include "generic/DebuggerEvent.hh"
#endif

template class BaseMap<IntKey>;
