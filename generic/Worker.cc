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

#if defined(INTERFACE)
#pragma implementation "generic/Worker.hh"
#endif

#include "generic/Worker.hh"
#include "generic/Scheduler.hh"
#include "generic/Backtrace.hh"
#include "generic/Tuple.hh"

#if PROFILE
#include "generic/String.hh"
#endif

// Calling Convention Conversion
void Worker::Construct() {
  u_int nArgs = Scheduler::nArgs;
  switch (nArgs) {
  case 0:
    Scheduler::currentArgs[0] = Store::IntToWord(0);
    break;
  case Scheduler::ONE_ARG:
    return;
  default:
    {
      Tuple *tuple = Tuple::New(nArgs);
      for (u_int i = nArgs; i--; )
	tuple->Init(i, Scheduler::currentArgs[i]);
      Scheduler::currentArgs[0] = tuple->ToWord();
    }
    break;
  }
  Scheduler::nArgs = Scheduler::ONE_ARG;
}

u_int Worker::Deconstruct() {
  switch (Scheduler::nArgs) {
  case 0:
    return 0;
  case Scheduler::ONE_ARG:
    {
      word arg = Scheduler::currentArgs[0];
      Transient *t = Store::WordToTransient(arg);
      if (t == INVALID_POINTER) { // is determined
	Tuple *tuple = Tuple::FromWord(arg);
	Assert(tuple != INVALID_POINTER);
	Scheduler::nArgs = static_cast<Block *>(tuple)->GetSize(); //--**
	Assert(Scheduler::nArgs <= Scheduler::maxArgs);
	for (u_int i = Scheduler::nArgs; i--; )
	  Scheduler::currentArgs[i] = tuple->Sel(i);
	return 0;
      } else { // need to request
	Scheduler::currentData = arg;
	return 1;
      }
    }
  default:
    return 0;
  }
}

//
// Worker virtual functions: default implementations
//
void Worker::PurgeFrame(StackFrame *) {
  return; // default: nothing to do
}

Worker::Result Worker::Handle(word) {
  // default: pass the exception up the stack
  return RAISE;
}

#if PROFILE
word Worker::GetProfileKey(StackFrame *) {
  return Store::UnmanagedPointerToWord(this);
}

String *Worker::GetProfileName(StackFrame *) {
  return String::New(this->Identify());
}
#endif
