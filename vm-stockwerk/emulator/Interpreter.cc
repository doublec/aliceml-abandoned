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
#pragma implementation "emulator/Interpreter.hh"
#endif

#include "emulator/Interpreter.hh"
#include "emulator/TaskStack.hh"
#include "emulator/Scheduler.hh"
#include "emulator/Backtrace.hh"

// Calling Convention Conversion
void Interpreter::Construct() {
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

bool Interpreter::Deconstruct() {
  switch (Scheduler::nArgs) {
  case 0:
    Scheduler::nArgs = 0;
    return false;
  case Scheduler::ONE_ARG:
    {
      word arg = Scheduler::currentArgs[0];
      Transient *t = Store::WordToTransient(arg);
      if (t == INVALID_POINTER) { // is determined
	Tuple *tuple = Tuple::FromWord(arg);
	Assert(tuple != INVALID_POINTER);
	Scheduler::nArgs = static_cast<Block *>(tuple)->GetSize(); //--**
	Assert(Scheduler::nArgs < Scheduler::maxArgs);
	for (u_int i = Scheduler::nArgs; i--; )
	  Scheduler::currentArgs[i] = tuple->Sel(i);
	return false;
      } else { // need to request
	Scheduler::currentData = arg;
	return true;
      }
    }
  default:
    return false;
  }
}

//
// Interpreter virtual functions: default implementations
//
void Interpreter::PrepareForGC(Block *) {
  return; // default: nothing to do
}

Block *Interpreter::GetAbstractRepresentation(Block *) {
  return INVALID_POINTER; // default: may not be pickled
}

void Interpreter::PushCall(TaskStack *, Closure *) {
  Error("Interpreter::PushCall must never be called");
}

void Interpreter::PurgeFrame(word) {
  return; // default: nothing to do
}

Interpreter::Result
Interpreter::Handle(word exn, Backtrace *trace, TaskStack *taskStack) {
  // default: pass the exception up the stack
  trace->Enqueue(taskStack->GetFrame());
  taskStack->PopFrame();
  Scheduler::currentBacktrace = trace;
  Scheduler::currentData      = exn;
  return Interpreter::RAISE;
}
