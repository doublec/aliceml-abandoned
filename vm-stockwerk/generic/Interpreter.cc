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
#pragma implementation "generic/Interpreter.hh"
#endif

#include "generic/Interpreter.hh"
#include "generic/TaskStack.hh"
#include "generic/Scheduler.hh"
#include "generic/Backtrace.hh"

#if PROFILE
#include "generic/String.hh"
#include "generic/ConcreteCode.hh"
#endif

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

u_int Interpreter::Deconstruct() {
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
// Interpreter virtual functions: default implementations
//
Block *Interpreter::GetAbstractRepresentation(Block *) {
  return INVALID_POINTER; // default: may not be pickled
}

void Interpreter::PushCall(TaskStack *, Closure *) {
  Error("Interpreter::PushCall must never be called");
}

void Interpreter::PurgeFrame(word) {
  return; // default: nothing to do
}

Interpreter::Result Interpreter::Handle(TaskStack *taskStack) {
  // default: pass the exception up the stack
  Scheduler::currentBacktrace->Enqueue(taskStack->GetFrame());
  taskStack->PopFrame();
  return Interpreter::RAISE;
}

#if PROFILE
word Interpreter::GetProfileKey(StackFrame *) {
  return Store::UnmanagedPointerToWord(this);
}

word Interpreter::GetProfileKey(ConcreteCode *concreteCode) {
  return concreteCode->ToWord();
}

String *Interpreter::GetProfileName(StackFrame *) {
  return String::New(this->Identify());
}

String *Interpreter::GetProfileName(ConcreteCode *) {
  return String::New(this->Identify());
}
#endif
