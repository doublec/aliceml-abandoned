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
word Interpreter::Construct(word args) {
  Block *p = Store::WordToBlock(args);
  Assert(p != INVALID_POINTER);
  switch (p->GetLabel()) {
  case EMPTYARG_LABEL:
    return p->GetArg(0);
  case ONEARG_LABEL:
    return p->GetArg(0);
  case TUPARGS_LABEL:
    {
      u_int nargs = p->GetSize(); // Hack Alert
      Tuple *tuple = Tuple::New(nargs);
      for (u_int i = nargs; i--;) {
	tuple->Init(i, p->GetArg(i));
      }
      return tuple->ToWord();
    }
  default:
    Error("Interpreter::Construct: non-argument block");
  }
}

word Interpreter::Deconstruct(word args) {
  Block *p = Store::WordToBlock(args);
  Assert(p != INVALID_POINTER);
  switch (p->GetLabel()) {
  case EMPTYARG_LABEL:
    return args;
  case ONEARG_LABEL:
    {
      word arg     = p->GetArg(0);
      Transient *t = Store::WordToTransient(arg);
      // Found Block
      if (t == INVALID_POINTER) {
	Tuple *tuple = Tuple::FromWord(arg);
	Assert(tuple != INVALID_POINTER);
	u_int nargs = static_cast<Block *>(tuple)->GetSize(); // to be done
	Block *args_outp = Interpreter::TupArgs(nargs); 
	for (u_int i = nargs; i--;) {
	  args_outp->InitArg(i, tuple->Sel(i));
	}
	return args_outp->ToWord();
      }
      // Need to wait
      else {
	Scheduler::currentData = arg;
	return Store::IntToWord(0);
      }
    }
  case TUPARGS_LABEL:
    return args;
  default:
    Error("Interpreter::Deconstruct: non-argument block");
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
