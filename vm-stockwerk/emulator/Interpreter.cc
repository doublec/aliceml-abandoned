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
    Assert(0);
    return Store::IntToWord(0);
  }
}

word Interpreter::Deconstruct(word args) {
  Block *p = Store::WordToBlock(args);
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
	u_int nargs  = ((Block *) tuple)->GetSize();
	// Hack Alert: to be done
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
    Assert(0);
    return Store::IntToWord(0);
  }
}

//
// Interpreter Functions
//
void Interpreter::PrepareForGC(Block *) {
  return;
}

Block *Interpreter::GetAbstractRepresentation(Block *) {
  return INVALID_POINTER;
}

void Interpreter::PushCall(TaskStack *, Closure *) {
  Error("Interpreter::PushCall must never be called");
}

void Interpreter::PurgeFrame(TaskStack *) {
  return;
}

Interpreter::Result Interpreter::Handle(word exn, Backtrace *trace,
					TaskStack *taskStack) {
  // Default Handler: Clear Frame until Handler is found
  trace->Enqueue(taskStack->GetFrame());
  taskStack->PopFrame();
  Scheduler::currentBacktrace = trace;
  Scheduler::currentData      = exn;
  return Interpreter::RAISE;
}
