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

// Calling Convention Conversion
word Interpreter::Construct(word args) {
  Block *p = Store::WordToBlock(args);
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
	u_int nargs = p->GetSize();
	Block *args_outp = Interpreter::TupArgs(nargs); // Hack Alert
	for (u_int i = nargs; i--;) {
	  args_outp->InitArg(i, p->GetArg(i));
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
void Interpreter::PushCall(TaskStack *taskStack, word closure) {
  taskStack->PushCall(closure);
}

void Interpreter::PurgeFrame(TaskStack *taskStack) {
  return;
}

Interpreter::Result Interpreter::Handle(word exn, word /*debug*/,
					TaskStack *taskStack) {
  // Default Handler: Clear Frame until Handler is found
  //--** construct backtrace
  taskStack->PopFrame();
  Scheduler::currentData = exn;
  return Interpreter::RAISE;
}

const char *Interpreter::Identify() {
  return "Interpreter";
}

const char *Interpreter::ToString(word args, TaskStack *taskStack) {
  return "Interpreter::ToString";
}
