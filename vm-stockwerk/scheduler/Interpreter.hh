//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __INTERPRETER_HH__
#define __INTERPRETER_HH__

#include "scheduler/ConcreteCode.hh"

class TaskStack;

class Interpreter {
public:
  enum result {
    CONTINUE, // out = nargs; nargs == -1: OneArg / nargs >= 0: TupArgs
    PREEMPT,  // out = nargs; nargs == -1: OneArg / nargs >= 0: TupArgs
    RAISE,    // out = exn
    REQUEST,  // out = #vars (placed on stack)
    TERMINATE // (out unused)
  };

  // Handling code:
  virtual ConcreteCode *Prepare(word abstractCode) = 0;

  // Handling stack frames:
  virtual void PushCall(TaskStack *taskStack, word closure) = 0;
  virtual void PopFrame(TaskStack *taskStack) = 0;

  // Execution:
  virtual result Run(TaskStack *taskStack, int nargs, word &out) = 0;
};

#endif __INTERPRETER_HH__
