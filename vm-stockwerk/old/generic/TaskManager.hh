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

#ifndef __SCHEDULER__INTERPRETER_HH__
#define __SCHEDULER__INTERPRETER_HH__

#include "scheduler/ConcreteCode.hh"

class TaskStack;
class Closure;

class Interpreter {
public:
  class Result {
  public:
    enum Code {
      CONTINUE, // nargs == -1: OneArg / nargs >= 0: TupArgs
      PREEMPT,  // nargs == -1: OneArg / nargs >= 0: TupArgs
      RAISE,    // (nargs unused)
      REQUEST,  // nargs > 0
      TERMINATE // (nargs unused)
    };

    Code code;
    int nargs;

    Result(Code c): code(c) {
      Assert(c == RAISE || c == TERMINATE);
    }
    Result(Code c, int n): code(c), nargs(n) {
      Assert(c == CONTINUE || c == PREEMPT || c == REQUEST);
    }
  };

  // Handling stack frames:
  virtual void PushCall(TaskStack *taskStack, Closure *closure) = 0;
  virtual void PopFrame(TaskStack *taskStack) = 0;
  //--** virtual u_int PurgeFrame(TaskStack *taskStack, u_int offset) = 0;

  // Execution:
  virtual Result Run(TaskStack *taskStack, int nargs) = 0;
};

#endif __SCHEDULER__INTERPRETER_HH__
