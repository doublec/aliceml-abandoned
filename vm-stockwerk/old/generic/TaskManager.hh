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

#ifndef __GENERIC__TASK_MANAGER_HH__
#define __GENERIC__TASK_MANAGER_HH__

#include "generic/ConcreteCode.hh"

class TaskStack;
class Closure;

class TaskManager {
public:
  class Result {
  public:
    enum Code {
      CONTINUE, // nargs == -1: single argument / nargs >= 0: flattened tuple
      PREEMPT,  // nargs == -1: single argument / nargs >= 0: flattened tuple
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
  virtual void PopFrame(TaskStack *taskStack) = 0; //--** remove
  //--** virtual u_int PurgeFrame(TaskStack *taskStack, u_int offset) = 0;

  // Execution:
  virtual Result Run(TaskStack *taskStack, int nargs) = 0;
  //--** virtual Result Handle(TaskStack *taskStack, word exn) = 0;
};

#endif __GENERIC__TASK_MANAGER_HH__
