//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __EMULATOR__INTERPRETER_HH__
#define __EMULATOR__INTERPRETER_HH__

#include "store/Store.hh"
#include "emulator/Tuple.hh"

class TaskStack;
class Closure;
class Backtrace;

class Interpreter : public Handler {
public:
  enum Result {
    CONTINUE,
    PREEMPT,
    RAISE,
    REQUEST,
    TERMINATE
  };
  // Interpreter Constructor
  Interpreter() {}
  // Handler Methods
  virtual void PrepareForGC(Block *p);
  virtual Block *GetAbstractRepresentation(Block *blockWithHandler);
  // Calling Convention Conversion
  static void Construct();
  //   Deconstruct returns true iff argument needs to be requested;
  //   Sets Scheduler::currentData as a side-effect
  static bool Deconstruct();
  // Frame Handling
  virtual void PushCall(TaskStack *taskStack, Closure *closure);
  virtual void PurgeFrame(word frame);
  // Execution
  virtual Result Run(TaskStack *taskStack) = 0;
  virtual Result Handle(word exn, Backtrace *trace, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify() = 0;
  virtual void DumpFrame(word frame) = 0;
};

#endif
