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

#ifndef __GENERIC__INTERPRETER_HH__
#define __GENERIC__INTERPRETER_HH__

#include "store/Store.hh"
#include "generic/Tuple.hh"
#include "generic/ConcreteRepresentationHandler.hh"

class TaskStack;
class Closure;
class Backtrace;

#if PROFILE
class StackFrame;
class ConcreteCode;
class String;
#endif

class Interpreter : public ConcreteRepresentationHandler {
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
  // ConcreteRepresentation Methods
  virtual Block *GetAbstractRepresentation(Block *blockWithHandler);
  // Calling Convention Conversion
  static void Construct();
  //   Deconstruct returns 1 iff argument needs to be requested,
  //   in which case it sets Scheduler::currentData as a side-effect;
  //   returns 0 iff deconstruction was immediately successful
  static u_int Deconstruct();
  // Frame Handling
  virtual void PushCall(TaskStack *taskStack, Closure *closure);
  virtual void PurgeFrame(word frame);
  // Execution
  virtual Result Run(TaskStack *taskStack) = 0;
  virtual Result Handle(TaskStack *taskStack);
  // Debugging
  virtual const char *Identify() = 0;
  virtual void DumpFrame(word frame) = 0;
#if PROFILE
  // Profiling
  virtual word GetProfileKey(StackFrame *frame);
  virtual word GetProfileKey(ConcreteCode *concreteCode);
  virtual String *GetProfileName(StackFrame *frame);
  virtual String *GetProfileName(ConcreteCode *concreteCode);
#endif
};

#endif
