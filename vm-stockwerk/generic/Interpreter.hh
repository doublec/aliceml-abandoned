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
  virtual Block *GetAbstractRepresentation();
  // Argument Creation
  static inline word EmptyArg() {
    Block *p = Store::AllocBlock(EMPTYARG_LABEL, 1);
    p->InitArg(0, Store::IntToWord(0));
    return p->ToWord();
  }
  static inline word OneArg(word value) {
    Block *p = Store::AllocBlock(ONEARG_LABEL, 1);
    p->InitArg(0, value);
    return p->ToWord();
  }
  static inline Block *TupArgs(u_int size) {
    return Store::AllocBlock(TUPARGS_LABEL, size);
  }
  // Calling Convention Conversion
  static word Construct(word args);
  // Returns IntToWord(0) if need to REQUEST argument
  // Presets Scheduler::currentData
  static word Deconstruct(word args);
  // Frame Handling
  virtual void PushCall(TaskStack *taskStack, Closure *closure);
  virtual void PurgeFrame(TaskStack *taskStack);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack) = 0;
  virtual Result Handle(word exn, word debug, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify() = 0;
  virtual const char *ToString(word args, TaskStack *taskStack) = 0;
};

#endif
