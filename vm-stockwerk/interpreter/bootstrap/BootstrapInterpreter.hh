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

#ifndef __BOOTSTRAP_INTERPRETER_HH__
#define __BOOTSTRAP_INTERPRETER_HH__

#include "scheduler/Interpreter.hh"

class BootstrapInterpreter: public Interpreter {
public:
  ConcreteCode *Prepare(word abstractCode);
  void PushCall(TaskStack *taskStack, word closure);
  void PopFrame(TaskStack *taskStack);
  Result Run(TaskStack *taskStack, int nargs);
};

#endif __BOOTSTRAP_INTERPRETER_HH__
