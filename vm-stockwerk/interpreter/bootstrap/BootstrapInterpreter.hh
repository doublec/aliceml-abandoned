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

#ifndef __INTERPRETER__BOOTSTRAP__BOOTSTRAP_INTERPRETER_HH__
#define __INTERPRETER__BOOTSTRAP__BOOTSTRAP_INTERPRETER_HH__

#pragma interface "interpreter/bootstrap/BootstrapInterpreter.hh"

#include "scheduler/Interpreter.hh"

class BootstrapInterpreter: public Interpreter {
public:
  ConcreteCode *Prepare(word abstractCode);
  void PushCall(TaskStack *taskStack, word closure);
  void PopFrame(TaskStack *taskStack);
  Result Run(TaskStack *taskStack, int nargs);
};

#endif __INTERPRETER__BOOTSTRAP__BOOTSTRAP_INTERPRETER_HH__
