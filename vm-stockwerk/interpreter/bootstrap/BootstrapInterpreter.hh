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

class BootstrapInterpreter: Interpreter {
public:
  virtual result Run(int nargs, TaskStack *&taskStack, word &data);
  virtual Thread *NewThread(word code);
};

#endif
