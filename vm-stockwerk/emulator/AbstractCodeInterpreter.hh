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

#ifndef __INTERPRETER__ABSTRACT_CODE_INTERPRETER_HH__
#define __INTERPRETER__ABSTRACT_CODE_INTERPRETER_HH__

#if defined(INTERFACE)
#pragma interface "emulator/AbstractCodeInterpreter.hh"
#endif

#include "emulator/Interpreter.hh"

class AbstractCodeInterpreter : public Interpreter {
public:
  // Exported AbstractCodeInterpreter Instance
  static AbstractCodeInterpreter *self;
  // AbstractCodeInterpreter Constructor
  AbstractCodeInterpreter() : Interpreter() {}
  // AbstractCodeInterpreter Static Constructor
  static void Init() {
    self = new AbstractCodeInterpreter();
  }
  // Frame Handling
  virtual void PushCall(TaskStack *taskStack, Closure *closure);
  virtual void PurgeFrame(TaskStack *taskStack);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack);
  virtual Result Handle(word exn, word debug, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual const char *ToString(word args, TaskStack *taskStack);
};

#endif
