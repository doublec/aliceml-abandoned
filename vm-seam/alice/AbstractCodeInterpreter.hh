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

#ifndef __ALICE__ABSTRACT_CODE_INTERPRETER_HH__
#define __ALICE__ABSTRACT_CODE_INTERPRETER_HH__

#if defined(INTERFACE)
#pragma interface "alice/AbstractCodeInterpreter.hh"
#endif

#include "generic/Interpreter.hh"

class AbstractCodeInterpreter: public Interpreter {
public:
  // Exported AbstractCodeInterpreter Instance
  static AbstractCodeInterpreter *self;
  // AbstractCodeInterpreter Constructor
  AbstractCodeInterpreter() : Interpreter() {}
  // AbstractCodeInterpreter Static Constructor
  static void Init() {
    self = new AbstractCodeInterpreter();
  }
  // Handler Methods
  virtual Block *GetAbstractRepresentation(Block *blockWithHandler);
  // Frame Handling
  virtual void PushCall(TaskStack *taskStack, Closure *closure);
  // Execution
  virtual Result Run(TaskStack *taskStack);
  virtual Result Handle(word exn, Backtrace *trace, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

#endif
