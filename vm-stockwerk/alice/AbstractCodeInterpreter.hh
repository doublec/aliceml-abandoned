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
private:
  // AbstractCodeInterpreter Constructor
  AbstractCodeInterpreter() : Interpreter() {}
public:
  // Exported AbstractCodeInterpreter Instance
  static AbstractCodeInterpreter *self;
  // AbstractCodeInterpreter Static Constructor
  static void Init();
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
#if defined(ALICE_PROFILE)
  // Profiling
  virtual word GetProfileKey(StackFrame *frame);
  virtual word GetProfileKey(ConcreteCode *concreteCode);
  virtual String *GetProfileName(StackFrame *frame);
  virtual String *GetProfileName(ConcreteCode *concreteCode);
#endif
};

#endif
