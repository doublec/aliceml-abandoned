//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE__NATIVE_CODE_INTERPRETER_HH__
#define __ALICE__NATIVE_CODE_INTERPRETER_HH__

#if defined(INTERFACE)
#pragma interface "alice/NaticeCodeInterpreter.hh"
#endif

#include "generic/Interpreter.hh"

typedef Interpreter::Result (*native_fun)(class NativeCodeFrame *);

class NativeCodeInterpreter : public Interpreter {
public:
  // Exported NativeCodeInterpreter Instance
  static NativeCodeInterpreter *self;
  // NativeCodeInterpreter Constructor
  NativeCodeInterpreter() : Interpreter() {}
  // NativeCodeInterpreter Static Constructor
  static void Init() {
    self = new NativeCodeInterpreter();
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
