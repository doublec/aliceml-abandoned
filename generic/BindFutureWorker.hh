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

#ifndef __EMULATOR__BYNEED_INTERPRETER_HH__
#define __EMULATOR__BYNEED_INTERPRETER_HH__

#if defined(INTERFACE)
#pragma interface "emulator/ByneedInterpreter.hh"
#endif

#include "emulator/Interpreter.hh"

class Transient;

class ByneedInterpreter : public Interpreter {
public:
  // Exported ByneedInterpreter Instance
  static ByneedInterpreter *self;
  // ByneedInterpreter Constructor
  ByneedInterpreter() : Interpreter() {}
  // ByneedInterpreter Static Constructor
  static void Init() {
    self = new ByneedInterpreter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack, Transient *future);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack);
  virtual Result Handle(word exn, Backtrace *debug, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

#endif
