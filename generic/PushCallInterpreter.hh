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

#ifndef __EMULATOR_PUSHCALL_INTERPRETER_HH__
#define __EMULATOR_PUSHCALL_INTERPRETER_HH__

#if defined(INTERFACE)
#pragma interface "emulator/PushCallInterpreter.hh"
#endif

#include "emulator/Interpreter.hh"

class PushCallInterpreter : public Interpreter {
public:
  // Exported PushCallInterpreter Instance
  static PushCallInterpreter *self;
  // PushCallInterpreter Constructor
  PushCallInterpreter() : Interpreter() {}
  // PushCallInterpreter Static Constructor
  static void Init() {
    self = new PushCallInterpreter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack, word closure);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

#endif
