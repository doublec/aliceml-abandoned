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

#ifndef __GENERIC__BYNEED_INTERPRETER_HH__
#define __GENERIC__BYNEED_INTERPRETER_HH__

#if defined(INTERFACE)
#pragma interface "generic/ByneedInterpreter.hh"
#endif

#include "generic/Interpreter.hh"

class Transient;

class ByneedInterpreter : public Interpreter {
private:
  // ByneedInterpreter Constructor
  ByneedInterpreter() : Interpreter() {}
public:
  // Exported ByneedInterpreter Instance
  static ByneedInterpreter *self;
  // ByneedInterpreter Static Constructor
  static void Init() {
    self = new ByneedInterpreter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack, Transient *future);
  // Execution
  virtual Result Run(TaskStack *taskStack);
  virtual Result Handle(TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

#endif
