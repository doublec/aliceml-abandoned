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

#ifndef __EMULATOR__VECTOR_TABULATE_INTERPRETER_HH__
#define __EMULATOR__VECTOR_TABULATE_INTERPRETER_HH__

#if defined(INTERFACE)
#pragma interface "emulator/VectorTabulateInterpreter.hh"
#endif

class TaskStack;
class Vector;

class VectorTabulateInterpreter : public Interpreter {
private:
  static VectorTabulateInterpreter *self;
public:
  // VectorTabulateInterpreter Constructor
  VectorTabulateInterpreter() : Interpreter() {}
  // VectorTabulateInterpreter Static Constructor
  static void Init() {
    self = new VectorTabulateInterpreter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack,
			Vector *vector, word fun, int i, int n);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

#endif
