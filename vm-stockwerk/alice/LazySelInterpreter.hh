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

#ifndef __EMULATOR_LAZY_SELECTION_INTERPRETER_HH__
#define __EMULATOR_LAZY_SELECTION_INTERPRETER_HH__

#if defined(INTERFACE)
#pragma implementation "emulator/LazySelectionInterpreter.hh"
#endif

#include "emulator/Interpreter.hh"

class LazySelectionInterpreter : public Interpreter {
public:
  // Exported LazySelectionInterpreter Instance
  static LazySelectionInterpreter *self;
  // LazySelectionInterpreter Constructor
  LazySelectionInterpreter() : Interpreter() {}
  // LazySelectionInterpreter Static Constructor
  static void Init() {
    self = new LazySelectionInterpreter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack, word tuple, int index);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual const char *ToString(word args, TaskStack *taskStack);
};

#endif
