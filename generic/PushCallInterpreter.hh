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

#ifndef __GENERIC__PUSH_CALL_INTERPRETER_HH__
#define __GENERIC__PUSH_CALL_INTERPRETER_HH__

#if defined(INTERFACE)
#pragma interface "generic/PushCallInterpreter.hh"
#endif

#include "generic/Interpreter.hh"

class DllExport PushCallInterpreter: public Interpreter {
private:
  // PushCallInterpreter Constructor
  PushCallInterpreter(): Interpreter() {}
public:
  // Exported PushCallInterpreter Instance
  static PushCallInterpreter *self;
  // PushCallInterpreter Static Constructor
  static void Init() {
    self = new PushCallInterpreter();
  }
  // Frame Handling
  static void PushFrame(word closure);
  static void PushFrame(Thread *thread, word closure);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

#endif
