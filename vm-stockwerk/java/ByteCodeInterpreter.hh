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
#ifndef __JAVA__BYTE_CODE_INTERPRETER_HH__
#define __JAVA__BYTE_CODE_INTERPRETER_HH__

#if defined(INTERFACE)
#pragma interface "java/ByteCodeInterpreter.hh"
#endif

#include "generic/Interpreter.hh"

class DllExport ByteCodeInterpreter : public Interpreter {
public:
  // Exported ByteCodeIntpreter Instance
  static ByteCodeInterpreter *self;
  // ByteCodeInterpreter Constructor
  ByteCodeInterpreter() : Interpreter() {}
  // ByteCodeInterpreter Static Constructor
  static void Init();
  // Handler Methods
  virtual Block *GetAbstractRepresentation(ConcreteRepresentation *);
  // Frame Handling
  virtual void PushCall(Closure *closure);
  // Execution
  virtual Result Run();
  virtual Result Handle();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

#endif
