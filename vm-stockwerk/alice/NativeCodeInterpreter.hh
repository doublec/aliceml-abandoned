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
#pragma interface "alice/NativeCodeInterpreter.hh"
#endif

#include "Seam.hh"
#include "alice/Base.hh"

typedef Worker::Result (*native_fun)(class NativeCodeFrame *);

class AliceDll NativeCodeInterpreter : public Interpreter {
public:
  // Exported Code
  static word continuation[4];
  // Exported NativeCodeInterpreter Instance
  static NativeCodeInterpreter *self;
  // NativeCodeInterpreter Constructor
  NativeCodeInterpreter() : Interpreter() {}
  // NativeCodeInterpreter Static Constructor
  static void Init() {
    self = new NativeCodeInterpreter();
  }
  static StackFrame *FastPushCall(word continuation, Closure *closure);
  // Handler Methods
  virtual Transform *GetAbstractRepresentation(ConcreteRepresentation *);
  // Frame Handling
  virtual void PushCall(Closure *closure);
  virtual u_int GetFrameSize(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *sFrame);
  virtual Result Handle(word data);
  virtual u_int GetInArity(ConcreteCode *concreteCode);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
#if PROFILE
  // Profiling
  virtual word GetProfileKey(StackFrame *frame);
  virtual word GetProfileKey(ConcreteCode *concreteCode);
  virtual String *GetProfileName(StackFrame *frame);
  virtual String *GetProfileName(ConcreteCode *concreteCode);
#endif
};

#endif
