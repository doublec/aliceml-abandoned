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

#include "Seam.hh"
#include "java/Base.hh"

class JavaDll ByteCodeInterpreter : public Interpreter {
private:
  ByteCodeInterpreter() : Interpreter() {}
public:
  static ByteCodeInterpreter *self;

  static void Init();

  virtual Transform *GetAbstractRepresentation(ConcreteRepresentation *);

  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Result Run(StackFrame *sFrame);
  virtual Result Handle(word data);
  virtual u_int GetInArity(ConcreteCode *concreteCode);
  virtual void PushCall(Closure *closure);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);

  //--** move this to the JavaLanguageLayer?:
  static void FillStackTraceElement(word wFrame, Object *stackTraceElement);
};

#endif
