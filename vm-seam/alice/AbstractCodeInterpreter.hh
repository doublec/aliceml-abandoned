//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE__ABSTRACT_CODE_INTERPRETER_HH__
#define __ALICE__ABSTRACT_CODE_INTERPRETER_HH__

#if defined(INTERFACE)
#pragma interface "alice/AbstractCodeInterpreter.hh"
#endif

#include "Seam.hh"
#include "alice/Data.hh"

class AliceConcreteCode;

class AliceDll AbstractCodeInterpreter: public Interpreter {
private:
  AbstractCodeInterpreter(): Interpreter() {}
public:
  static AbstractCodeInterpreter *self;

  static void Init();
  static String *MakeProfileName(TagVal *abstractCode);
  static void DumpAliceFrame(word funCoord, bool handler, word coord, bool inlined = false, std::ostream& out = std::cerr);
  static word GetCloseConcreteCode(word parentConcreteCode, TagVal *closeInstr);;

  virtual Transform *GetAbstractRepresentation(ConcreteRepresentation *);

  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Result Run(StackFrame *sFrame);
  virtual Result Handle(word data);
  virtual u_int GetInArity(ConcreteCode *concreteCode);
  virtual u_int GetOutArity(ConcreteCode *concreteCode);
  virtual void PushCall(Closure *closure);
  void PushCall_Internal(AliceConcreteCode *concreteCode, Closure *closure);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame, std::ostream& out);

#if PROFILE
  virtual word GetProfileKey(StackFrame *frame);
  virtual word GetProfileKey(ConcreteCode *concreteCode);
  virtual String *GetProfileName(StackFrame *frame);
  virtual String *GetProfileName(ConcreteCode *concreteCode);
#endif

};

#endif
