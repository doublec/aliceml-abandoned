//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__INTERPRETER_HH__
#define __GENERIC__INTERPRETER_HH__

#if defined(INTERFACE)
#pragma interface "generic/Interpreter.hh"
#endif

#include "generic/ConcreteRepresentationHandler.hh"
#include "generic/Worker.hh"

class Closure;
class Transform;
class ConcreteCode;
#if PROFILE
class String;
#endif

class SeamDll Interpreter:
  public ConcreteRepresentationHandler, public Worker {
public:
  typedef Result (*function)();
  // Interpreter Constructor
  Interpreter() {}
  // ConcreteRepresentation Methods
  virtual Transform *GetAbstractRepresentation(ConcreteRepresentation *);
  // Frame Handling
  virtual void PushCall(Closure *closure) = 0;
  // Runtime compilation
  //   returns INVALID_INT if unknown; Scheduler::ONE_ARG for single argument
  virtual u_int GetInArity(ConcreteCode *concreteCode) = 0;
  //   returns NULL if none
  virtual function GetCFunction();
#if PROFILE
  // Profiling
  virtual word GetProfileKey(StackFrame *sFrame);
  virtual String *GetProfileName(StackFrame *sFrame);
  virtual word GetProfileKey(ConcreteCode *concreteCode);
  virtual String *GetProfileName(ConcreteCode *concreteCode);
#endif
};

#endif
