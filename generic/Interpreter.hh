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

#if PROFILE
class ConcreteCode;
class String;
#endif

class DllExport Interpreter:
  public ConcreteRepresentationHandler, public Worker {
public:
  typedef Result (*function)();
  // Interpreter Constructor
  Interpreter() {}
  // ConcreteRepresentation Methods
  virtual Block *GetAbstractRepresentation(ConcreteRepresentation *);
  // Frame Handling
  virtual void PushCall(Closure *closure) = 0;
  // Runtime compilation
  virtual u_int GetArity();
  virtual function GetCFunction();
#if PROFILE
  // Profiling
  virtual word GetProfileKey(ConcreteCode *concreteCode);
  virtual String *GetProfileName(ConcreteCode *concreteCode);
#endif
};

#endif
