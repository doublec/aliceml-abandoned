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

#if defined(INTERFACE)
#pragma implementation "generic/Interpreter.hh"
#endif

#include "generic/Interpreter.hh"
//#include "generic/Scheduler.hh"
//#include "generic/Backtrace.hh"
//#include "generic/Tuple.hh"

#if PROFILE
#include "generic/String.hh"
#include "generic/ConcreteCode.hh"
#endif

//
// Interpreter virtual functions: default implementations
//
Block *Interpreter::GetAbstractRepresentation(Block *) {
  return INVALID_POINTER; // default: may not be pickled
}

u_int Interpreter::GetArity() {
  return 0; // to be done
}

Interpreter::function Interpreter::GetCFunction() {
  return NULL; // default: no c function available
}

#if PROFILE
word Interpreter::GetProfileKey(ConcreteCode *concreteCode) {
  return concreteCode->ToWord();
}

String *Interpreter::GetProfileName(ConcreteCode *) {
  return String::New(this->Identify());
}
#endif
