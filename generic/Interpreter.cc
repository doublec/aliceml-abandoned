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
Transform *Interpreter::GetAbstractRepresentation(ConcreteRepresentation *) {
  return INVALID_POINTER; // default: may not be pickled
}

Interpreter::function Interpreter::GetCFunction() {
  return NULL; // default: no corresponding C function
}

#if PROFILE
word Interpreter::GetProfileKey(StackFrame *) {
  return Store::UnmanagedPointerToWord(this);
}

String *Interpreter::GetProfileName(StackFrame *) {
  return String::New(this->Identify());
}

word Interpreter::GetProfileKey(ConcreteCode *concreteCode) {
  return concreteCode->ToWord();
}

String *Interpreter::GetProfileName(ConcreteCode *) {
  return String::New(this->Identify());
}
#endif
