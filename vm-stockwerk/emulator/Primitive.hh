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

#ifndef __EMULATOR_PRIMITIVE_HH__
#define __EMULATOR_PRIMITIVE_HH__

#if defined(INTERFACE)
#pragma interface "emulator/Primitive.hh"
#endif

#include "emulator/Interpreter.hh"

class Primitive {
public:
  // Primitive C Type
  typedef Interpreter::Result (*function)(word, TaskStack *);
  // Primitive Function
  static word MakeFunction(const char *name, function value,
			   u_int arity, bool sited = false);
  static word MakeClosure(const char *name, function value,
			  u_int arity, bool sited = false);
};

#endif
