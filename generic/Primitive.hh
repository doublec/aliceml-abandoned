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

#ifndef __GENERIC__PRIMITIVE_HH__
#define __GENERIC__PRIMITIVE_HH__

#if defined(INTERFACE)
#pragma interface "generic/Primitive.hh"
#endif

#include "generic/Interpreter.hh"

class SeamDll Primitive {
public:
  static word MakeFunction(const char *name, Interpreter::function function,
			   u_int inArity, u_int outArity, Transform *abstract);

  // Push a new primitive frame and call primitive directly
  static Worker::Result Execute(Interpreter *interpreter);
};

#endif
