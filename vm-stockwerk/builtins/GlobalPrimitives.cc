//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#pragma implementation "builtins/GlobalPrimitives.hh"

#include "builtins/Primitive.hh"
#include "builtins/GlobalPrimitives.hh"

void GlobalPrimitives::Init() {
  General_Chr = Primitive::Lookup("General.Chr");
  General_Div = Primitive::Lookup("General.Div");
  General_Overflow = Primitive::Lookup("General.Overflow");
  General_Size = Primitive::Lookup("General.Size");
  General_Subscript = Primitive::Lookup("General.Subscript");
  Future_Future = Primitive::Lookup("Future.Future");
  Future_await = Primitive::Lookup("Future.await");
  Hole_Cyclic = Primitive::Lookup("Hole.Cyclic");
  Hole_Hole = Primitive::Lookup("Hole.Hole");
  Internal_raise = Primitive::Lookup("Internal.raise");
};
