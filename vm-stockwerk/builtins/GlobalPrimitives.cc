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

#if defined(INTERFACE)
#pragma implementation "builtins/GlobalPrimitives.hh"
#endif

#include "builtins/Primitive.hh"
#include "builtins/GlobalPrimitives.hh"

word GlobalPrimitives::Future_Future;
word GlobalPrimitives::Future_await;
word GlobalPrimitives::General_Chr;
word GlobalPrimitives::General_Div;
word GlobalPrimitives::General_Overflow;
word GlobalPrimitives::General_Size;
word GlobalPrimitives::General_Subscript;
word GlobalPrimitives::Hole_Cyclic;
word GlobalPrimitives::Hole_Hole;
word GlobalPrimitives::Internal_raise;

void GlobalPrimitives::Init() {
  General_Chr = Primitive::Lookup(String::New("General.Chr"));
  General_Div = Primitive::Lookup(String::New("General.Div"));
  General_Overflow = Primitive::Lookup(String::New("General.Overflow"));
  General_Size = Primitive::Lookup(String::New("General.Size"));
  General_Subscript = Primitive::Lookup(String::New("General.Subscript"));
  Future_Future = Primitive::Lookup(String::New("Future.Future"));
  Future_await = Primitive::Lookup(String::New("Future.await"));
  Hole_Cyclic = Primitive::Lookup(String::New("Hole.Cyclic"));
  Hole_Hole = Primitive::Lookup(String::New("Hole.Hole"));
  Internal_raise = Primitive::Lookup(String::New("Internal.raise"));
}
