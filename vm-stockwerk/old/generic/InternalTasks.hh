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

//--** This file should live elsewhere

#ifndef __GLOBAL_PRIMITIVES_HH__
#define __GLOBAL_PRIMITIVES_HH__

#if defined(INTERFACE)
#pragma interface "builtins/GlobalPrimitives.hh"
#endif

#include "store/store.hh"

//
// Note: Everything entered into this table must also be
// referenced through the primitive table (to make sure it
// is reached during GC).
//

class GlobalPrimitives {
public:
  static word Future_Future;
  static word Future_await;
  static word General_Chr;
  static word General_Div;
  static word General_Overflow;
  static word General_Size;
  static word General_Subscript;
  static word Hole_Cyclic;
  static word Hole_Hole;
  static word Internal_applyUnit;
  static word Internal_bind;
  static word Internal_byneedHandler;
  static word Internal_popHandler;
  static word Internal_raise;
  static word Vector_tabulate_cont;

  static void Init();
};

#endif __GLOBAL_PRIMITIVES_HH__
