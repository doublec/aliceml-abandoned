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

#ifndef __ALICE__PRIMITIVES__AUTHORING_HH__
#define __ALICE__PRIMITIVES__AUTHORING_HH__

#include "emulator/NativeAuthoring.hh"
#include "emulator/Alice.hh"
#include "emulator/PrimitiveTable.hh"

#define DECLARE_ARRAY(array, x) DECLARE_BLOCKTYPE(Array, array, x)
#define DECLARE_CELL(cell, x) DECLARE_BLOCKTYPE(Cell, cell, x)
#define DECLARE_CLOSURE(closure, x) DECLARE_BLOCKTYPE(Closure, closure, x)
#define DECLARE_GLOBAL_STAMP(gs, x) DECLARE_BLOCKTYPE(GlobalStamp, gs, x)
#define DECLARE_REAL(real, x) DECLARE_BLOCKTYPE(Real, real, x)
#define DECLARE_STRING(string, x) DECLARE_BLOCKTYPE(String, string, x)
#define DECLARE_THREAD(thread, x) DECLARE_BLOCKTYPE(Thread, thread, x)
#define DECLARE_VECTOR(vector, x) DECLARE_BLOCKTYPE(Vector, vector, x)

//--** does not work for infinite lists
#define DECLARE_LIST_ELEMS(tagVal, length, x, cmd)			\
  u_int length = 0;							\
  TagVal *tagVal;							\
  { word list = x;						\
    while ((tagVal = TagVal::FromWord(list)) != INVALID_POINTER) {	\
      cmd;								\
      length++;								\
      list = tagVal->Sel(1);						\
    }									\
    if (Store::WordToInt(list) == INVALID_INT) { REQUEST(list); }	\
  }

#define DECLARE_LIST(tagVal, length, x)					\
  DECLARE_LIST_ELEMS(tagVal, length, x, ;)

#endif
