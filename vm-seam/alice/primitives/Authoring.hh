//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE__PRIMITIVES__AUTHORING_HH__
#define __ALICE__PRIMITIVES__AUTHORING_HH__

#include "generic/NativeAuthoring.hh"
#include "alice/Data.hh"
#include "alice/PrimitiveTable.hh"

#define DECLARE_BOOL(b, x)				\
  bool b;						\
  {							\
    int i = Store::WordToInt(x);			\
    if (i == INVALID_INT) { REQUEST(x); } else b = i;	\
  }
#define DECLARE_ARRAY(array, x) DECLARE_BLOCKTYPE(Array, array, x)
#define DECLARE_CELL(cell, x) DECLARE_BLOCKTYPE(Cell, cell, x)
#define DECLARE_REAL(real, x) DECLARE_BLOCKTYPE(Real, real, x)
#define DECLARE_RECORD(record, x) DECLARE_BLOCKTYPE(Record, record, x)
#define DECLARE_TAGVAL(tagVal, x) DECLARE_BLOCKTYPE(TagVal, tagVal, x)
#define DECLARE_THREAD(thread, x) DECLARE_BLOCKTYPE(Thread, thread, x)
#define DECLARE_VECTOR(vector, x) DECLARE_BLOCKTYPE(Vector, vector, x)

//--** does not work for infinite lists
#define DECLARE_LIST_ELEMS(tagVal, length, x, cmd)			\
  u_int length = 0;							\
  TagVal *tagVal;							\
  { word list = x;							\
    while ((tagVal = TagVal::FromWord(list)) != INVALID_POINTER) {	\
      cmd;								\
      length++;								\
      list = tagVal->Sel(1);						\
    }									\
    if (Store::WordToInt(list) == INVALID_INT) { REQUEST(list); }	\
  } \
  tagVal = TagVal::FromWord(x);

#define DECLARE_LIST(tagVal, length, x)					\
  DECLARE_LIST_ELEMS(tagVal, length, x, ;)

#define RETURN_UNIT RETURN0

#define INIT_STRUCTURE(r, s1, s2, f, i, b)			\
  r->Init(s2, Primitive::MakeClosure(s1 "." s2, f, i, b));

#define RETURN_STRUCTURE(label, record)		\
  {						\
    Record *structure = Record::New(1);		\
    structure->Init(label, record->ToWord());	\
    return structure->ToWord();			\
  }

#endif
