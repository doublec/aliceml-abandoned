//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE__AUTHORING_HH__
#define __ALICE__AUTHORING_HH__

#include "alice/Data.hh"
#include "alice/Types.hh"
#include "alice/PrimitiveTable.hh"
#include "alice/AliceLanguageLayer.hh"

#define BOOL_TO_WORD(b) Store::IntToWord((b)? Types::_true: Types::_false)

#define DECLARE_BOOL(b, x)				\
  bool b;						\
  {							\
    s_int i = Store::WordToInt(x);			\
    Assert(i == Types::_false || i == Types::_true);	\
    if (i == INVALID_INT) { REQUEST(x); } else b = i;	\
  }
#define DECLARE_ARRAY(array, x) DECLARE_BLOCKTYPE(Array, array, x)
#define DECLARE_CELL(cell, x) DECLARE_BLOCKTYPE(Cell, cell, x)
#define DECLARE_CONVAL(conVal, x) DECLARE_BLOCKTYPE(ConVal, conVal, x)
#define DECLARE_REAL(real, x) DECLARE_DOUBLE(real, x)
#define DECLARE_RECORD(record, x) DECLARE_BLOCKTYPE(Record, record, x)
#define DECLARE_TAGVAL(tagVal, x) DECLARE_BLOCKTYPE(TagVal, tagVal, x)
#define DECLARE_BIGTAGVAL(bigTagVal, x) \
  DECLARE_BLOCKTYPE(BigTagVal, bigTagVal, x)
#define DECLARE_THREAD(thread, x) DECLARE_BLOCKTYPE(Thread, thread, x)
#define DECLARE_VECTOR(vector, x) DECLARE_BLOCKTYPE(Vector, vector, x)
#define DECLARE_WORD8VECTOR(vector, x) \
  DECLARE_BLOCKTYPE(Word8Vector, vector, x)
#define DECLARE_WORD8ARRAY(array, x) DECLARE_BLOCKTYPE(Word8Array, array, x)

//--** does not work for infinite lists
#define DECLARE_LIST_ELEMS(tagVal, length, x, cmd)			\
  u_int length = 0;							\
  TagVal *tagVal;							\
  { word list = x;							\
    while ((tagVal = TagVal::FromWord(list)) != INVALID_POINTER) {	\
      Assert(tagVal->GetTag() == Types::cons);				\
      cmd;								\
      length++;								\
      list = tagVal->Sel(1);						\
    }									\
    if (Store::WordToInt(list) == INVALID_INT) { REQUEST(list); }	\
    Assert(Store::WordToInt(list) == Types::nil);			\
  }									\
  tagVal = TagVal::FromWord(x);

#define DECLARE_LIST(tagVal, length, x)					\
  DECLARE_LIST_ELEMS(tagVal, length, x, ;)

#define RETURN_UNIT RETURN0
#define RETURN_BOOL(b) RETURN(BOOL_TO_WORD(b));
#define RETURN_REAL(r) RETURN(Real::New(r)->ToWord());

#define INIT_STRUCTURE_N(r, s1, s2, f, i, j) {				      \
  Transform *abstract = INVALID_POINTER;				      \
  word function = Primitive::MakeFunction(s1 "." s2, f, i, j, abstract);      \
  r->Init(s2, Closure::New(function, 0)->ToWord());			      \
}

#define INIT_STRUCTURE(r, s1, s2, f, i) \
  INIT_STRUCTURE_N(r, s1, s2, f, i, 1)

#define RETURN_STRUCTURE(label, record)		\
  {						\
    Record *structure = Record::New(1);		\
    structure->Init(label, record->ToWord());	\
    return structure->ToWord();			\
  }

#endif
