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

#ifndef __BUILTINS__AUTHORING_HH__
#define __BUILTINS__AUTHORING_HH__

#include "scheduler/TaskStack.hh"
#include "builtins/Primitive.hh"
#include "builtins/GlobalPrimitives.hh"

#define DEFINE0(name)							\
  static Interpreter::Result name(TaskStack *taskStack) {		\
    taskStack->PopFrame(1);
#define DEFINE1(name)							\
  static Interpreter::Result name(TaskStack *taskStack) {		\
    word x0 = taskStack->GetWord(0);					\
    taskStack->PopFrame(2);
#define DEFINE2(name)							\
  static Interpreter::Result name(TaskStack *taskStack) {		\
    word x0 = taskStack->GetWord(0);					\
    word x1 = taskStack->GetWord(1);					\
    taskStack->PopFrame(3);
#define DEFINE3(name)							\
  static Interpreter::Result name(TaskStack *taskStack) {		\
    word x0 = taskStack->GetWord(0);					\
    word x1 = taskStack->GetWord(1);					\
    word x2 = taskStack->GetWord(2);					\
    taskStack->PopFrame(4);
#define END }

#define DECLARE_INT(i, x)						\
  int i = Store::WordToInt(x);						\
  if (i == INVALID_INT) { REQUEST(x); } else {}

#define DECLARE_BLOCKTYPE(t, a, x)					\
  t *a = t::FromWord(x);						\
  if (a == INVALID_POINTER) { REQUEST(x); } else {}
#define DECLARE_ARRAY(array, x) DECLARE_BLOCKTYPE(Array, array, x)
#define DECLARE_CELL(cell, x) DECLARE_BLOCKTYPE(Cell, cell, x)
#define DECLARE_CLOSURE(closure, x) DECLARE_BLOCKTYPE(Closure, closure, x)
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

#define RAISE(w) {							\
  taskStack->PushFrame(1);						\
  taskStack->PutWord(0, w);						\
  return Interpreter::Result(Interpreter::Result::RAISE);		\
}

#define RETURN(w) {							\
  taskStack->PushFrame(1);						\
  taskStack->PutWord(0, w);						\
  return Interpreter::Result(Interpreter::Result::CONTINUE, -1);	\
}
#define RETURN_UNIT \
  return Interpreter::Result(Interpreter::Result::CONTINUE, 0);
#define RETURN_INT(i) RETURN(Store::IntToWord(i));
#define RETURN_BOOL(b) RETURN_INT(b);

#define REQUEST(w) {							\
  taskStack->PushFrame(1);						\
  taskStack->PutWord(0, w);						\
  return Interpreter::Result(Interpreter::Result::REQUEST, 1);		\
}

#endif __BUILTINS__AUTHORING_HH__
