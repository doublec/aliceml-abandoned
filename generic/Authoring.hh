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

#ifndef __GENERIC__NATIVE_TASK_AUTHORING_HH__
#define __GENERIC__NATIVE_TASK_AUTHORING_HH__

#include "adt/HashTable.hh"
#include "emulator/TaskStack.hh"
#include "emulator/Interpreter.hh"
#include "emulator/Scheduler.hh"

#define DEFINE0(name)						\
  static Interpreter::Result name(word, TaskStack *taskStack) {	\
    word prim_self = taskStack->GetFrame();			\
    prim_self = prim_self;					\
    taskStack->PopFrame();
#define DEFINE1(name)							\
  static Interpreter::Result name(word args, TaskStack *taskStack) {	\
    word prim_self = taskStack->GetFrame();				\
    prim_self = prim_self;						\
    taskStack->PopFrame();						\
    word x0 = args;
#define DEFINE2(name)							\
  static Interpreter::Result name(word args, TaskStack *taskStack) {	\
    Block *pargs = Store::DirectWordToBlock(args);			\
    word prim_self = taskStack->GetFrame();				\
    prim_self = prim_self;						\
    taskStack->PopFrame();						\
    word x0 = pargs->GetArg(0);						\
    word x1 = pargs->GetArg(1);
#define DEFINE3(name)							\
  static Interpreter::Result name(word args, TaskStack *taskStack) {	\
    Block *pargs = Store::DirectWordToBlock(args);			\
    word prim_self = taskStack->GetFrame();				\
    prim_self = prim_self;						\
    taskStack->PopFrame();						\
    word x0 = pargs->GetArg(0);						\
    word x1 = pargs->GetArg(1);						\
    word x2 = pargs->GetArg(2);

#define END }

#define RETURN(w) {							\
  Scheduler::currentArgs = Interpreter::OneArg(w);                      \
  return Interpreter::CONTINUE;	                                        \
}
#define RETURN_UNIT {							\
  Scheduler::currentArgs = Interpreter::EmptyArg();                     \
  return Interpreter::CONTINUE;		                                \
}
#define RETURN_INT(i) {							\
  Scheduler::currentArgs = Interpreter::OneArg(Store::IntToWord(i));    \
  return Interpreter::CONTINUE;	                                        \
}
#define RETURN_BOOL(b) RETURN_INT(b);

#define PREEMPT {							\
  return Interpreter::PREEMPT;		                                \
}

#define RAISE(w) {							\
  Scheduler::currentData = w;                                           \
  taskStack->PushFrame(prim_self);                                      \
  return Interpreter::RAISE;		                                \
}

#define REQUEST(w) {							\
  Scheduler::currentData = w;                                           \
  taskStack->PushFrame(prim_self);                                      \
  return Interpreter::REQUEST;		                                \
}

#define TERMINATE							\
  return Interpreter::TERMINATE;

#define DECLARE_INT(i, x)						\
  int i = Store::WordToInt(x);						\
  if (i == INVALID_INT) { REQUEST(x); } else {}
#define DECLARE_BLOCKTYPE(t, a, x)					\
  t *a = t::FromWord(x);						\
  if (a == INVALID_POINTER) { REQUEST(x); } else {}

#define DECLARE_CLOSURE(closure, x) DECLARE_BLOCKTYPE(Closure, closure, x)
#define DECLARE_TUPLE(tuple, x) DECLARE_BLOCKTYPE(Tuple, tuple, x)

#define DECLARE_HASH_TABLE(hashtable, x) \
  DECLARE_BLOCKTYPE(HashTable, hashtable, x)

#endif
