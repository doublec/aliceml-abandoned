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

#ifndef __GENERIC__NATIVE_AUTHORING_HH__
#define __GENERIC__NATIVE_AUTHORING_HH__

#include "adt/HashTable.hh"
#include "generic/Interpreter.hh"
#include "generic/Scheduler.hh"
#include "generic/Backtrace.hh"

#define DEFINE0(name)					\
  static Interpreter::Result name() {			\
    Assert(Scheduler::nArgs == 0);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;
#define DEFINE1(name)					\
  static Interpreter::Result name() {			\
    Assert(Scheduler::nArgs == 1);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
    word x0 = Scheduler::currentArgs[0];
#define DEFINE2(name)					\
  static Interpreter::Result name() {			\
    Assert(Scheduler::nArgs == 2);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];
#define DEFINE3(name)					\
  static Interpreter::Result name() {			\
    Assert(Scheduler::nArgs == 3);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];
#define END }

#define RETURN0 {				\
  Scheduler::nArgs = 0;				\
  return Interpreter::CONTINUE;			\
}
#define RETURN1(w) {				\
  Scheduler::nArgs = Scheduler::ONE_ARG;	\
  Scheduler::currentArgs[0] = w;		\
  return Interpreter::CONTINUE;			\
}
#define RETURN2(w1, w2) {			\
  Scheduler::nArgs = 2;				\
  Scheduler::currentArgs[0] = w1;		\
  Scheduler::currentArgs[1] = w2;		\
  return Interpreter::CONTINUE;			\
}
#define RETURN3(w1, w2, w3) {			\
  Scheduler::nArgs = 3;				\
  Scheduler::currentArgs[0] = w1;		\
  Scheduler::currentArgs[1] = w2;		\
  Scheduler::currentArgs[2] = w3;		\
  return Interpreter::CONTINUE;			\
}

#define RETURN(w) RETURN1(w)

#define RETURN_INT(i) RETURN(Store::IntToWord(i));

#define PREEMPT return Interpreter::PREEMPT;
#define SUSPEND return Interpreter::SUSPEND;

#define RAISE(w) {						\
  Scheduler::currentData = w;					\
  Scheduler::currentBacktrace = Backtrace::New(prim_self);	\
  return Interpreter::RAISE;					\
}

#define REQUEST(w) {				\
  Scheduler::currentData = w;			\
  Scheduler::PushFrameNoCheck(prim_self);	\
  return Interpreter::REQUEST;			\
}

#define TERMINATE return Interpreter::TERMINATE;

#define DECLARE_INT(i, x)						\
  int i = Store::WordToInt(x);						\
  if (i == INVALID_INT) { REQUEST(x); } else {}
#define DECLARE_BLOCKTYPE(t, a, x)					\
  t *a = t::FromWord(x);						\
  if (a == INVALID_POINTER) { REQUEST(x); } else {}

#define DECLARE_CLOSURE(closure, x) DECLARE_BLOCKTYPE(Closure, closure, x)
#define DECLARE_TUPLE(tuple, x) DECLARE_BLOCKTYPE(Tuple, tuple, x)
#define DECLARE_STRING(string, x) DECLARE_BLOCKTYPE(String, string, x)
#define DECLARE_UNIQUE_STRING(uniqueString, x) \
  DECLARE_BLOCKTYPE(UniqueString, uniqueString, x)
#define DECLARE_HASH_TABLE(hashtable, x) \
  DECLARE_BLOCKTYPE(HashTable, hashtable, x)

#endif
