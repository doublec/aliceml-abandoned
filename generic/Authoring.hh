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

#ifndef __GENERIC__AUTHORING_HH__
#define __GENERIC__AUTHORING_HH__

#include "adt/HashTable.hh"
#include "generic/Worker.hh"
#include "generic/Scheduler.hh"
#include "generic/Backtrace.hh"

#define DEFINE0(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 0);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;
#define DEFINE1(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == Scheduler::ONE_ARG);	\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
    word x0 = Scheduler::currentArgs[0];
#define DEFINE2(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 2);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];
#define DEFINE3(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 3);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];
#define DEFINE4(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 4);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];
#define END }

#define RETURN0 {				\
  Scheduler::nArgs = 0;				\
  return Worker::CONTINUE;			\
}
#define RETURN1(w) {				\
  Scheduler::nArgs = Scheduler::ONE_ARG;	\
  Scheduler::currentArgs[0] = w;		\
  return Worker::CONTINUE;			\
}
#define RETURN2(w1, w2) {			\
  Scheduler::nArgs = 2;				\
  Scheduler::currentArgs[0] = w1;		\
  Scheduler::currentArgs[1] = w2;		\
  return Worker::CONTINUE;			\
}
#define RETURN3(w1, w2, w3) {			\
  Scheduler::nArgs = 3;				\
  Scheduler::currentArgs[0] = w1;		\
  Scheduler::currentArgs[1] = w2;		\
  Scheduler::currentArgs[2] = w3;		\
  return Worker::CONTINUE;			\
}

#define RETURN(w) RETURN1(w)

#define RETURN_INT(i) RETURN(Store::IntToWord(i));

#define PREEMPT0 {				\
  Scheduler::nArgs = 0;				\
  return Worker::PREEMPT;			\
}

#define SUSPEND return Worker::SUSPEND;

#define RAISE(w) {						\
  Scheduler::currentData = w;					\
  Scheduler::currentBacktrace = Backtrace::New(prim_self);	\
  return Worker::RAISE;						\
}

#define REQUEST(w) {				\
  Scheduler::currentData = w;			\
  Scheduler::PushFrameNoCheck(prim_self);	\
  return Worker::REQUEST;			\
}

#define EXIT(i) {				\
  Scheduler::currentData = Store::IntToWord(i);	\
  return Worker::EXIT;				\
}

#define DECLARE_INT(i, x)			\
  s_int i = Store::WordToInt(x);		\
  if (i == INVALID_INT) { REQUEST(x); } else {}
#define DECLARE_BLOCKTYPE(t, a, x)			\
  t *a = t::FromWord(x);				\
  if (a == INVALID_POINTER) { REQUEST(x); } else {}

#define DECLARE_CLOSURE(closure, x) DECLARE_BLOCKTYPE(Closure, closure, x)
#define DECLARE_TUPLE(tuple, x) DECLARE_BLOCKTYPE(Tuple, tuple, x)
#define DECLARE_STRING(string, x) DECLARE_BLOCKTYPE(String, string, x)
#define DECLARE_FLOAT(f, x) DECLARE_BLOCKTYPE(Float, f, x)
#define DECLARE_DOUBLE(d, x) DECLARE_BLOCKTYPE(Double, d, x)
#define DECLARE_UNIQUE_STRING(uniqueString, x) \
  DECLARE_BLOCKTYPE(UniqueString, uniqueString, x)
#define DECLARE_HASH_TABLE(hashtable, x) \
  DECLARE_BLOCKTYPE(HashTable, hashtable, x)
#define DECLARE_IODESC(ioDesc, x) DECLARE_BLOCKTYPE(IODesc, ioDesc, x)

#endif
