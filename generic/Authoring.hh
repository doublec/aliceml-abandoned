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

#include "adt/IntMap.hh"
#include "adt/ChunkMap.hh"
#include "generic/Worker.hh"
#include "generic/Scheduler.hh"
#include "generic/Backtrace.hh"

// to be done: better solution without hacks
#define POP_PRIM_SELF() \
  StackFrame *prim_frame = Scheduler::GetFrame(); \
  Worker *prim_self = prim_frame->GetWorker(); \
  Scheduler::PopFrame(1); \
  prim_self = prim_self;

#define PUSH_PRIM_SELF() \
  NEW_STACK_FRAME(self_frame, prim_self, 0); \
  self_frame = self_frame;

#define DEFINE0(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 0);			\
    POP_PRIM_SELF();
#define DEFINE1(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == Scheduler::ONE_ARG);	\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::currentArgs[0];
#define DEFINE2(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 2);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];
#define DEFINE3(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 3);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];
#define DEFINE4(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 4);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];
#define DEFINE5(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 5);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];		\
    word x4 = Scheduler::currentArgs[4];
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
  PUSH_PRIM_SELF() \
  word prim_wFrame = self_frame->Clone(); \
  Scheduler::PopFrame(); \
  Scheduler::currentBacktrace = Backtrace::New(prim_wFrame); \
  return Worker::RAISE;						\
}

#define REQUEST(w) {				\
  Scheduler::currentData = w;			\
  PUSH_PRIM_SELF() \
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
#define DECLARE_INT_MAP(intmap, x) \
  DECLARE_BLOCKTYPE(IntMap, intmap, x)
#define DECLARE_CHUNK_MAP(chunkmap, x) \
  DECLARE_BLOCKTYPE(ChunkMap, chunkmap, x)
#define DECLARE_IODESC(ioDesc, x) DECLARE_BLOCKTYPE(IODesc, ioDesc, x)

#endif
