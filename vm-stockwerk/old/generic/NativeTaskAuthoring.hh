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

#include "generic/TaskStack.hh"
#include "generic/NativeTaskManager.hh"

#define DEFINE0(name)							\
  static TaskManager::Result name(TaskStack *taskStack) {
#define DEFINE1(name)							\
  static TaskManager::Result name(TaskStack *taskStack) {		\
    word x0 = taskStack->GetWord(0);					\
    taskStack->PopFrame(1);
#define DEFINE2(name)							\
  static TaskManager::Result name(TaskStack *taskStack) {		\
    word x0 = taskStack->GetWord(0);					\
    word x1 = taskStack->GetWord(1);					\
    taskStack->PopFrame(2);
#define DEFINE3(name)							\
  static TaskManager::Result name(TaskStack *taskStack) {		\
    word x0 = taskStack->GetWord(0);					\
    word x1 = taskStack->GetWord(1);					\
    word x2 = taskStack->GetWord(2);					\
    taskStack->PopFrame(3);
#define END }

#define RETURN(w) {							\
  taskStack->PutWord(0, w);						\
  return TaskManager::Result(TaskManager::Result::CONTINUE, -1);	\
}
#define RETURN_UNIT {							\
  taskStack->PopFrame(1);						\
  return TaskManager::Result(TaskManager::Result::CONTINUE, 0);		\
}
#define RETURN_INT(i) {							\
  taskStack->PutInt(0, i);						\
  return TaskManager::Result(TaskManager::Result::CONTINUE, -1);	\
}
#define RETURN_BOOL(b) RETURN_INT(b);

#define PREEMPT {							\
  taskStack->PopFrame(1);						\
  return TaskManager::Result(TaskManager::Result::PREEMPT, 0);		\
}

#define RAISE(w) {							\
  taskStack->PutWord(0, w);						\
  return TaskManager::Result(TaskManager::Result::RAISE);		\
}

#define REQUEST(w) {							\
  taskStack->PushFrame(1);						\
  taskStack->PutWord(0, w);						\
  return TaskManager::Result(TaskManager::Result::REQUEST, 1);		\
}
#define REQUEST2(w1, w2) {						\
  taskStack->PushFrame(2);						\
  taskStack->PutWord(0, w1);						\
  taskStack->PutWord(1, w2);						\
  return TaskManager::Result(TaskManager::Result::REQUEST, 2);		\
}

#define TERMINATE							\
  return TaskManager::Result(TaskManager::Result::TERMINATE);

#define DECLARE_INT(i, x)						\
  int i = Store::WordToInt(x);						\
  if (i == INVALID_INT) { REQUEST(x); } else {}
#define DECLARE_BLOCKTYPE(t, a, x)					\
  t *a = t::FromWord(x);						\
  if (a == INVALID_POINTER) { REQUEST(x); } else {}
#define DECLARE_CLOSURE(closure, x) DECLARE_BLOCKTYPE(Closure, closure, x)

#endif __GENERIC__NATIVE_TASK_AUTHORING_HH__
