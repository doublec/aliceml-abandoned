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

#if defined(INTERFACE)
#pragma implementation "generic/pickling/Transformer.hh"
#endif

#include "generic/TaskStack.hh"
#include "generic/pickling/Transformer.hh"

Transformer::Transformer(transformer f): TaskManager(handler), function(f) {}

void Transformer::PushCall(TaskStack *taskStack, Closure *closure) {
  Assert(closure->GetConcreteCode()->GetTaskManager() == this);
  taskStack->PushFrame(2);
  taskStack->PutWord(1, closure->GetConcreteCode()->GetAbstractCode());
  taskStack->PutUnmanagedPointer(0, this);
}

u_int Transformer::PurgeFrame(TaskStack *, u_int offset) {
  return offset + 2;
}

TaskManager::Result Transformer::Handle(TaskStack *taskStack) {
  word exn = taskStack->GetWord(0);
  taskStack->PopFrame(2);
  taskStack->PutWord(0, exn);
  return Result(Result::RAISE);
}

TaskManager::Result Transformer::Run(TaskStack *taskStack, int nargs) {
  Assert(nargs == 0 || nargs == -1);
  word abstractRepresentation;
  if (nargs == -1) {
    abstractRepresentation = taskStack->GetWord(2);
    taskStack->PopFrame(3);
  } else {
    abstractRepresentation = taskStack->GetWord(1);
    taskStack->PopFrame(2);
  }
  return function(taskStack, abstractRepresentation);
}
