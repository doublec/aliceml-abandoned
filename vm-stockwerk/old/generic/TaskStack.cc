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
#pragma implementation "generic/TaskStack.hh"
#endif

#include "generic/RootSet.hh"
#include "generic/TaskStack.hh"
#include "generic/TaskManager.hh"

class EmptyConcreteCodeHandler: public Handler {
public:
  virtual void PrepareForGC(Block *p);
  virtual void Finalize(word value);
  virtual Block *GetAbstractRepresentation();
};

void EmptyConcreteCodeHandler::PrepareForGC(Block *) {}

void EmptyConcreteCodeHandler::Finalize(word) {}

Block *EmptyConcreteCodeHandler::GetAbstractRepresentation() {
  return INVALID_POINTER;
}

class EmptyTaskManager: public TaskManager {
public:
  EmptyTaskManager();

  Closure *ToClosure();

  virtual void PushCall(TaskStack *taskStack, Closure *closure);
  virtual u_int PurgeFrame(TaskStack *taskStack, u_int offset);
  virtual Result Handle(TaskStack *taskStack);
  virtual Result Run(TaskStack *taskStack, int nargs);
};

EmptyTaskManager::EmptyTaskManager():
  TaskManager(new EmptyConcreteCodeHandler()) {}

Closure *EmptyTaskManager::ToClosure() {
  ConcreteCode *concreteCode = ConcreteCode::New(this, 0);
  return Closure::New(concreteCode, 0);
}

u_int EmptyTaskManager::PurgeFrame(TaskStack */*taskStack*/, u_int offset) {
  return 0; // meaning: this was the last one
}

TaskManager::Result EmptyTaskManager::Handle(TaskStack *) {
  //--** output information about the unhandled exception
  return Result(Result::TERMINATE);
}

TaskManager::Result EmptyTaskManager::Run(TaskStack *, int) {
  return Result(Result::TERMINATE);
}

word TaskStack::emptyTask;

void TaskStack::Init() {
  emptyTask = (new EmptyTaskManager())->ToClosure()->ToWord();
  RootSet::Add(emptyTask);
}
