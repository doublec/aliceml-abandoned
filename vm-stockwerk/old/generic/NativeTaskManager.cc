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
#pragma implementation "generic/NativeTaskManager.hh"
#endif

#include "generic/TaskStack.hh"
#include "generic/NativeTaskManager.hh"
#include "generic/InternalTasks.hh"
#include "generic/Tuple.hh"

class NativeConcreteCodeHandler: public Handler {
public:
  virtual void PrepareForGC(Block *p);
  virtual void Finalize(word value);
  virtual Block *GetAbstractRepresentation();
};

void NativeConcreteCodeHandler::PrepareForGC(Block *) {}

void NativeConcreteCodeHandler::Finalize(word) {}

Block *NativeConcreteCodeHandler::GetAbstractRepresentation() {
  return INVALID_POINTER;
}

NativeTaskManager::NativeTaskManager(NativeTaskManager::function f,
				     int nargs, u_int nslots):
  TaskManager(new NativeConcreteCodeHandler()),
  func(f), arity(nargs == 1? -1: nargs), frameSize(nslots + 1) {}

Closure *NativeTaskManager::ToClosure() {
  ConcreteCode *concreteCode = ConcreteCode::New(this, 0);
  return Closure::New(concreteCode, 0);
}

void NativeTaskManager::PushCall(TaskStack *taskStack, Closure *closure) {
  Assert(closure->GetConcreteCode()->GetTaskManager() == this);
  taskStack->PushFrame(1);
  taskStack->PutUnmanagedPointer(0, this);
}

void NativeTaskManager::PopFrame(TaskStack *taskStack) {
  taskStack->PopFrame(frameSize);
}

TaskManager::Result NativeTaskManager::Run(TaskStack *taskStack, int nargs) {
  if (arity != nargs) {
    if (nargs == -1) { // i.e., arity >= 0
      word suspendWord = taskStack->GetWord(0);
      if (arity == 0) { // await unit
	if (Store::WordToInt(suspendWord) == INVALID_INT) {
	  taskStack->PopFrame(1);
	  taskStack->PushCall(Closure::FromWordDirect(InternalTasks::await));
	  taskStack->PushFrame(1);
	  taskStack->PutWord(0, suspendWord);
	  return Result(Result::CONTINUE, 1);
	}
	Assert(Store::WordToInt(suspendWord) == 0); // unit
      } else { // await and deconstruct tuple (here arity > 0 holds)
	Tuple *tuple = Tuple::FromWord(suspendWord);
	if (tuple == INVALID_POINTER) {
	  taskStack->PopFrame(1);
	  taskStack->PushCall(Closure::FromWordDirect(InternalTasks::await));
	  taskStack->PushFrame(1);
	  taskStack->PutWord(0, suspendWord);
	  return Result(Result::CONTINUE, 1);
	}
	taskStack->PushFrame(arity - 1);
	Assert(tuple->GetWidth() == static_cast<u_int>(arity));
	for (u_int i = arity; i--; )
	  taskStack->PutWord(i, tuple->Sel(i));
      }
    } else if (nargs == 0) { // unconditionally return unit
      taskStack->PushFrame(1);
      taskStack->PutWord(0, 0);
    } else { // unconditionally construct tuple
      Tuple *tuple = Tuple::New(nargs);
      for (u_int i = nargs; i--; )
	tuple->Init(i, taskStack->GetWord(i));
      taskStack->PopFrame(nargs - 1);
      taskStack->PutWord(0, tuple->ToWord());
    }
  }
  return func(taskStack);
}
