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

#ifndef __GENERIC__NATIVE_TASK_MANAGER_HH__
#define __GENERIC__NATIVE_TASK_MANAGER_HH__

#if defined(INTERFACE)
#pragma interface "generic/NativeTaskManager.hh"
#endif

#include "generic/TaskManager.hh"

class NativeTaskManager: public TaskManager {
public:
  typedef Result (*function)(TaskStack *);
private:
  function func;
  int arity;
  u_int frameSize;
public:
  NativeTaskManager(function f, int nargs, u_int nslots):
    func(f), arity(nargs == 1? -1: nargs), frameSize(nslots + 1) {}

  Closure *ToClosure();

  virtual void PushCall(TaskStack *taskStack, Closure *closure);
  virtual void PopFrame(TaskStack *taskStack);
  virtual Result Run(TaskStack *taskStack, int nargs);
};

#endif __GENERIC__NATIVE_TASK_MANAGER_HH__
