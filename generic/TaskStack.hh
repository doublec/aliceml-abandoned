//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__TASK_STACK_HH__
#define __GENERIC__TASK_STACK_HH__

#if defined(INTERFACE)
#pragma interface "generic/TaskStack.hh"
#endif

#include "adt/Stack.hh"
#include "generic/Interpreter.hh"
#include "generic/StackFrame.hh"

class TaskStack: private Stack {
private:
  static const u_int INITIAL_SIZE = 16; // to be checked
public:
  using Stack::ToWord;
  using Stack::IsEmpty;

  // TaskStack public static data
  static word emptyTask;
  // TaskStack Static Constructor
  static void Init();

  // TaskStack Constructor
  static TaskStack *New() {
    TaskStack *taskStack = static_cast<TaskStack *>(Stack::New(INITIAL_SIZE));
    taskStack->SlowPush(emptyTask);
    return taskStack;
  }
  // TaskStack Untagging
  static TaskStack *FromWord(word x) {
    return static_cast<TaskStack *>(Stack::FromWord(x));
  }
  static TaskStack *FromWordDirect(word x) {
    return static_cast<TaskStack *>(Stack::FromWordDirect(x));
  }

  // TaskStack Functions
  void PushFrame(word frame) {
    Stack::SlowPush(frame);
  }
  void PopFrame() {
    Stack::Pop();
  }
  word GetFrame() {
    return Stack::Top();
  }
  Interpreter *GetInterpreter() {
    return StackFrame::FromWordDirect(GetFrame())->GetInterpreter();
  }
  //   PushCall requires that Scheduler::nArgs and Scheduler::currentArgs
  //   have already been set:
  Interpreter::Result PushCall(word closure);
  void Purge();
  void Dump();
};

#endif
