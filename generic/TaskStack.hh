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

#ifndef __EMULATOR__TASKSTACK_HH__
#define __EMULATOR__TASKSTACK_HH__

#if defined(INTERFACE)
#pragma interface "emulator/TaskStack.hh"
#endif

#include "adt/Stack.hh"
#include "emulator/Closure.hh"
#include "emulator/Interpreter.hh"
#include "emulator/StackFrame.hh"

class TaskStack : private Stack {
private:
  static const u_int INITIAL_SIZE = 16;
public:
  using Stack::ToWord;
  using Stack::IsEmpty;
  // TaskStack Functions
  static u_int maxWidth, maxDepth;
  static void Dump(word x);
  void DumpTaskStack();
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
    return StackFrame::FromWord(GetFrame())->GetInterpreter();
  }
  Interpreter::Result PushCall(word closure);
  void Clear() {
    ClearFrame(GetStackSize());
    Blank(0);
  }
  void Purge() {
    return; // to be done
  }
  // TaskStack public static data
  static word emptyTask;
  // TaskStack Static Constructor
  static void Init();
  // TaskStack Constructor
  static TaskStack *New() {
    TaskStack *taskStack = (TaskStack *) Stack::New(INITIAL_SIZE);
    taskStack->PushFrame(emptyTask);
    return taskStack;
  }
  // TaskStack Untagging
  static TaskStack *FromWord(word x) {
    return (TaskStack *) Stack::FromWord(x);
  }
  static TaskStack *FromWordDirect(word x) {
    return (TaskStack *) Stack::FromWordDirect(x);
  }
};

#endif
