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

class StackFrame;

class DllExport TaskStack: private DynamicBlock {
private:
  static const u_int INITIAL_SIZE = 20;
  static word emptyTask, emptyStack;
public:
  static const u_int initialNumberOfFrames = 1;

  using DynamicBlock::ToWord;
  using DynamicBlock::GetSize;
  using DynamicBlock::GetArg;
  using DynamicBlock::ReplaceArg;

  static void Init();

  u_int GetTop() {
    return GetActiveSize();
  }
  void SetTop(u_int top);

  StackFrame *GetFrame(u_int index) {
    return (StackFrame *) (GetBase() + (index + 1));
  }

  static TaskStack *New(u_int size);
  static TaskStack *New() {
    return New(INITIAL_SIZE);
  }
  static TaskStack *FromWordDirect(word x) {
    DynamicBlock *b = DynamicBlock::FromWordDirect(x);
    return static_cast<TaskStack *>(b);
  }

  TaskStack *Enlarge();
  void Purge();
  void Dump(u_int top);
};

#endif
