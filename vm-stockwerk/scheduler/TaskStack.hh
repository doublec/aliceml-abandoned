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

#ifndef __SCHEDULER__TASKSTACK_HH__
#define __SCHEDULER__TASKSTACK_HH__

#pragma interface "scheduler/TaskStack.hh"

#include "scheduler/Closure.hh"

#define TASK_STACK_INITIAL_SIZE 8 /* words */

#define TASK_STACK_LABEL Store::MakeLabel(0) //--**

//--** we'll want to implement this in terms of class Stack

class TaskStack: private Block {
public:
  using Block::ToWord;

  static TaskStack *New() {
    Block *b = Store::AllocBlock(TASK_STACK_LABEL, TASK_STACK_INITIAL_SIZE);
    b->InitArg(1, Store::IntToWord(0));
    return static_cast<TaskStack *>(b);
  }
  static TaskStack *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == TASK_STACK_LABEL);
    return static_cast<TaskStack *>(b);
  }

  // Handling stack frames:
  void PushFrame(u_int size); //--** implement; size may be 0
  void PopFrame(u_int size); //--** implement
  bool IsEmpty(); //--** implement
  void Clear(); //--** implement
  void PushCall(Closure *closure) {
    closure->GetConcreteCode()->GetInterpreter()->PushCall(this, closure);
  }

  // Accessing the current frame:
  void PutWord(u_int offset, word v); //---** implement
  word GetWord(u_int offset); //--** implement
  void PutInt(u_int offset, int i) {
    PutWord(offset, Store::IntToWord(i));
  }
  int GetInt(u_int offset) {
    return Store::WordToInt(GetWord(offset));
  }
  void PutUnmanagedPointer(u_int offset, void *pointer) {
    PutWord(offset, Store::UnmanagedPointerToWord(pointer));
  }
  void *GetUnmanagedPointer(u_int offset) {
    return Store::WordToUnmanagedPointer(GetWord(offset));
  }
};

#endif __SCHEDULER__TASKSTACK_HH__
