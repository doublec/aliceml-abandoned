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

#ifndef __GENERIC__CONCRETE_CODE_HH__
#define __GENERIC__CONCRETE_CODE_HH__

#if defined(INTERFACE)
#pragma interface "generic/ConcreteCode.hh"
#endif

#include "generic/TaskManager.hh"

class ConcreteCode: private Block {
private:
  static const u_int SIZE = 2;
  static const u_int TASK_MANAGER_POS = 2;
public:
  using Block::ToWord;

  static ConcreteCode *New(TaskManager *taskManager, u_int size) {
    Block *b = Store::AllocBlockWithHandler(SIZE + size, taskManager->handler);
    b->InitArg(TASK_MANAGER_POS, Store::UnmanagedPointerToWord(taskManager));
    return static_cast<ConcreteCode *>(b);
  }
  static ConcreteCode *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == CONCRETECODE_LABEL);
    return static_cast<ConcreteCode *>(b);
  }
  static ConcreteCode *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == CONCRETECODE_LABEL);
    return static_cast<ConcreteCode *>(b);
  }

  TaskManager *GetTaskManager() {
    return static_cast<TaskManager *>
      (Store::DirectWordToUnmanagedPointer(GetArg(TASK_MANAGER_POS)));
  }
  void Init(u_int index, word value) {
    InitArg(SIZE + index + 1, value);
  }
  word Get(u_int index) {
    return GetArg(SIZE + index + 1);
  }
};

#endif __GENERIC__CONCRETE_CODE_HH__
