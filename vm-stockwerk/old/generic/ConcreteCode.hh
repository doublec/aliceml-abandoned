//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000-2001
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
  // static const u_int HANDLER_POS = 0;
  static const u_int TASK_MANAGER_POS = 1;
  static const u_int BASE_SIZE = 2;
public:
  using Block::ToWord;

  static ConcreteCode *New(TaskManager *taskManager, u_int size) {
    Block *b =
      Store::AllocBlockWithHandler(BASE_SIZE + size, taskManager->handler);
    b->InitArg(TASK_MANAGER_POS, Store::UnmanagedPointerToWord(taskManager));
    return static_cast<ConcreteCode *>(b);
  }
  static ConcreteCode *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == HANDLERBLOCK_LABEL);
    return static_cast<ConcreteCode *>(b);
  }
  static ConcreteCode *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == HANDLERBLOCK_LABEL);
    return static_cast<ConcreteCode *>(b);
  }

  word GetAbstractCode() {
    return GetHandler()->GetAbstractRepresentation()->ToWord();
  }
  TaskManager *GetTaskManager() {
    return static_cast<TaskManager *>
      (Store::DirectWordToUnmanagedPointer(GetArg(TASK_MANAGER_POS)));
  }
  void Init(u_int index, word value) {
    InitArg(BASE_SIZE + index, value);
  }
  word Get(u_int index) {
    return GetArg(BASE_SIZE + index);
  }
};

#endif __GENERIC__CONCRETE_CODE_HH__
