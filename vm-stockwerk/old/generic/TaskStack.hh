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

#ifndef __TASKSTACK_HH__
#define __TASKSTACK_HH__

#include "scheduler/Scheduler.hh"

class TaskStack: private Block {
public:
  using Block::ToWord;

  static TaskStack *New();

  // Handling stack frames:
  void PushFrame(u_int size); //--** size may be 0
  void PopFrame(u_int size);
  bool IsEmpty();
  void Clear();

  // Accessing the current frame:
  void PutWord(u_int offset, word v);
  word GetWord(u_int offset);
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

#endif
