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

#include "Scheduler.hh"
#include "Interpreter.hh"

#define TASK_STACK_INITIAL_SIZE 8

class TaskStack: private Block {
public:
  using Block::ToWord;

  static TaskStack *New(u_int initialSize);

  void PushFrame(Interpreter *interpreter, u_int size);
  void PopFrame();

  bool IsEmpty();

  // Exception handling:
  void PushMark();
  void PopToMark();

  Interpreter *GetInterpreter();

  void PutWord(u_int offset, word v);
  word GetWord(u_int offset);
  void PutInt(u_int offset, int i) {
    PutWord(offset, Store::IntToWord(i));
  }
  int GetInt(u_int offset) {
    return Store::WordToInt(GetWord(offset));
  }
  void PutPointer(u_int offset, void *pointer) {
    PutWord(offset, Store::UnmanagedPointerToWord(pointer));
  }
  void *GetPointer(u_int offset) {
    return Store::WordToUnmanagedPointer(GetWord(offset));
  }
};

#endif
