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

class DllExport TaskStack: private Block {
private:
  static const u_int INITIAL_SIZE = 16; // to be checked

  static word emptyTask;
public:
  static const u_int initialNumberOfFrames = 1;

  using Block::ToWord;
  using Block::GetSize;
  using Block::GetArg;
  using Block::ReplaceArg;

  static void Init();

  static TaskStack *New(u_int size) {
    Assert(size >= 2); // required for Enlarge to work correctly
    Block *b = Store::AllocBlock(TASKSTACK_LABEL, size);
    b->InitArg(0, emptyTask);
    return static_cast<TaskStack *>(b);
  }
  static TaskStack *New() {
    return New(INITIAL_SIZE);
  }
  static TaskStack *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == TASKSTACK_LABEL);
    return static_cast<TaskStack *>(b);
  }

  TaskStack *Enlarge();
  void Purge(u_int nFrames);
  void Dump(u_int nFrames);
};

#endif
