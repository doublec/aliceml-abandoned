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

#if defined(INTERFACE)
#pragma interface "scheduler/TaskStack.hh"
#endif

#include "adt/Stack.hh"
#include "scheduler/Closure.hh"
#include "scheduler/Interpreter.hh"

class TaskStack: private Stack {
private:
  static const u_int threshold = 8; // words
public:
  using Stack::ToWord;
  using Stack::IsEmpty;

  static TaskStack *New() {
    //--** push suicide interpreter on the stack
    return static_cast<TaskStack *>(Stack::New(threshold));
  }
  static TaskStack *FromWord(word x) {
    return static_cast<TaskStack *>(Stack::FromWord(x));
  }

  // Handling stack frames:
  void PushFrame(u_int size) {
    AllocArgFrame(size);
  }
  void PopFrame(u_int size) {
    ClearFrame(size);
  }
  void PushCall(Closure *closure) {
    closure->GetConcreteCode()->GetInterpreter()->PushCall(this, closure);
  }
  void Clear() {
    ClearFrame(GetStackSize());
    Blank(0);
  }
  void Purge() {
    Blank(threshold);
    //--** should call Interpreter::PurgeFrame on each frame
  }

  // Accessing the current frame:
  void PutWord(u_int offset, word value) {
    PutFrameArg(offset, value);
  }
  word GetWord(u_int offset) {
    return GetFrameArg(offset);
  }
  void PutInt(u_int offset, int i) {
    PutFrameArg(offset, i);
  }
  int GetInt(u_int offset) {
    return Store::DirectWordToInt(GetWord(offset));
  }
  void PutUnmanagedPointer(u_int offset, void *pointer) {
    PutWord(offset, Store::UnmanagedPointerToWord(pointer));
  }
  void *GetUnmanagedPointer(u_int offset) {
    return Store::DirectWordToUnmanagedPointer(GetWord(offset));
  }
};

#endif __SCHEDULER__TASKSTACK_HH__
