//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "emulator/LazySelectionInterpreter.hh"
#endif

#include <cstdio>
#include "emulator/LazySelectionInterpreter.hh"
#include "emulator/TaskStack.hh"
#include "emulator/Tuple.hh"
#include "emulator/Scheduler.hh"

// LazySelection Frame
class LazySelectionFrame : private StackFrame {
private:
  static const u_int BLOCK_POS = 0;
  static const u_int INDEX_POS = 1;
  static const u_int SIZE      = 2;
public:
  using Block::ToWord;
  using StackFrame::GetInterpreter;
  // LazySelectionFrame Accessors
  word GetBlock() {
    return StackFrame::GetArg(BLOCK_POS);
  }
  int GetIndex() {
    return Store::WordToInt(StackFrame::GetArg(INDEX_POS));
  }
  // LazySelectionFrame Constructor
  static LazySelectionFrame *New(Interpreter *interpreter, word blk, int idx) {
    StackFrame *frame = StackFrame::New(LAZY_SELECTION_FRAME,interpreter, SIZE);
    frame->ReplaceArg(BLOCK_POS, blk);
    frame->ReplaceArg(INDEX_POS, Store::IntToWord(idx));
    return (LazySelectionFrame *) frame;
  }
  // LazySelectionFrame Untagging
  static LazySelectionFrame *FromWord(word frame) {
    Block *p = Store::WordToBlock(frame);
    Assert(p == INVALID_POINTER ||
	   p->GetLabel() == (BlockLabel) LAZY_SELECTION_FRAME);
    return (LazySelectionFrame *) p;
  }
  static LazySelectionFrame *FromWordDirect(word frame) {
    Block *p = Store::DirectWordToBlock(frame);
    Assert(p == INVALID_POINTER ||
	   p->GetLabel() == (BlockLabel) LAZY_SELECTION_FRAME);
    return (LazySelectionFrame *) p;
  }
};

//
// LazySelectionInterpreter Functions
//
LazySelectionInterpreter *LazySelectionInterpreter::self;

void LazySelectionInterpreter::PushFrame(TaskStack *taskStack,
					 word tuple,
					 int index) {
  taskStack->PushFrame(LazySelectionFrame::New(self, tuple, index)->ToWord());
}

Interpreter::Result
LazySelectionInterpreter::Run(word args, TaskStack *taskStack) {
  LazySelectionFrame *frame =
    LazySelectionFrame::FromWord(taskStack->GetFrame());
  Assert(frame != INVALID_POINTER);
  word block = frame->GetBlock();
  Transient *transient = Store::WordToTransient(block);
  // Found Block
  if (transient == INVALID_POINTER) {
    taskStack->PopFrame(); // Discard Frame
    args = Tuple::FromWord(block)->Sel(frame->GetIndex());
    Scheduler::currentArgs = Interpreter::OneArg(args);
    return Interpreter::CONTINUE;
  }
  // Need to wait for Block
  else {
    Scheduler::currentData = block;
    Scheduler::currentArgs = args;
    return Interpreter::REQUEST;
  }
}

const char *LazySelectionInterpreter::Identify() {
  return "LazySelectionInterpreter";
}

const char *
LazySelectionInterpreter::ToString(word args, TaskStack *taskStack) {
  static char info[256];
  LazySelectionFrame *frame =
    LazySelectionFrame::FromWord(taskStack->GetFrame());
  Assert(frame != INVALID_POINTER);
  sprintf(info, "LazySelection . %d", frame->GetIndex());
  return info;
}
