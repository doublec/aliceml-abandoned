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
#include "emulator/Scheduler.hh"
#include "emulator/ConcreteCode.hh"

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
    StackFrame *frame =
      StackFrame::New(LAZY_SELECTION_FRAME,interpreter, SIZE);
    frame->InitArg(BLOCK_POS, blk);
    frame->InitArg(INDEX_POS, Store::IntToWord(idx));
    return static_cast<LazySelectionFrame *>(frame);
  }
  // LazySelectionFrame Untagging
  static LazySelectionFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == LAZY_SELECTION_FRAME);
    return static_cast<LazySelectionFrame *>(p);
  }
};

//
// LazySelectionInterpreter
//
LazySelectionInterpreter *LazySelectionInterpreter::self;

void LazySelectionInterpreter::PushFrame(TaskStack *taskStack,
					 word tuple,
					 int index) {
  taskStack->PushFrame(LazySelectionFrame::New(self, tuple, index)->ToWord());
}

void LazySelectionInterpreter::PushCall(TaskStack *taskStack,
					Closure *closure) {
  PushFrame(taskStack, closure->Sub(0), Store::WordToInt(closure->Sub(1)));
}

Interpreter::Result
LazySelectionInterpreter::Run(TaskStack *taskStack) {
  LazySelectionFrame *frame =
    LazySelectionFrame::FromWordDirect(taskStack->GetFrame());
  word block = frame->GetBlock();
  Transient *transient = Store::WordToTransient(block);
  if (transient == INVALID_POINTER) { // is determined
    taskStack->PopFrame(); // Discard Frame
    Scheduler::nArgs = Scheduler::ONE_ARG;
    Scheduler::currentArgs[0] = Tuple::FromWord(block)->Sel(frame->GetIndex());
    return Interpreter::CONTINUE;
  } else { // need to request
    Scheduler::currentData = block;
    return Interpreter::REQUEST;
  }
}

const char *LazySelectionInterpreter::Identify() {
  return "LazySelectionInterpreter";
}

void LazySelectionInterpreter::DumpFrame(word frameWord) {
  LazySelectionFrame *frame = LazySelectionFrame::FromWordDirect(frameWord);
  fprintf(stderr, "Select #%d\n", frame->GetIndex());
}

//
// LazySelectionClosure
//
LazySelectionClosure *LazySelectionClosure::New(word tuple, int index) {
  ConcreteCode *concreteCode =
    ConcreteCode::New(LazySelectionInterpreter::self, 0);
  Closure *closure = Closure::New(concreteCode->ToWord(), 2);
  closure->Init(0, tuple);
  closure->Init(1, Store::IntToWord(index));
  return static_cast<LazySelectionClosure *>(closure);
}
