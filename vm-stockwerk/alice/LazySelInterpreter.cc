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
#pragma implementation "alice/LazySelInterpreter.hh"
#endif

#include <cstdio>
#include "generic/TaskStack.hh"
#include "generic/Scheduler.hh"
#include "generic/ConcreteCode.hh"
#include "alice/LazySelInterpreter.hh"

// LazySel Frame
class LazySelFrame : private StackFrame {
private:
  static const u_int BLOCK_POS = 0;
  static const u_int INDEX_POS = 1;
  static const u_int SIZE      = 2;
public:
  using Block::ToWord;
  using StackFrame::GetInterpreter;
  // LazySelFrame Accessors
  word GetBlock() {
    return StackFrame::GetArg(BLOCK_POS);
  }
  int GetIndex() {
    return Store::WordToInt(StackFrame::GetArg(INDEX_POS));
  }
  // LazySelFrame Constructor
  static LazySelFrame *New(Interpreter *interpreter, word blk, int idx) {
    StackFrame *frame =
      StackFrame::New(LAZY_SELECTION_FRAME,interpreter, SIZE);
    frame->InitArg(BLOCK_POS, blk);
    frame->InitArg(INDEX_POS, Store::IntToWord(idx));
    return static_cast<LazySelFrame *>(frame);
  }
  // LazySelFrame Untagging
  static LazySelFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == LAZY_SELECTION_FRAME);
    return static_cast<LazySelFrame *>(p);
  }
};

//
// LazySelInterpreter
//
LazySelInterpreter *LazySelInterpreter::self;

void
LazySelInterpreter::PushFrame(TaskStack *taskStack, word tuple, int index) {
  taskStack->PushFrame(LazySelFrame::New(self, tuple, index)->ToWord());
}

void LazySelInterpreter::PushCall(TaskStack *taskStack, Closure *closure) {
  PushFrame(taskStack, closure->Sub(0), Store::WordToInt(closure->Sub(1)));
}

Interpreter::Result LazySelInterpreter::Run(TaskStack *taskStack) {
  LazySelFrame *frame = LazySelFrame::FromWordDirect(taskStack->GetFrame());
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

const char *LazySelInterpreter::Identify() {
  return "LazySelInterpreter";
}

void LazySelInterpreter::DumpFrame(word frameWord) {
  LazySelFrame *frame = LazySelFrame::FromWordDirect(frameWord);
  fprintf(stderr, "Select #%d\n", frame->GetIndex());
}

//
// LazySelClosure
//
LazySelClosure *LazySelClosure::New(word tuple, int index) {
  ConcreteCode *concreteCode = ConcreteCode::New(LazySelInterpreter::self, 0);
  Closure *closure = Closure::New(concreteCode->ToWord(), 2);
  closure->Init(0, tuple);
  closure->Init(1, Store::IntToWord(index));
  return static_cast<LazySelClosure *>(closure);
}
