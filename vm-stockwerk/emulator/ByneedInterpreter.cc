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
#pragma implementation "emulator/ByneedInterpreter.hh"
#endif

#include <cstdio>
#include "emulator/ByneedInterpreter.hh"
#include "emulator/TaskStack.hh"
#include "emulator/Scheduler.hh"
#include "emulator/Transients.hh"

static inline int IsCyclic(word args, Transient *future) {
  return Store::WordToTransient(args) == future;
}

// ByneedFrame
class ByneedFrame : private StackFrame {
private:
  static const u_int FUTURE_POS = 0;
  static const u_int SIZE       = 1;
public:
  using Block::ToWord;
  using StackFrame::GetInterpreter;
  // ByneedFrame Accessors
  Transient *GetTransient() {
    return Store::WordToTransient(StackFrame::GetArg(FUTURE_POS));
  }
  // ByneedFrame Constructor
  static ByneedFrame *New(Interpreter *interpreter, Transient *future) {
    StackFrame *frame = StackFrame::New(BYNEED_FRAME, interpreter, SIZE);
    frame->ReplaceArg(FUTURE_POS, future->ToWord());
    return (ByneedFrame *) frame;
  }
  // ByneedFrame Untagging
  static ByneedFrame *FromWord(word frame) {
    Block *p = Store::WordToBlock(frame);
    Assert(p == INVALID_POINTER ||
	   p->GetLabel() == (BlockLabel) BYNEED_FRAME);
    return (ByneedFrame *) p;
  }
  static ByneedFrame *FromWordDirect(word frame) {
    Block *p = Store::DirectWordToBlock(frame);
    Assert(p == INVALID_POINTER ||
	   p->GetLabel() == (BlockLabel) BYNEED_FRAME);
    return (ByneedFrame *) p;
  }
};

//
// ByneedInterpreter Functions
//
ByneedInterpreter *ByneedInterpreter::self;

void ByneedInterpreter::PushFrame(TaskStack *taskStack, Transient *future) {
  taskStack->PushFrame(ByneedFrame::New(self, future)->ToWord());
}

Interpreter::Result ByneedInterpreter::Run(word args, TaskStack *taskStack) {
  Transient *future =
    ByneedFrame::FromWord(taskStack->GetFrame())->GetTransient();
  Assert(future != INVALID_POINTER);
  taskStack->PopFrame();
  args = Interpreter::Construct(args); // Argument Conversion
  // Prevent self binding
  if (IsCyclic(args, future)) {
    future->Become(CANCELLED_LABEL, Future::cyclicExn);
    Scheduler::currentData = Future::cyclicExn;
    return Interpreter::RAISE;
  }
  // Bind succeeded
  else {
    ((Future *) future)->ScheduleWaitingThreads();
    future->Become(REF_LABEL, args);
    Scheduler::currentData = future->ToWord();
    Scheduler::currentArgs = Interpreter::OneArg(Scheduler::currentData);
    return Interpreter::CONTINUE;
  }
}

Interpreter::Result ByneedInterpreter::Handle(word exn, word /*debug*/,
					      TaskStack *taskStack) {
  Transient *future =
    ByneedFrame::FromWord(taskStack->GetFrame())->GetTransient();
  Assert(future != INVALID_POINTER);
  taskStack->PopFrame();
  ((Future *) future)->ScheduleWaitingThreads();
  future->Become(CANCELLED_LABEL, exn);
  Scheduler::currentData = future->ToWord();
  Scheduler::currentArgs = Interpreter::OneArg(Scheduler::currentData);
  return Interpreter::CONTINUE;
}

const char *ByneedInterpreter::Identify() {
  return "ByneedInterpreter";
}

void ByneedInterpreter::DumpFrame(word) {
  fprintf(stderr, "Byneed\n");
}
