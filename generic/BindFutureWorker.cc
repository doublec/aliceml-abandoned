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
#include "emulator/Backtrace.hh"
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
    return static_cast<ByneedFrame *>(frame);
  }
  // ByneedFrame Untagging
  static ByneedFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == BYNEED_FRAME);
    return static_cast<ByneedFrame *>(p);
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
  ByneedFrame *frame = ByneedFrame::FromWordDirect(taskStack->GetFrame());
  Transient *future  = frame->GetTransient();
  Assert(future != INVALID_POINTER);
  taskStack->PopFrame();
  args = Interpreter::Construct(args); // Argument Conversion
  // Prevent self binding
  if (IsCyclic(args, future)) {
    future->Become(CANCELLED_LABEL, Hole::cyclicExn);
    Scheduler::currentData = Hole::cyclicExn;
    Scheduler::currentBacktrace = Backtrace::New(frame->ToWord());
    return Interpreter::RAISE;
  }
  // Bind succeeded
  else {
    static_cast<Future *>(future)->ScheduleWaitingThreads();
    future->Become(REF_LABEL, args);
    Scheduler::currentArgs = Interpreter::OneArg(future->ToWord());
    return Interpreter::CONTINUE;
  }
}

Interpreter::Result ByneedInterpreter::Handle(word exn, Backtrace *,
					      TaskStack *taskStack) {
  Transient *future =
    ByneedFrame::FromWordDirect(taskStack->GetFrame())->GetTransient();
  taskStack->PopFrame();
  static_cast<Future *>(future)->ScheduleWaitingThreads();
  Assert(future->GetLabel() == FUTURE_LABEL);
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
