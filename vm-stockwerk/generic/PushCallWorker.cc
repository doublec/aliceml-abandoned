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
#pragma implementation "emulator/PushCallInterpreter.hh"
#endif

#include <cstdio>
#include "emulator/PushCallInterpreter.hh"
#include "emulator/TaskStack.hh"
#include "emulator/Scheduler.hh"

// PushCall Frame
class PushCallFrame : private StackFrame {
private:
  static const u_int CLOSURE_POS = 0;
  static const u_int SIZE        = 1;
public:
  using Block::ToWord;
  using StackFrame::GetInterpreter;
  // PushCallFrame Accessors
  word GetClosure() {
    return StackFrame::GetArg(CLOSURE_POS);
  }
  // PushCallFrame Constructor
  static PushCallFrame *New(Interpreter *interpreter, word closure) {
    StackFrame *frame = StackFrame::New(CALL_FRAME, interpreter, SIZE);
    frame->ReplaceArg(CLOSURE_POS, closure);
    return static_cast<PushCallFrame *>(frame);
  }
  // PushCallFrame Untagging
  static PushCallFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == CALL_FRAME);
    return static_cast<PushCallFrame *>(p);
  }
};

//
// PushCallInterpreter Functions
//

PushCallInterpreter *PushCallInterpreter::self;

void PushCallInterpreter::PushFrame(TaskStack *taskStack, word closure) {
  taskStack->PushFrame(PushCallFrame::New(self, closure)->ToWord());
}

Interpreter::Result PushCallInterpreter::Run(word args, TaskStack *taskStack) {
  word closure =
    PushCallFrame::FromWordDirect(taskStack->GetFrame())->GetClosure();
  taskStack->PopFrame();
  Scheduler::currentArgs = args;
  return taskStack->PushCall(closure);
}

const char *PushCallInterpreter::Identify() {
  return "PushCallInterpreter";
}

void PushCallInterpreter::DumpFrame(word) {
  fprintf(stderr, "Push Call\n");
}
