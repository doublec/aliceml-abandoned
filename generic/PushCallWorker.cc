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
#pragma implementation "generic/PushCallInterpreter.hh"
#endif

#include <cstdio>
#include "generic/TaskStack.hh"
#include "generic/Scheduler.hh"
#include "generic/PushCallInterpreter.hh"

// PushCall Frame
class PushCallFrame : private StackFrame {
private:
  static const u_int CLOSURE_POS = 0;
  static const u_int SIZE        = 1;
public:
  using Block::ToWord;

  // PushCallFrame Constructor
  static PushCallFrame *New(Interpreter *interpreter, word closure) {
    StackFrame *frame = StackFrame::New(CALL_FRAME, interpreter, SIZE);
    frame->InitArg(CLOSURE_POS, closure);
    return static_cast<PushCallFrame *>(frame);
  }
  // PushCallFrame Untagging
  static PushCallFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == CALL_FRAME);
    return static_cast<PushCallFrame *>(p);
  }

  // PushCallFrame Accessors
  word GetClosure() {
    return StackFrame::GetArg(CLOSURE_POS);
  }
};

//
// PushCallInterpreter Functions
//
PushCallInterpreter *PushCallInterpreter::self;

void PushCallInterpreter::PushFrame(TaskStack *taskStack, word closure) {
  taskStack->PushFrame(PushCallFrame::New(self, closure)->ToWord());
}

Interpreter::Result PushCallInterpreter::Run(TaskStack *taskStack) {
  word closure =
    PushCallFrame::FromWordDirect(taskStack->GetFrame())->GetClosure();
  taskStack->PopFrame();
  return taskStack->PushCall(closure);
}

const char *PushCallInterpreter::Identify() {
  return "PushCallInterpreter";
}

void PushCallInterpreter::DumpFrame(word) {
  std::fprintf(stderr, "Push Call\n");
}
