//
// Authors:
//   Jens Regenberg <jens@ps.uni-sb.de>
//
// Copyright:
//   Jens Regenberg, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if DEBUGGER
#if defined(INTERFACE)
#pragma implementation "generic/DebugWorker.cc"
#endif

#include <cstdio>
#include "generic/Backtrace.hh"
#include "generic/Scheduler.hh"
#include "generic/RootSet.hh"
#include "generic/DebugWorker.hh"

// DebugFrame
class DebugFrame: private StackFrame {
private:
  enum { EVENT_POS, SIZE };
public:
  // DebugFrame Constructor
  static DebugFrame *New(Worker *worker, word event) {
    NEW_STACK_FRAME(frame, worker, SIZE);
    frame->InitArg(EVENT_POS, event);
    return static_cast<DebugFrame *>(frame);
  }

  // DebugFrame Accessors
  word GetEvent() {
    return StackFrame::GetArg(EVENT_POS);
  }
  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
};


//
// Debug Interpreter Functions
//

DebugWorker *DebugWorker::self;

void DebugWorker::PushFrame(word event) {
  DebugFrame::New(self, event);
}

u_int DebugWorker::GetFrameSize(StackFrame *sFrame) {
  DebugFrame *debugFrame = static_cast<DebugFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return debugFrame->GetSize();
}

Worker::Result DebugWorker::Run(StackFrame *) {
  Error("DebugWorker::Run: tried to run debug frame");
}

Worker::Result DebugWorker::Handle(word) {
  return Worker::RAISE;
}

const char *DebugWorker::Identify() {
  return "DebugWorker";
}

void DebugWorker::DumpFrame(StackFrame *) {
  std::fprintf(stderr, "Debuggger\n");
}

word DebugWorker::GetEvent(StackFrame *sFrame) {
  DebugFrame *debugFrame = static_cast<DebugFrame *>(sFrame);
  Assert(debugFrame->GetWorker() == this);
  return debugFrame->GetEvent();
}
#endif
