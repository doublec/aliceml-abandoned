//
// Authors:
//   Jens Regenberg <jens@ps.uni-sb.de>
//
// Copyright:
//   Jens Regenberg, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if DEBUGGER
#if defined(INTERFACE)
#pragma implementation "generic/DebugWorker.hh"
#endif

#include <cstdio>
#include <ostream>
#include "generic/Backtrace.hh"
#include "generic/Scheduler.hh"
#include "generic/RootSet.hh"
#include "generic/DebugWorker.hh"
#include "generic/DebugFrame.hh"
#include "generic/StackFrame.hh"

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

void DumpFrame(StackFrame *sFrame, std::ostream& out) {
  out << "[Debugger]" << std::endl;
}

word DebugWorker::GetEvent(StackFrame *sFrame) {
  DebugFrame *debugFrame = static_cast<DebugFrame *>(sFrame);
  Assert(debugFrame->GetWorker() == self);
  return debugFrame->GetEvent();
}
#endif
