//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "alice/LazySelInterpreter.hh"
#endif

#include <cstdio>
#include "alice/Data.hh"
#include "alice/LazySelInterpreter.hh"

// LazySel Frame
class LazySelFrame: private StackFrame {
private:
  enum { RECORD_POS, LABEL_POS, SIZE };
public:
  static LazySelFrame *New(Interpreter *interpreter, word record,
			   UniqueString *label) {
    NEW_STACK_FRAME(frame, interpreter, SIZE);
    frame->InitArg(RECORD_POS, record);
    frame->InitArg(LABEL_POS, label->ToWord());
    return static_cast<LazySelFrame *>(frame);
  }

  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  word GetRecord() {
    return GetArg(RECORD_POS);
  }
  UniqueString *GetLabel() {
    return UniqueString::FromWordDirect(GetArg(LABEL_POS));
  }
};

//
// LazySelInterpreter
//
LazySelInterpreter *LazySelInterpreter::self;

void LazySelInterpreter::Init() {
  self = new LazySelInterpreter();
}

void LazySelInterpreter::PushCall(Closure *closure0) {
  LazySelClosure *closure = reinterpret_cast<LazySelClosure *>(closure0);
  LazySelFrame::New(self, closure->GetRecord(), closure->GetLabel());
}

u_int LazySelInterpreter::GetFrameSize(StackFrame *sFrame) {
  LazySelFrame *frame = reinterpret_cast<LazySelFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result LazySelInterpreter::Run(StackFrame *sFrame) {
  LazySelFrame *frame = reinterpret_cast<LazySelFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  word wRecord = frame->GetRecord();
  Transient *transient = Store::WordToTransient(wRecord);
  if (transient == INVALID_POINTER) { // is determined
    UniqueString *label = frame->GetLabel();
    Scheduler::PopFrame(frame->GetSize());
    Scheduler::SetNArgs(1);
    Scheduler::SetCurrentArg(0, Record::FromWord(wRecord)->PolySel(label));
    return Worker::CONTINUE;
  } else { // need to request
    Scheduler::SetCurrentData(wRecord);
    return Worker::REQUEST;
  }
}

u_int LazySelInterpreter::GetInArity(ConcreteCode *) {
  return 0;
}

u_int LazySelInterpreter::GetOutArity(ConcreteCode *) {
  return 1;
}

const char *LazySelInterpreter::Identify() {
  return "LazySelInterpreter";
}

void LazySelInterpreter::DumpFrame(StackFrame *sFrame, std::ostream& out) {
  LazySelFrame *frame = reinterpret_cast<LazySelFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  out << "[LazySel] " << frame->GetLabel()->ToString() << std::endl;
}
