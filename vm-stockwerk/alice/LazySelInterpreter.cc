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
#include "generic/Scheduler.hh"
#include "alice/Data.hh"
#include "alice/StackFrame.hh"
#include "alice/LazySelInterpreter.hh"

// LazySel Frame
class LazySelFrame: private StackFrame {
private:
  enum { RECORD_POS, LABEL_POS, SIZE };
public:
  using Block::ToWord;

  static LazySelFrame *New(Interpreter *interpreter, word record,
			   UniqueString *label) {
    StackFrame *frame =
      StackFrame::New(LAZY_SELECTION_FRAME, interpreter, SIZE);
    frame->InitArg(RECORD_POS, record);
    frame->InitArg(LABEL_POS, label->ToWord());
    return static_cast<LazySelFrame *>(frame);
  }
  static LazySelFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == LAZY_SELECTION_FRAME);
    return static_cast<LazySelFrame *>(p);
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

void LazySelInterpreter::PushCall(Closure *closure0) {
  LazySelClosure *closure = static_cast<LazySelClosure *>(closure0);
  LazySelFrame *frame =
    LazySelFrame::New(self, closure->GetRecord(), closure->GetLabel());
  Scheduler::PushFrame(frame->ToWord());
}

Worker::Result LazySelInterpreter::Run() {
  LazySelFrame *frame = LazySelFrame::FromWordDirect(Scheduler::GetFrame());
  word wRecord = frame->GetRecord();
  Transient *transient = Store::WordToTransient(wRecord);
  if (transient == INVALID_POINTER) { // is determined
    Scheduler::PopFrame();
    Scheduler::nArgs = Scheduler::ONE_ARG;
    Scheduler::currentArgs[0] =
      Record::FromWord(wRecord)->PolySel(frame->GetLabel());
    return Worker::CONTINUE;
  } else { // need to request
    Scheduler::currentData = wRecord;
    return Worker::REQUEST;
  }
}

u_int LazySelInterpreter::GetInArity(ConcreteCode *) {
  return 0;
}

const char *LazySelInterpreter::Identify() {
  return "LazySelInterpreter";
}

void LazySelInterpreter::DumpFrame(word frameWord) {
  LazySelFrame *frame = LazySelFrame::FromWordDirect(frameWord);
  std::fprintf(stderr, "Select %s\n",
	       frame->GetLabel()->ToString()->ExportC());
}
