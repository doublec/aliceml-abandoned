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
#include "generic/StackFrame.hh"
#include "generic/Transients.hh"
#include "generic/Scheduler.hh"
#include "alice/LazySelInterpreter.hh"

// LazySel Frame
class LazySelFrame: private StackFrame {
private:
  enum { RECORD_POS, LABELS_POS, BYNEEDS_POS, SIZE };
public:
  using Block::ToWord;
  using StackFrame::GetInterpreter;
  // LazySelFrame Accessors
  word GetRecord() {
    return GetArg(RECORD_POS);
  }
  Vector *GetLabels() {
    return Vector::FromWordDirect(GetArg(LABELS_POS));
  }
  Vector *GetByneeds() {
    return Vector::FromWordDirect(GetArg(BYNEEDS_POS));
  }
  // LazySelFrame Constructor
  static LazySelFrame *New(Interpreter *interpreter, word record,
			   Vector *labels, Vector *byneeds) {
    StackFrame *frame =
      StackFrame::New(LAZY_SELECTION_FRAME, interpreter, SIZE);
    frame->InitArg(RECORD_POS, record);
    frame->InitArg(LABELS_POS, labels->ToWord());
    frame->InitArg(BYNEEDS_POS, byneeds->ToWord());
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

void LazySelInterpreter::PushCall(Closure *closure0) {
  LazySelClosure *closure = static_cast<LazySelClosure *>(closure0);
  LazySelFrame *frame =
    LazySelFrame::New(self, closure->GetRecord(),
		      closure->GetLabels(), closure->GetByneeds());
  Scheduler::PushFrame(frame->ToWord());
}

Interpreter::Result LazySelInterpreter::Run() {
  LazySelFrame *frame = LazySelFrame::FromWordDirect(Scheduler::GetFrame());
  word wRecord = frame->GetRecord();
  Transient *transient = Store::WordToTransient(wRecord);
  if (transient == INVALID_POINTER) { // is determined
    Record *record = Record::FromWord(wRecord);
    Vector *labels = frame->GetLabels();
    Vector *byneeds = frame->GetByneeds();
    Assert(labels->GetLength() == byneeds->GetLength());
    word result = 0;
    for (u_int i = labels->GetLength(); i--; ) {
      Transient *transient = Store::DirectWordToTransient(byneeds->Sub(i));
      UniqueString *label = UniqueString::FromWordDirect(labels->Sub(i));
      word value = record->PolySel(label);
      if (transient->GetLabel() == FUTURE_LABEL) {
	Assert(result == 0);
	result = value;
      } else {
	Assert(transient->GetLabel() == BYNEED_LABEL);
	transient->Become(REF_LABEL, value);
      }
    }
    Assert(result != 0);
    Scheduler::PopFrame();
    Scheduler::nArgs = Scheduler::ONE_ARG;
    Scheduler::currentArgs[0] = result;
    return Interpreter::CONTINUE;
  } else { // need to request
    Scheduler::currentData = wRecord;
    return Interpreter::REQUEST;
  }
}

const char *LazySelInterpreter::Identify() {
  return "LazySelInterpreter";
}

void LazySelInterpreter::DumpFrame(word frameWord) {
  LazySelFrame *frame = LazySelFrame::FromWordDirect(frameWord);
  std::fprintf(stderr, "Select");
  Vector *labels = frame->GetLabels();
  for (u_int i = labels->GetLength(); i--; ) {
    UniqueString *label = UniqueString::FromWordDirect(labels->Sub(i));
    std::fprintf(stderr, " %s", label->ToString()->ExportC());
  }
  std::fprintf(stderr, "\n");
}
