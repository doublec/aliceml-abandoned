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
  static const u_int RECORD_POS = 0;
  static const u_int LABEL_POS  = 1;
  static const u_int SIZE       = 2;
public:
  using Block::ToWord;
  using StackFrame::GetInterpreter;
  // LazySelFrame Accessors
  word GetRecord() {
    return StackFrame::GetArg(RECORD_POS);
  }
  UniqueString *GetLabel() {
    return UniqueString::FromWordDirect(StackFrame::GetArg(LABEL_POS));
  }
  // LazySelFrame Constructor
  static LazySelFrame *New(Interpreter *interpreter,
			   word record, UniqueString *label) {
    StackFrame *frame =
      StackFrame::New(LAZY_SELECTION_FRAME,interpreter, SIZE);
    frame->InitArg(RECORD_POS, record);
    frame->InitArg(LABEL_POS, label->ToWord());
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
LazySelInterpreter::PushFrame(TaskStack *taskStack,
			      word record, UniqueString *label) {
  taskStack->PushFrame(LazySelFrame::New(self, record, label)->ToWord());
}

void LazySelInterpreter::PushCall(TaskStack *taskStack, Closure *closure) {
  PushFrame(taskStack, closure->Sub(0),
	    UniqueString::FromWordDirect(closure->Sub(1)));
}

Interpreter::Result LazySelInterpreter::Run(TaskStack *taskStack) {
  LazySelFrame *frame = LazySelFrame::FromWordDirect(taskStack->GetFrame());
  word record = frame->GetRecord();
  Transient *transient = Store::WordToTransient(record);
  if (transient == INVALID_POINTER) { // is determined
    taskStack->PopFrame(); // Discard Frame
    Scheduler::nArgs = Scheduler::ONE_ARG;
    Scheduler::currentArgs[0] =
      Record::FromWord(record)->PolySel(frame->GetLabel());
    return Interpreter::CONTINUE;
  } else { // need to request
    Scheduler::currentData = record;
    return Interpreter::REQUEST;
  }
}

const char *LazySelInterpreter::Identify() {
  return "LazySelInterpreter";
}

void LazySelInterpreter::DumpFrame(word frameWord) {
  LazySelFrame *frame = LazySelFrame::FromWordDirect(frameWord);
  fprintf(stderr, "Select %s\n", frame->GetLabel()->ToString()->ExportC());
}

//
// LazySelClosure
//
LazySelClosure *LazySelClosure::New(word record, UniqueString *label) {
  ConcreteCode *concreteCode = ConcreteCode::New(LazySelInterpreter::self, 0);
  Closure *closure = Closure::New(concreteCode->ToWord(), 2);
  closure->Init(0, record);
  closure->Init(1, label->ToWord());
  return static_cast<LazySelClosure *>(closure);
}
