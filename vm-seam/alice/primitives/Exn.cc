//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Andreas Rossberg <rossberg@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus and Andreas Rossberg, 2004
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/Authoring.hh"

//
// CatchWorker
//
class CatchWorkerFrame : public StackFrame {
protected:
  enum { HANDLER_POS, SIZE };
public:
  static CatchWorkerFrame *New(Worker *worker, word wClosure) {
    NEW_STACK_FRAME(frame, worker, SIZE);
    frame->InitArg(HANDLER_POS, wClosure);
    return STATIC_CAST(CatchWorkerFrame *, frame);
  }
  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  word GetHandler() {
    return StackFrame::GetArg(HANDLER_POS);
  }
};

class CatchWorker : public Worker {
private:
  CatchWorker() : Worker() {}
public:
  static CatchWorker *self;

  static void Init() {
    self = new CatchWorker();
  }

  static void PushFrame(word closure) {
    CatchWorkerFrame::New(self, closure);
  }

  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Result Run(StackFrame *sFrame);
  virtual Result Handle(word data);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

CatchWorker *CatchWorker::self;

u_int CatchWorker::GetFrameSize(StackFrame *sFrame) {
  Assert(sFrame->GetWorker() == this);
  CatchWorkerFrame *catchWorkerFrame = STATIC_CAST(CatchWorkerFrame *, sFrame);
  return catchWorkerFrame->GetSize();
}

Worker::Result CatchWorker::Run(StackFrame *sFrame) {
  Assert(sFrame->GetWorker() == this);
  CatchWorkerFrame *catchWorkerFrame = STATIC_CAST(CatchWorkerFrame *, sFrame);
  Scheduler::PopHandler();
  Scheduler::PopFrame(catchWorkerFrame->GetSize());
  return Worker::CONTINUE;
}

Worker::Result CatchWorker::Handle(word) {
  StackFrame *sFrame = Scheduler::GetFrame();
  Assert(sFrame->GetWorker() == this);
  CatchWorkerFrame *catchWorkerFrame = STATIC_CAST(CatchWorkerFrame *, sFrame);
  word handler = catchWorkerFrame->GetHandler();
  Scheduler::PopFrame(catchWorkerFrame->GetSize());
  Scheduler::SetNArgs(2);
  Scheduler::SetCurrentArg(0, Scheduler::GetCurrentData());
  Scheduler::SetCurrentArg(1, Scheduler::GetCurrentBacktrace()->ToWord());
  return Scheduler::PushCall(handler);
}

const char *CatchWorker::Identify() {
  return "CatchWorker";
}

void CatchWorker::DumpFrame(StackFrame *) {
  // TODO: Insert useful stuff
}

DEFINE1(Exn_name) {
  DECLARE_BLOCK(conVal, x0);
  String *name;
 retry:
  switch (conVal->GetLabel()) {
  case UNIQUESTRING_LABEL:
    name = UniqueString::FromWordDirect(conVal->ToWord())->ToString();
    break;
  case Alice::ConVal:
    conVal = ConVal::FromWordDirect(conVal->ToWord())->GetConstructor();
    goto retry;
  default:
    name = Constructor::FromWordDirect(conVal->ToWord())->GetName();
    break;
  }
  //--** drop prefix
  RETURN(name->ToWord());
} END

DEFINE2(Exn_catch) {
  word handler = x0;
  word closure = x1;
  CatchWorker::PushFrame(handler);
  word data = Store::IntToWord(0); // Unused
  Scheduler::PushHandler(data);
  Scheduler::SetNArgs(1);
  Scheduler::SetCurrentArg(0, Store::IntToWord(0)); // Unit
  return Scheduler::PushCall(closure);
} END

DEFINE2(Exn_reraise) {
  word exn = x0;
  word backtrace = x1;
  Scheduler::SetNArgs(0);
  Scheduler::SetCurrentData(exn);
  Scheduler::SetCurrentBacktrace(Backtrace::FromWord(backtrace));
  return Worker::RAISE;
} END

DEFINE2(Exn_dumpTrace) {
  DECLARE_BLOCKTYPE(Backtrace, backtrace, x1);
  backtrace->Dump();
  RETURN_UNIT;
} END

void PrimitiveTable::RegisterExn() {
  CatchWorker::Init();
  Register("Exn.catch", Exn_catch, 2);
  Register("Exn.dumpTrace", Exn_dumpTrace, 2);
  Register("Exn.name", Exn_name, 1);
  Register("Exn.reraise", Exn_reraise, 2);
}
