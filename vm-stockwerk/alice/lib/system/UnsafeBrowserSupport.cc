//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "generic/Transients.hh"
#include "generic/Interpreter.hh"
#include "generic/Closure.hh"
#include "generic/ConcreteCode.hh"
#include "generic/Scheduler.hh"
#include "alice/Authoring.hh"
#include "alice/StackFrame.hh"

class RequestFrame : public StackFrame {
protected:
  enum { QUEUE_POS, BYNEED_POS, AGAIN_POS, SIZE };
public:
  using Block::ToWord;

  Queue *GetQueue() {
    return Queue::FromWord(StackFrame::GetArg(QUEUE_POS));
  }
  word GetByneed() {
    return StackFrame::GetArg(BYNEED_POS);
  }
  u_int GetAgain() {
    return Store::DirectWordToInt(StackFrame::GetArg(AGAIN_POS));
  }
  void SetAgain() {
    StackFrame::ReplaceArg(AGAIN_POS, 1);
  }
  static RequestFrame *New(Interpreter *interpreter, word queue, word byneed) {
    StackFrame *frame = StackFrame::New(REQUEST_FRAME, interpreter, SIZE);
    frame->InitArg(QUEUE_POS, queue);
    frame->InitArg(BYNEED_POS, byneed);
    frame->InitArg(AGAIN_POS, 0);
    return static_cast<RequestFrame *>(frame);
  }
  static RequestFrame *FromWordDirect(word wFrame) {
    StackFrame *frame = StackFrame::FromWordDirect(wFrame);
    Assert(frame->GetLabel() == REQUEST_FRAME);
    return static_cast<RequestFrame *>(frame);
  }
};

class RequestInterpreter : public Interpreter {
public:
  static RequestInterpreter *self;

  RequestInterpreter() : Interpreter() {}

  static void Init() {
    self = new RequestInterpreter();
  }

  virtual Result Run();
  virtual u_int GetInArity(ConcreteCode *concreteCode);
  virtual const char *Identify();
  virtual void DumpFrame(word wFrame);

  virtual void PushCall(Closure *closure);
};

RequestInterpreter *RequestInterpreter::self;

Worker::Result RequestInterpreter::Run() {
  RequestFrame *frame = RequestFrame::FromWordDirect(Scheduler::GetFrame());
  if (frame->GetAgain()) {
    Scheduler::PopFrame();
    Queue *queue = frame->GetQueue();
    while (!queue->IsEmpty()) {
      Thread *thread = Thread::FromWordDirect(queue->Dequeue());
      Scheduler::ResumeThread(thread);
    }
    RETURN0;
  }
  else {
    frame->SetAgain();
    Scheduler::currentData = frame->GetByneed();
    return Worker::REQUEST;
  }
}

u_int RequestInterpreter::GetInArity(ConcreteCode *) {
  return 0;
}

const char *RequestInterpreter::Identify() {
  return "RequestInterpreter";
}

void RequestInterpreter::DumpFrame(word) {
  // to be done: insert useful stuff
  return;
}

void RequestInterpreter::PushCall(Closure *closure) {
  RequestFrame *frame = RequestFrame::New(RequestInterpreter::self,
					  closure->Sub(0), closure->Sub(1));
  Scheduler::PushFrame(frame->ToWord());
}

DEFINE1(UnsafeBrowserSupport_waitRequest) {
  Transient *transient = Store::WordToTransient(x0);
  if (transient == INVALID_POINTER) {
    RETURN(x0);
  }
  if (transient->GetLabel() == BYNEED_LABEL) {
    Closure *closure           = static_cast<Byneed *>(transient)->GetClosure();
    word wConcreteCode         = closure->GetConcreteCode();
    ConcreteCode *concreteCode = ConcreteCode::FromWord(wConcreteCode);
    Queue *queue;
    if ((concreteCode != INVALID_POINTER) &&
	(concreteCode->GetInterpreter() == RequestInterpreter::self)) {
      queue = Queue::FromWordDirect(closure->Sub(0));
      Scheduler::currentArgs[0] = closure->Sub(1);
    }
    else {
      Byneed *copy            = Byneed::New(closure->ToWord());
      ConcreteCode *concCode  = ConcreteCode::New(RequestInterpreter::self, 0);
      Closure *requestClosure = Closure::New(concCode->ToWord(), 2);
      queue = Queue::New(2);
      requestClosure->Init(0, queue->ToWord());
      requestClosure->Init(1, copy->ToWord());
      transient->ReplaceArg(requestClosure->ToWord());
      Scheduler::currentArgs[0] = copy->ToWord();
    }
    queue->Enqueue(Scheduler::GetCurrentThread()->ToWord());
    Scheduler::nArgs = Scheduler::ONE_ARG;
    SUSPEND;
  }
  REQUEST(x0);
} END

word UnsafeBrowserSupport() {
  Record *record = Record::New(1);
  RequestInterpreter::Init();
  INIT_STRUCTURE(record, "UnsafeBrowserSupport", "waitRequest",
		 UnsafeBrowserSupport_waitRequest, 1, true);
  RETURN_STRUCTURE("UnsafeBrowserSupport$", record);
}
