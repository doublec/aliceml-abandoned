//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2004
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/Authoring.hh"

// TODO: This stuff belongs to the generic SEAM Layer

#if HAVE_SIGNAL
#include <signal.h>
#include <sys/time.h>
#endif

#if !HAVE_SIGNAL || HAVE_CONSOLECTRL
#include <windows.h>
#endif

enum { ALICE_SIGINT, ALICE_SIGSTOP };

static int AliceSignalToNativeSignal(u_int signal) {
  switch (signal) {
#if HAVE_CONSOLECTRL
  case ALICE_SIGINT:
    return CTRL_C_EVENT;
  case ALICE_SIGSTOP:
    return CTRL_BREAK_EVENT;
#endif
#if HAVE_SIGNAL
#if defined(SIGINT)
  case ALICE_SIGINT:
    return SIGINT;
#endif
#if defined(SIGSTOP)
  case ALICE_SIGSTOP:
    return SIGTSTP;
#endif
#endif
  default:
    Error("AliceSignalToNativeSignal: unknown signal");
  }
}

static int NativeSignalToAliceSignal(u_int signal) {
  switch (signal) {
#if HAVE_CONSOLECTRL
  case CTRL_C_EVENT:
    return ALICE_SIGINT;
  case CTRL_BREAK_EVENT:
    return ALICE_SIGSTOP;
#endif
#if HAVE_SIGNAL
#if defined(SIGINT)
  case SIGINT:
    return ALICE_SIGINT;
#endif
#if defined(SIGTSTP)
  case SIGTSTP:
    return ALICE_SIGSTOP;
#endif
#endif
  default:
    Error("NativeSignalToAliceSignal: unknown signal");
  }
}

class SignalTranslationFrame : public StackFrame {
protected:
  enum {
    CLOSURE_POS, RE_ENTER_POS, SIZE
  };
public:
  word GetClosure() {
    return StackFrame::GetArg(CLOSURE_POS);
  }
  u_int ReEntered() {
    return Store::DirectWordToInt(StackFrame::GetArg(RE_ENTER_POS));
  }
  void SetEntered() {
    StackFrame::ReplaceArg(RE_ENTER_POS, 1);
  }
  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  static SignalTranslationFrame *New(Interpreter *interpreter, word wClosure) {
    NEW_STACK_FRAME(frame, interpreter, SIZE);
    frame->InitArg(CLOSURE_POS, wClosure);
    frame->InitArg(RE_ENTER_POS, STATIC_CAST(s_int, 0));
    return STATIC_CAST(SignalTranslationFrame *, frame);
  }
};

class SignalTranslationInterpreter : public Interpreter {
public:
  static SignalTranslationInterpreter *self;
  SignalTranslationInterpreter() : Interpreter() {}
  static void Init();
  virtual void PushCall(Closure *closure);
  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Result Run(StackFrame *sFrame);
  virtual Result Handle(word data);
  virtual u_int GetInArity(ConcreteCode *concreteCode);
  virtual u_int GetOutArity(ConcreteCode *concreteCode);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

SignalTranslationInterpreter *SignalTranslationInterpreter::self;

void SignalTranslationInterpreter::Init() {
  self = new SignalTranslationInterpreter();
}

void SignalTranslationInterpreter::PushCall(Closure *closure) {
  SignalTranslationFrame::New(self, closure->Sub(0));
}

u_int SignalTranslationInterpreter::GetFrameSize(StackFrame *sFrame) {
  SignalTranslationFrame *frame = STATIC_CAST(SignalTranslationFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result SignalTranslationInterpreter::Run(StackFrame *sFrame) {
  SignalTranslationFrame *frame = STATIC_CAST(SignalTranslationFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  if (frame->ReEntered()) {
    Scheduler::PopFrame(frame->GetSize());
    return Worker::CONTINUE;
  } else {
    word wClosure = frame->GetClosure();
    frame->SetEntered();
    u_int nativeSignal = Store::DirectWordToInt(Scheduler::GetCurrentArg(0));
    Scheduler::SetCurrentArg(0,
      Store::IntToWord(NativeSignalToAliceSignal(nativeSignal)));
    return Scheduler::PushCall(wClosure);
  }
}

// Alice thread semantic: Silently ignore exceptions raised by the signal handler
Worker::Result SignalTranslationInterpreter::Handle(word) {
  StackFrame *sFrame = Scheduler::GetFrame();
  SignalTranslationFrame *frame =
    STATIC_CAST(SignalTranslationFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  Scheduler::PopFrame(frame->GetSize());
  Scheduler::SetNArgs(1);
  Scheduler::SetCurrentArg(0, Store::IntToWord(0));
  return Worker::CONTINUE;
}

u_int SignalTranslationInterpreter::GetInArity(ConcreteCode *) {
  return (u_int) INVALID_INT;
}

u_int SignalTranslationInterpreter::GetOutArity(ConcreteCode *) {
  return (u_int) INVALID_INT;
}

const char *SignalTranslationInterpreter::Identify() {
  return "SignalTranslationInterpreter";
}

void SignalTranslationInterpreter::DumpFrame(StackFrame *) {
  std::fprintf(stderr, "Alice Signal Translation\n");
}

DEFINE2(UnsafeSignal_register) {
  DECLARE_INT(signal, x0);
  word closure = x1;
  int nativeSignal = AliceSignalToNativeSignal(signal);
  ConcreteCode *concreteCode =
    ConcreteCode::New(SignalTranslationInterpreter::self, 0);
  Closure *translateClosure = Closure::New(concreteCode->ToWord(), 1);
  translateClosure->Init(0, closure);
  SignalHandler::RegisterSignal(nativeSignal, translateClosure->ToWord());
  RETURN_UNIT;
} END

AliceDll word UnsafeSignal() {
  SignalTranslationInterpreter::Init();
  Record *record = Record::New(1);
  INIT_STRUCTURE(record, "UnsafeSignal", "register",
		 UnsafeSignal_register, 2);
  RETURN_STRUCTURE("UnsafeSignal$", record);
}
