//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstdio>
#include "emulator/Closure.hh"
#include "emulator/Thread.hh"
#include "emulator/Scheduler.hh"
#include "emulator/Authoring.hh"
#include "emulator/StackFrame.hh"
#include "emulator/Transients.hh"

class RaiseFrame: private StackFrame {
private:
  static const u_int EXN_POS = 0;
  static const u_int SIZE    = 1;
public:
  using StackFrame::ToWord;
  // RaiseFrame Accessors
  word GetExn() {
    return GetArg(EXN_POS);
  }
  // RaiseFrame Constructor
  static RaiseFrame *New(Interpreter *interpreter, word exn) {
    StackFrame *frame =
      StackFrame::New(RAISE_FRAME, interpreter, SIZE);
    frame->InitArg(EXN_POS, exn);
    return static_cast<RaiseFrame *>(frame);
  }
  // VectorTabulateFrame Untagging
  static RaiseFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == RAISE_FRAME);
    return static_cast<RaiseFrame *>(p);
  }
};

class RaiseInterpreter: public Interpreter {
private:
  static RaiseInterpreter *self;
public:
  // RaiseInterpreter Constructor
  RaiseInterpreter(): Interpreter() {}
  // RaiseInterpreter Static Constructor
  static void Init() {
    self = new RaiseInterpreter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack, word exn) {
    taskStack->PushFrame(RaiseFrame::New(self, exn)->ToWord());
  }
  // Execution
  virtual Result Run(TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// RaiseInterpreter Functions
//
RaiseInterpreter *RaiseInterpreter::self;

Interpreter::Result RaiseInterpreter::Run(TaskStack *taskStack) {
  RaiseFrame *frame = RaiseFrame::FromWordDirect(taskStack->GetFrame());
  Scheduler::currentData = frame->GetExn();
  return Interpreter::RAISE;
}

const char *RaiseInterpreter::Identify() {
  return "RaiseInterpreter";
}

void RaiseInterpreter::DumpFrame(word) {
  fprintf(stderr, "Raise\n");
}

// Builtins

DEFINE0(Thread_current) {
  RETURN(Scheduler::GetCurrentThread()->ToWord());
} END

DEFINE1(Thread_isSuspended) {
  DECLARE_THREAD(thread, x0);
  RETURN_BOOL(thread->IsSuspended());
} END

DEFINE2(Thread_raiseIn) {
  DECLARE_THREAD(thread, x0);
  if (thread == Scheduler::GetCurrentThread()) {
    RAISE(x1);
  } else {
    Thread::state state = thread->GetState();
    if (state == Thread::TERMINATED) {
      RAISE(PrimitiveTable::Thread_Terminated);
    } else {
      RaiseInterpreter::PushFrame(thread->GetTaskStack(), x1);
      thread->SetArgs(0, Store::IntToWord(0));
      if (state == Thread::BLOCKED) {
	Future *future = static_cast<Future *>
	  (Store::WordToTransient(thread->GetFuture()));
	Assert(future != INVALID_POINTER);
	future->RemoveFromWaitQueue(thread);
	Scheduler::WakeupThread(thread);
      }
      RETURN_UNIT;
    }
  }
} END

DEFINE1(Thread_resume) {
  DECLARE_THREAD(thread, x0);
  Scheduler::ResumeThread(thread);
  RETURN_UNIT;
} END

DEFINE1(Thread_state) {
  DECLARE_THREAD(thread, x0);
  switch (thread->GetState()) {
  case Thread::BLOCKED:
    RETURN_INT(0);
  case Thread::RUNNABLE:
    RETURN_INT(1);
  case Thread::TERMINATED:
    RETURN_INT(2);
  }
  Error("Thread_state: unknown state returned by Thread::GetState");
} END

DEFINE1(Thread_suspend) {
  DECLARE_THREAD(thread, x0);
  Scheduler::SuspendThread(thread);
  if (thread == Scheduler::GetCurrentThread()) {
    PREEMPT;
  } else {
    RETURN_UNIT;
  }
} END

DEFINE0(Thread_yield) {
  PREEMPT;
} END

void PrimitiveTable::RegisterThread() {
  RaiseInterpreter::Init();
  PrimitiveTable::Thread_Terminated =
    UniqueConstructor::New(String::New("Thread.Terminated"))->ToWord();
  RegisterUniqueConstructor("Thread.Terminate");
  Register("Thread.Terminated", PrimitiveTable::Thread_Terminated);
  Register("Thread.current", Thread_current, 0);
  Register("Thread.isSuspended", Thread_isSuspended, 1);
  Register("Thread.raiseIn", Thread_raiseIn, 2);
  Register("Thread.resume", Thread_resume, 1);
  Register("Thread.state", Thread_state, 1);
  Register("Thread.suspend", Thread_suspend, 1);
  Register("Thread.yield", Thread_yield, 0);
}
