//
// Author:
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
#pragma implementation "generic/Debugger.hh"
#endif

#include "generic/Scheduler.hh"
#include "generic/Thread.hh"
#include "generic/RootSet.hh"
#include "generic/Debug.hh"
#include "generic/Stream.hh"
#include "generic/Debugger.hh"
#include "generic/StackFrame.hh"
#include "generic/DebugWorker.hh"

// Helper

typedef enum { HD_POS, TL_POS, SIZE} ListLabel;

void Debugger::GenerateMissingEvents() {
  TaskStack *taskStack = Scheduler::currentTaskStack;
  word *base = (word *) taskStack->GetFrame(0);
  word *top = base + Scheduler::GetCurrentStackTop() - 1;
  while (top >= base) {
    StackFrame *frame = (StackFrame *) top;
    Worker *worker = frame->GetWorker();
    top -= worker->GetFrameSize(frame);
    if (worker == DebugWorker::self) {
      word event = DebugWorker::GetEvent(frame);
      Debugger::SendEvent(event);
    }
  }
}

static bool Exists(word wThread, word list) {
  while (list != Store::IntToWord(0)) {
    Block *b = Store::WordToBlock(list);
    if (b == INVALID_POINTER) {
      Error("Invalid Thread List");
    }
    if (b->GetArg(HD_POS) == wThread)
      return true;
    list = b->GetArg(TL_POS);
  }
  return false;
}

#define SetBreakpoint(thread) {                         \
  if (Exists(thread->ToWord(), breakPointList)) {           \
    return;                                             \
  }                                                     \
  Block *b = Store::AllocBlock((BlockLabel) 1, SIZE);   \
  b->InitArg(HD_POS, thread->ToWord());                 \
  b->InitArg(TL_POS, breakPointList);                       \
                                                        \
  breakPointList = b->ToWord();                             \
}

//
// Debugger Functions
//

word Debugger::eventStream;
word Debugger::breakPointList;

void Debugger::Init() {
  eventStream = Stream::New()->ToWord();
  breakPointList = Store::IntToWord(0);
  RootSet::Add(eventStream);
  RootSet::Add(breakPointList);
}

bool Debugger::IsBreakpoint(Thread *thread) {
  bool bp = false;

  if (breakPointList == Store::IntToWord(0)) {
    return false;
  }

  word acc = Store::IntToWord(0);
  while(breakPointList != Store::IntToWord(0)) {
    Block *b = Store::WordToBlock(breakPointList);
    if (b == INVALID_POINTER) {
      Error("Invalid Thread List");
    }
    word wThread = b->GetArg(HD_POS);
    if (wThread == thread->ToWord()) {
      bp = true;
      if (thread->GetDebugMode() == Thread::NONE) {
	thread->SetDebugMode(Thread::DEBUG);
	GenerateMissingEvents();
      }
    } else {
      Block *curr = Store::AllocBlock((BlockLabel) 1, SIZE);
      curr->InitArg(HD_POS, wThread);
      curr->InitArg(TL_POS, acc);
      acc = curr->ToWord();
    }
    breakPointList = b->GetArg(TL_POS);
  }
  breakPointList = acc;
  return bp;
}

word Debugger::GetEventStream() {
  Stream *stream = Stream::FromWordDirect(eventStream);
  return stream->GetEntryList();
}

void Debugger::SendEvent(word event) {
  Stream *stream = Stream::FromWordDirect(eventStream);
  stream->SendEvent(event);
}

void Debugger::Detach(Thread *thread) {
  thread->SetDebugMode(Thread::DETACH);
  Scheduler::ResumeThread(thread);
}

void Debugger::SingleStep(Thread *thread) {
  Scheduler::ResumeThread(thread);
}

void Debugger::Breakpoint(Thread *thread) {
  SetBreakpoint(thread);
}

#endif
