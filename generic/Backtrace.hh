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

#ifndef __GENERIC__BACKTRACE_HH__
#define __GENERIC__BACKTRACE_HH__

#if defined(INTERFACE)
#pragma interface "generic/Backtrace.hh"
#endif

#include "adt/Queue.hh"
#include "generic/Worker.hh"
#include "generic/StackFrame.hh"
#include "generic/Scheduler.hh"

class SeamDll Backtrace: private Queue {
private:
  static const u_int initialBacktraceSize = 12; // to be checked
public:
  using Queue::ToWord;
  using Queue::Enqueue;
  using Queue::GetNumberOfElements;
  using Queue::GetNthElement;

  static Backtrace *New() {
    return STATIC_CAST(Backtrace *, Queue::New(initialBacktraceSize));
  }
  static Backtrace *New(word frame) {
    Backtrace *backtrace = New();
    backtrace->Enqueue(frame);
    return backtrace;
  }
  static Backtrace *FromWord(word x) {
    return STATIC_CAST(Backtrace *, Queue::FromWord(x));
  }
  static Backtrace *FromWordDirect(word x) {
    return STATIC_CAST(Backtrace *, Queue::FromWordDirect(x));
  }

  void Dump() {
    // to be done: Hack Alert
    while (!IsEmpty()) {
      word wFrame = Dequeue();
      u_int size = Store::DirectWordToBlock(wFrame)->GetSize();
      StackFrame *frame = Scheduler::PushFrame(size);
      StackFrame::New(frame, size, wFrame);
      Worker *worker = frame->GetWorker();
      worker->DumpFrame(frame);
      Scheduler::PopFrame(size);
    }
  }
};

#endif
