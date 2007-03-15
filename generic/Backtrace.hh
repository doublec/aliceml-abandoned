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
  static const u_int topCutoff = 20;
  static const u_int bottomCutoff = 20;

  void Dump1() {
    // to be done: Hack Alert
    word wFrame = Dequeue();
    u_int size = Store::DirectWordToBlock(wFrame)->GetSize();
    StackFrame *frame = Scheduler::PushFrame(size);
    StackFrame::New(frame, size, wFrame);
    Worker *worker = frame->GetWorker();
    fprintf(stderr, "- ");
    worker->DumpFrame(frame);
    Scheduler::PopFrame(size);
  }

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
    for (int i = topCutoff; i > 0 && !IsEmpty(); i--)
      Dump1();
    if (GetNumberOfElements() > bottomCutoff + bottomCutoff/4) {
      fprintf(stderr, "... (%d frames omitted)\n",
              GetNumberOfElements() - bottomCutoff);
      while (GetNumberOfElements() > bottomCutoff)
        Dequeue();
    }
    while (!IsEmpty())
      Dump1();
  }
};

#endif
