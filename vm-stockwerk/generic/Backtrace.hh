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

class DllExport Backtrace: private Queue {
private:
  static const u_int initialBacktraceSize = 16; // to be checked
public:
  using Queue::ToWord;
  using Queue::Enqueue;

  // Backtrace Constuctor
  static Backtrace *New(word frame) {
    Queue *trace = Queue::New(initialBacktraceSize);
    trace->Enqueue(frame);
    return static_cast<Backtrace *>(trace);
  }
  // Backtrace Untagging
  static Backtrace *FromWordDirect(word x) {
    Queue *q = Queue::FromWordDirect(x);
    return static_cast<Backtrace *>(q);
  }

  // Backtrace Accessors
  void Dump() {
    while (!IsEmpty()) {
      word frame = Dequeue();
      Worker *worker =
	StackFrame::FromWordDirect(frame)->GetWorker();
      worker->DumpFrame(frame);
    }
  }
};

#endif
