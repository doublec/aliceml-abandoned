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

  void DumpNth(u_int n) {
    // FIXME: Hack Alert
    word wFrame = GetNthElement(n);
    u_int size = Store::DirectWordToBlock(wFrame)->GetSize();
    StackFrame *frame = Scheduler::PushFrame(size);
    StackFrame::New(frame, size, wFrame);
    fprintf(stderr, "- ");
    frame->GetWorker()->DumpFrame(frame);
    Scheduler::PopFrame(size);
  }

public:
  using Queue::ToWord;
  using Queue::Enqueue;
  using Queue::GetNumberOfElements;
  using Queue::GetNthElement;

  static Backtrace *New() {
    return static_cast<Backtrace *>(Queue::New(initialBacktraceSize));
  }
  static Backtrace *New(word frame) {
    Backtrace *backtrace = New();
    backtrace->Enqueue(frame);
    return backtrace;
  }
  static Backtrace *FromWord(word x) {
    return static_cast<Backtrace *>(Queue::FromWord(x));
  }
  static Backtrace *FromWordDirect(word x) {
    return static_cast<Backtrace *>(Queue::FromWordDirect(x));
  }

  void Dump() {
    u_int n = GetNumberOfElements();
  
    for (u_int i=0; i<n; i++) {
      if (i == topCutoff && n >= topCutoff + bottomCutoff + bottomCutoff/4) {
        u_int ommited = n - topCutoff - bottomCutoff;
        fprintf(stderr, "    ... %"U_INTF" frames omitted ...\n", ommited);
        i += ommited-1;
      }
      else {
        DumpNth(i);
      }
    }
  }
};

#endif
