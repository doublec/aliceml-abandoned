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

#include <iostream>
#include <sstream>
#include "adt/Queue.hh"
#include "generic/String.hh"
#include "generic/Worker.hh"
#include "generic/StackFrame.hh"
#include "generic/Scheduler.hh"

class SeamDll Backtrace: private Queue {
private:
  static const u_int initialBacktraceSize = 12; // to be checked
  static const u_int topCutoff = 20;
  static const u_int bottomCutoff = 20;

  void DumpNth(u_int n, std::ostream& out) {
    // FIXME: Hack Alert
    word wFrame = GetNthElement(n);
    u_int size = Store::DirectWordToBlock(wFrame)->GetSize();
    StackFrame *frame = Scheduler::PushFrame(size);
    StackFrame::New(frame, size, wFrame);
    out << "- ";
    frame->GetWorker()->DumpFrame(frame, out);
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
  
  static Backtrace *FromWord(word x) {
    return static_cast<Backtrace *>(Queue::FromWord(x));
  }
  
  static Backtrace *FromWordDirect(word x) {
    return static_cast<Backtrace *>(Queue::FromWordDirect(x));
  }
  
  void Dump(std::ostream& out = std::cerr) {
    u_int n = GetNumberOfElements();
  
    for (u_int i=0; i<n; i++) {
      if (i == topCutoff && n >= topCutoff + bottomCutoff + bottomCutoff/4) {
        u_int ommited = n - topCutoff - bottomCutoff;
        out << "    ... " << ommited << " frames ommited ..." << std::endl;
        i += ommited-1;
      }
      else {
        DumpNth(i, out);
      }
    }
  }
  
  String *DumpToString() {
    std::ostringstream ss;
    Dump(ss);
    std::string str = ss.str();
    return String::New(str);
  }
  
  Backtrace *Clone() {
    return static_cast<Backtrace*>(Queue::Clone());
  }
};

#endif
