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

#ifndef __GENERIC__PUSH_CALL_WORKER_HH__
#define __GENERIC__PUSH_CALL_WORKER_HH__

#if defined(INTERFACE)
#pragma interface "generic/PushCallWorker.hh"
#endif

#include "generic/Worker.hh"

class SeamDll PushCallWorker: public Worker {
private:
  // PushCallWorker Constructor
  PushCallWorker(): Worker() {}
public:
  // Exported PushCallWorker Instance
  static PushCallWorker *self;
  // PushCallWorker Static Constructor
  static void Init() {
    self = new PushCallWorker();
  }
  // Frame Handling
  static void PushFrame(word closure);
  static void PushFrame(Thread *thread, word closure);
  virtual u_int GetFrameSize(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *sFrame);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

#endif
