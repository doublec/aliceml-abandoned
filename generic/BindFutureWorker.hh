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

#ifndef __GENERIC__BIND_FUTURE_WORKER_HH__
#define __GENERIC__BIND_FUTURE_WORKER_HH__

#if defined(INTERFACE)
#pragma interface "generic/BindFutureWorker.hh"
#endif

#include "generic/Worker.hh"

class Thread;
class Transient;

class SeamDll BindFutureWorker: public Worker {
private:
  // BindFutureWorker Constructor
  BindFutureWorker(): Worker() {}
public:
  // Exported BindFutureWorker Instance
  static BindFutureWorker *self;
  // BindFutureWorker Static Constructor
  static void Init() {
    self = new BindFutureWorker();
  }
  // Frame Handling
  static void PushFrame(Thread *thread, Transient *future);
  virtual u_int GetFrameSize(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *sFrame);
  virtual Result Handle(word data);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

#endif
