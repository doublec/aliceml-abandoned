//
// Authors:
//   Jens Regenberg, <jens@ps.uni-sb.de>
//
// Copyright:
//   Jens Regenberg, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if DEBUGGER
#ifndef __GENERIC__DEBUG_WORKER_HH__
#define __GENERIC__DEBUG_WORKER_HH__

#if defined(INTERFACE)
#pragma interface "generic/DebugWorker.hh"
#endif

#include <ostream>
#include "generic/Worker.hh"

class SeamDll DebugWorker: public Worker {
private:
  // DebugWorker Constructor
  DebugWorker(): Worker() {}
public:
  static word invalidStackFrame;
  // Exported DebugWorker Instance
  static DebugWorker *self;
  // DebugWorker Static Constructor
  static void Init() {
    self = new DebugWorker();
  }
  // Frame Handling
  static void PushFrame(word event);
  virtual u_int GetFrameSize(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *);
  virtual Result Handle(word);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame, std::ostream& out);
  // Event Accessors
  static word GetEvent(StackFrame *sFrame);
};

#endif
#endif
