//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__WORKER_HH__
#define __GENERIC__WORKER_HH__

#if defined(INTERFACE)
#pragma interface "generic/Worker.hh"
#endif

#include "store/Store.hh"

#if PROFILE
class StackFrame;
class String;
#endif

class DllExport Worker {
public:
  enum Result {
    CONTINUE, PREEMPT, SUSPEND, RAISE, REQUEST, TERMINATE, EXIT
  };
  // Worker Constructor
  Worker() {}
  // Calling Convention Conversion
  static void Construct();
  //   Deconstruct returns 1 iff argument needs to be requested,
  //   in which case it sets Scheduler::currentData as a side-effect;
  //   returns 0 iff deconstruction was immediately successful
  static u_int Deconstruct();
  // Frame Handling
  virtual void PurgeFrame(word wFrame);
  // Execution
  virtual Result Run() = 0;
  virtual Result Handle(word data);
  // Debugging
  virtual const char *Identify() = 0;
  virtual void DumpFrame(word wFrame) = 0;
#if PROFILE
  // Profiling
  virtual word GetProfileKey(StackFrame *frame);
  virtual String *GetProfileName(StackFrame *frame);
#endif
};

#endif
