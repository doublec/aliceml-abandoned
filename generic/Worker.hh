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

class StackFrame;
#if PROFILE
class String;
#endif

class SeamDll Worker {
public:
  enum Result {
    CONTINUE, PREEMPT, SUSPEND, RAISE, REQUEST, TERMINATE, EXIT
  };
  // Worker Constructor
  Worker() SeamMemberDll;
  // Calling Convention Conversion
  static SeamMemberDll void Construct();
  //   Deconstruct returns 1 iff argument needs to be requested,
  //   in which case it sets Scheduler::currentData as a side-effect;
  //   returns 0 iff deconstruction was immediately successful
  static SeamMemberDll u_int Deconstruct();
  // Frame Handling
  virtual u_int GetFrameSize(StackFrame *sFrame) = 0;
  virtual SeamMemberDll void PurgeFrame(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *sFrame) = 0;
  virtual SeamMemberDll Result Handle(word data);
  // Debugging
  virtual const char *Identify() = 0;
  virtual void DumpFrame(StackFrame *sFrame) = 0;
#if PROFILE
  // Profiling
  virtual SeamMemberDll word GetProfileKey(StackFrame *sFrame);
  virtual SeamMemberDll String *GetProfileName(StackFrame *sFrame);
#endif
};

#endif
