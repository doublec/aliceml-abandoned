//
// Authors:
//   Jens Regenberg <jens@ps.uni-sb.de>
//
// Copyright:
//   Jens Regenberg, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if DEBUGGER
#ifndef __GENERIC__DEBUG_FRAME_HH__
#define __GENERIC__DEBUG_FRAME_HH__

#if defined(INTERFACE)
#pragma interface "generic/DebugFrame.hh"
#endif

#include "generic/StackFrame.hh"

class SeamDll DebugFrame: private StackFrame {
private:
  enum { EVENT_POS, SIZE };
public:
  // DebugFrame Constructor
  static DebugFrame *New(Worker *worker, word event) {
    NEW_STACK_FRAME(frame, worker, SIZE);
    frame->InitArg(EVENT_POS, event);
    return STATIC_CAST(DebugFrame *, frame);
  }

  // DebugFrame Accessors
  word GetEvent() {
    return StackFrame::GetArg(EVENT_POS);
  }
  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
};
#endif
#endif
