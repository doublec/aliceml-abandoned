//
// Author:
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
#if defined(INTERFACE)
#pragma implementation "generic/DebuggerEvent.hh"
#endif

#include "generic/DebuggerEvent.hh"
#include "generic/Debug.hh"

DebuggerEvent *DebuggerEvent::FromWord(word x) {
  Block *b = Store::WordToBlock(x);
  Assert(b == INVALID_POINTER || (b->GetLabel() > (BlockLabel) MIN_EVENT_LABEL 
				  && b->GetLabel() < (BlockLabel) MAX_EVENT_LABEL));
  return static_cast<DebuggerEvent *>(b);
}

DebuggerEvent *DebuggerEvent::FromWordDirect(word x) {
  Block *b = Store::DirectWordToBlock(x);
  Assert(b->GetLabel() > (BlockLabel) MIN_EVENT_LABEL 
	 && b->GetLabel() < (BlockLabel)MAX_EVENT_LABEL);
  return static_cast<DebuggerEvent *>(b);
}

#endif
