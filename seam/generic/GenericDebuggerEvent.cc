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
#pragma implementation "generic/GenericDebuggerEvent.hh"
#endif

#include "generic/GenericDebuggerEvent.hh"

GenericEventAccessor *GenericEventAccessor::self;

GenericEventAccessor::GenericEventAccessor() {
  return;
}

GenericDebuggerEvent *GenericDebuggerEvent::FromWord(word w) {
  Block *b = Store::WordToBlock(w);
  Assert(b == INVALID_POINTER || b->GetLabel() == (BlockLabel) GENERIC_EVENT_LABEL);
  return static_cast<GenericDebuggerEvent *>(b);
}

GenericDebuggerEvent *GenericDebuggerEvent::FromWordDirect(word w) {
  Block *b = Store::DirectWordToBlock(w);
  Assert(b->GetLabel() == (BlockLabel) GENERIC_EVENT_LABEL);
  return static_cast<GenericDebuggerEvent *>(b);
}
#endif
