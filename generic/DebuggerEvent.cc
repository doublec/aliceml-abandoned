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
  if (b->GetLabel() == 7) {
    Debug::maxDepth = 3;
    FILE *file = fopen("/home/jens/uni/fopra/readEvent.txt", "w+");
    if (file == NULL) {
      fprintf(stderr, "Unable to open file\n");
      exit(0);
    }
    Debug::DumpTo(file, x);
  }
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
