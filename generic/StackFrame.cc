//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "generic/StackFrame.hh"
#endif

#include "generic/StackFrame.hh"
#include "generic/Worker.hh"

word StackFrame::Clone() {
  u_int size = GetWorker()->GetFrameSize(this);
  Block *clone = Store::AllocMutableBlock(MIN_DATA_LABEL, size);
  for (u_int i = size; i--;)
    clone->InitArg(i, UnsafeGetArg(i));
  return clone->ToWord();
}
