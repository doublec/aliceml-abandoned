//
// Author:
//   Jens Regenberg <jens@ps.uni-sb.de>
//
// Copyright:
//   Jens Regenberg, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if DEBUGGER
#if defined(INTERFACE)
#pragma implementation "alice/AliceDebuggerEvent.hh"
#endif

#include "alice/AliceDebuggerEvent.hh"

AliceEventAccessor *AliceEventAccessor::self;
AliceEventAccessor::AliceEventAccessor() {
  return;
}
#endif
