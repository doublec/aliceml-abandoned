//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "generic/RootSet.hh"
#include "generic/Closure.hh"
#include "generic/Transients.hh"
#include "generic/NativeTaskAuthoring.hh"
#include "generic/InternalTasks.hh"

DEFINE1(InternalTasks_await) {
  if (Store::WordToTransient(x0) != INVALID_POINTER) {
    REQUEST(x0);
  } else {
    RETURN(x0);
  }
} END

static inline void Set(word &member, NativeTaskManager::function f,
		       int nargs, u_int nslots) {
  member = (new NativeTaskManager(f, nargs, nslots))->ToClosure()->ToWord();
  RootSet::Add(member);
}

word InternalTasks::await;

void InternalTasks::Init() {
  Set(await, InternalTasks_await, -1, 0);
}
