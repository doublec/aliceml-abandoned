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
#include "generic/Transients.hh"
#include "generic/TaskStack.hh"
#include "generic/Scheduler.hh"
#include "alice/primitives/PrimitiveTable.hh"

int main(int argc, char *argv[]) {
  u_int memLimits[STORE_GENERATION_NUM];

  for (u_int i = 0; i < STORE_GENERATION_NUM; i++) {
    memLimits[i] = i + 1;
  }
  Store::InitStore(memLimits, 75, 20);

  RootSet::Init();
  Hole::Init();
  TaskStack::Init();
  Scheduler::Init();
  PrimitiveTable::Init();
  //--** enqueue the ur-thread
  Scheduler::Run();
  exit(0);
}
