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

#include "scheduler/Scheduler.hh"
#include "builtins/Primitive.hh"
#include "builtins/GlobalPrimitives.hh"

int main(int argc, char *argv[]) {
  u_int memLimits[STORE_GENERATION_NUM];

  for (u_int i = 0; i < STORE_GENERATION_NUM; i++) {
    memLimits[i] = (i + 1);
  }
  Store::InitStore(memLimits);

  Primitive::Init();
  GlobalPrimitives::Init();
  Scheduler::Init();
  //--** enqueue the ur-thread
  Scheduler::Run();
  exit(0);
}
