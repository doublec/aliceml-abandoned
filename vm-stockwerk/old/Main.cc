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
  Primitive::Init();
  GlobalPrimitives::Init();
  Scheduler::Init();
  //--** enqueue the ur-thread
  Scheduler::Run();
  exit(0);
}
