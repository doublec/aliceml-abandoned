//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstdio>
#include "generic/Scheduler.hh"
#include "java/JavaLanguageLayer.hh"
#include "java/Startup.hh"
#include "InitSeam.hh"

static int JavaMain(u_int argc, char *argv[]) {
  if (argc < 2) {
    std::fprintf(stderr, "usage: %s <classfile> <args...>\n", argv[0]);
    return 2;
  }

  InitSeam();
  // Set up Java Language Layer:
  JavaLanguageLayer::Init();
  // Link and execute boot component:
  Startup(argc, argv);
  return Scheduler::Run();
}

int main(int argc, char *argv[]) {
  std::exit(JavaMain(argc, argv));
}
