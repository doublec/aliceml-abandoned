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
#include "Java.hh"

int main(int argc, char *argv[]) {
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
