//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstdio>
#include "Seam.hh"

int main(int argc, char *argv[]) {
  if (argc < 2) {
    std::fprintf(stderr, "usage: %s <languagelayer> <args...>\n", argv[0]);
    return 2;
  }
  String *languageId = String::New(argv[1]);
  argc--; argv++;

  InitSeam();
  Broker::Start(languageId, argc, argv);
  return Scheduler::Run();
}
