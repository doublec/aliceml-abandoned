//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "Alice.hh"

int main(int argc, char *argv[]) {
  char *home = std::getenv("ALICE_HOME");
  if (home == NULL) {
    Error("could not determine installation directory");
  }
  std::exit(AliceMain(home, argc, argv));
}
