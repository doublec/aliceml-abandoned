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

#include "Stockwerk.hh"

int main(int argc, char *argv[]) {
  char *home = std::getenv("STOCKHOME");
  if (home == NULL) {
    Error("could not determine installation directory");
  }
  std::exit(StockwerkMain(home, argc, argv));
}
