//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "emulator/Properties.hh"
#endif

#include <cstdlib>
#include "emulator/Properties.hh"
#include "emulator/RootSet.hh"

word Properties::aliceHome;
word Properties::rootUrl;
word Properties::commandLineArguments;

void Properties::Init() {
  RootSet::Add(aliceHome);
  RootSet::Add(rootUrl);
  RootSet::Add(commandLineArguments);

  char *home = getenv("STOCKHOME");
  if (home == NULL) {
    Error("could not determine installation directory\n");
  }
  u_int n = strlen(home);
  Chunk *homeChunk = Store::AllocChunk(n + 1);
  char *homeChunkBase = homeChunk->GetBase();
  memcpy(homeChunkBase, home, n);
  homeChunkBase[n] = '/';
  aliceHome = homeChunk->ToWord();

  rootUrl = Store::IntToWord(0);
  commandLineArguments = Store::IntToWord(0);
}
