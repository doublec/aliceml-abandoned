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
#pragma implementation "generic/Properties.hh"
#endif

#include <cstdlib>
#include <cstring>
#include "generic/Properties.hh"
#include "generic/RootSet.hh"
#include "generic/String.hh"

word Properties::aliceHome;
word Properties::rootUrl;
word Properties::commandLineArguments;
word Properties::atExn;

void Properties::Init() {
  RootSet::Add(aliceHome);
  RootSet::Add(rootUrl);
  RootSet::Add(commandLineArguments);
  RootSet::Add(atExn);

  char *home = std::getenv("STOCKHOME");
  if (home == NULL) {
    Error("could not determine installation directory");
  }
  u_int n = std::strlen(home);
  String *homeString = String::New(n + 1);
  u_char *p = homeString->GetValue();
  std::memcpy(p, home, n);
  p[n] = '/';
  aliceHome = homeString->ToWord();

  rootUrl = Store::IntToWord(0);
  commandLineArguments = Store::IntToWord(0);
  atExn = Store::IntToWord(0);
}
