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
#include "alice/Data.hh" //--**
#include "alice/Types.hh" //--**

word Properties::aliceHome;
word Properties::rootUrl;
word Properties::commandLineArguments;
word Properties::atExn;

void Properties::Init(char *home, u_int argc, char *argv[]) {
  RootSet::Add(aliceHome);
  RootSet::Add(rootUrl);
  RootSet::Add(commandLineArguments);
  RootSet::Add(atExn);

  // Initialize aliceHome, making sure it ends in a trailing slash:
  u_int n = std::strlen(home);
  if (n != 0 && home[n - 1] == '/') {
    aliceHome = String::New(home, n)->ToWord();
  } else {
    String *homeString = String::New(n + 1);
    u_char *p = homeString->GetValue();
    std::memcpy(p, home, n);
    p[n] = '/';
    aliceHome = homeString->ToWord();
  }

  // Initialize rootUrl:
  if (argc < 2) {
    rootUrl = Store::IntToWord(0);
  } else {
    Properties::rootUrl = String::New(argv[1])->ToWord();
    argv++; argc--;
  }

  // Initialize commandLineArguments:
  word list = Store::IntToWord(Types::nil);
  argv++; argc--;
  for (u_int i = argc; i--; ) {
    TagVal *cons = TagVal::New(Types::cons, 2);
    cons->Init(0, String::New(argv[i])->ToWord());
    cons->Init(1, list);
    list = cons->ToWord();
  }
  Properties::commandLineArguments = list;

  // Initialize atExn:
  atExn = Store::IntToWord(0);
}
