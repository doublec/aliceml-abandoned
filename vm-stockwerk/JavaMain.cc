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
#include <cstdlib>
#include "Seam.hh"
#include "java/JavaLanguageLayer.hh"
#include "java/Startup.hh"

static void InitJava() {
  static bool initialized = false;
  if (!initialized) {
    JavaLanguageLayer::Init();
    initialized = true;
  }
}

void Start(int argc, char *argv[]) {
  if (argc < 2) {
    std::fprintf(stderr, "usage: %s <classfile> <args...>\n", argv[0]);
    std::exit(2);
  }
  InitJava();
  Startup(argc, argv);
}

Worker::Result Load(String *name) {
  InitJava();
  Error("java.dll: Load not implemented");   //--** to be done
}
