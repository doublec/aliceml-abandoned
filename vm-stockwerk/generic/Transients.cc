//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "generic/Transients.hh"
#endif

#include <cstring>
#include "generic/RootSet.hh"
#include "generic/Transients.hh"
#include "alice/Data.hh" //--** should not be here

word Hole::cyclicExn;
word Hole::holeExn;

void Hole::InitExceptions() {
  cyclicExn = UniqueConstructor::New(String::New("Hole.Cyclic"))->ToWord();
  RootSet::Add(cyclicExn);
  holeExn = UniqueConstructor::New(String::New("Hole.Hole"))->ToWord();
  RootSet::Add(holeExn);
}
