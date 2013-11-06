//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000-2003
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
#include "generic/UniqueString.hh"

word Hole::cyclicExn;
word Hole::holeExn;

void Hole::Init() {
  cyclicExn = UniqueString::New(String::New("Hole.Cyclic"))->ToWord();
  RootSet::Add(cyclicExn);
  holeExn = UniqueString::New(String::New("Hole.Hole"))->ToWord();
  RootSet::Add(holeExn);
}
