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

word Hole::cyclicExn;
word Hole::holeExn;

void Hole::Init() {
  //--** this hard codes constructor representation
  static const char cyclicExnName[] = "Hole.Cyclic";
  Chunk *chunk1 = Store::AllocChunk(sizeof(cyclicExnName) - 1);
  std::memcpy(chunk1->GetBase(), cyclicExnName, sizeof(cyclicExnName) - 1);
  cyclicExn = chunk1->ToWord();
  RootSet::Add(cyclicExn);

  static const char holeExnName[] = "Hole.Hole";
  Chunk *chunk2 = Store::AllocChunk(sizeof(holeExnName) - 1);
  std::memcpy(chunk2->GetBase(), holeExnName, sizeof(holeExnName) - 1);
  holeExn = chunk2->ToWord();
  RootSet::Add(holeExn);
}
