//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
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

static const char *holeExnContents = "Hole.Hole";

word Hole::holeExn;

void Hole::Init() {
  Chunk *chunk = Store::AllocChunk(std::strlen(holeExnContents));
  std::memcpy(chunk->GetBase(), holeExnContents, strlen(holeExnContents));
  holeExn = chunk->ToWord();
  RootSet::Add(holeExn);
}
