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
#pragma implementation "emulator/Transients.hh"
#endif

#include <cstring>
#include "emulator/RootSet.hh"
#include "emulator/Transients.hh"

static const char *holeExnContents = "Hole.Hole";
word Hole::holeExn;

void Hole::Init() {
  Chunk *chunk = Store::AllocChunk(std::strlen(holeExnContents));
  std::memcpy(chunk->GetBase(), holeExnContents, strlen(holeExnContents));
  holeExn = chunk->ToWord();
  RootSet::Add(holeExn);
}

static const char *cyclicExnContents = "Future.cyclic";
word Future::cyclicExn;

void Future::Init() {
  Chunk *chunk = Store::AllocChunk(std::strlen(cyclicExnContents));
  std::memcpy(chunk->GetBase(), holeExnContents, strlen(cyclicExnContents));
  cyclicExn = chunk->ToWord();
  RootSet::Add(cyclicExn);
}

