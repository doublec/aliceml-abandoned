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

#ifndef __GENERIC__FLOAT_HH__
#define __GENERIC__FLOAT_HH__

#if defined(INTERFACE)
#pragma interface "generic/Float.hh"
#endif

#include <cstring>
#include "store/Store.hh"

class SeamDll Float: private Chunk {
public:
  using Chunk::ToWord;

  static Float *New(float value) {
    Chunk *chunk = Store::AllocChunk(sizeof(float));
    char *to = chunk->GetBase(), *from = reinterpret_cast<char *>(&value);
#if FLOAT_LITTLE_ENDIAN
    for (u_int i = sizeof(float); i--; *to++ = from[i]);
#else
    std::memcpy(to, from, sizeof(float));
#endif
    return STATIC_CAST(Float *, chunk);
  }
  static Float *NewFromNetworkRepresentation(u_char *from) {
    Chunk *chunk = Store::AllocChunk(sizeof(float));
    std::memcpy(chunk->GetBase(), from, sizeof(float));
    return STATIC_CAST(Float *, chunk);
  }
  static Float *FromWord(word x) {
    Chunk *chunk = Store::WordToChunk(x);
    Assert(chunk == INVALID_POINTER || chunk->GetSize() == sizeof(float));
    return STATIC_CAST(Float *, chunk);
  }
  static Float *FromWordDirect(word x) {
    Chunk *chunk = Store::DirectWordToChunk(x);
    Assert(chunk->GetSize() == sizeof(float));
    return STATIC_CAST(Float *, chunk);
  }

  float GetValue() {
    float result;
    char *to = reinterpret_cast<char *>(&result), *from = GetBase();
#if FLOAT_LITTLE_ENDIAN
    for (u_int i = sizeof(float); i--; *to++ = from[i]);
#else
    std::memcpy(to, from, sizeof(float));
#endif
    return result;
  }
  u_char *GetNetworkRepresentation() {
    return reinterpret_cast<u_char *>(GetBase());
  }
};

#endif
