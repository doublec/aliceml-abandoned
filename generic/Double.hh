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

#ifndef __GENERIC__DOUBLE_HH__
#define __GENERIC__DOUBLE_HH__

#if defined(INTERFACE)
#pragma interface "generic/Double.hh"
#endif

#include <cstring>
#include "store/Store.hh"

class SeamDll Double: private Chunk {
public:
  using Chunk::ToWord;

  static Double *New(double value) {
    Chunk *chunk = Store::AllocChunk(sizeof(double));
    char *to = chunk->GetBase(), *from = reinterpret_cast<char *>(&value);
#if DOUBLE_LITTLE_ENDIAN
    for (u_int i = sizeof(double); i--; *to++ = from[i]);
#elif DOUBLE_ARM_ENDIAN
    for (u_int i = sizeof(double)/2; i--; *to++ = from[i]);
    for (u_int i = sizeof(double)/2; i--; *to++ = from[i+(sizeof(double)/2)]);
#else
    std::memcpy(to, from, sizeof(double));
#endif
    return STATIC_CAST(Double *, chunk);
  }
  static Double *NewFromNetworkRepresentation(u_char *from) {
    Chunk *chunk = Store::AllocChunk(sizeof(double));
    std::memcpy(chunk->GetBase(), from, sizeof(double));
    return STATIC_CAST(Double *, chunk);
  }
  static Double *FromWord(word x) {
    Chunk *chunk = Store::WordToChunk(x);
    Assert(chunk == INVALID_POINTER || chunk->GetSize() == sizeof(double));
    return STATIC_CAST(Double *, chunk);
  }
  static Double *FromWordDirect(word x) {
    Chunk *chunk = Store::DirectWordToChunk(x);
    Assert(chunk->GetSize() == sizeof(double));
    return STATIC_CAST(Double *, chunk);
  }

  double GetValue() {
    double result;
    char *to = reinterpret_cast<char *>(&result), *from = GetBase();
#if DOUBLE_LITTLE_ENDIAN
    for (u_int i = sizeof(double); i--; *to++ = from[i]);
#elif DOUBLE_ARM_ENDIAN
    for (u_int i = sizeof(double)/2; i--; *to++ = from[i]);
    for (u_int i = sizeof(double)/2; i--; *to++ = from[i+(sizeof(double)/2)]);
#else
    std::memcpy(to, from, sizeof(double));
#endif
    return result;
  }
  u_char *GetNetworkRepresentation() {
    return reinterpret_cast<u_char *>(GetBase());
  }
};

#endif
