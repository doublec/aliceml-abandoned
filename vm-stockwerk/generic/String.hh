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

#ifndef __GENERIC__STRING_HH__
#define __GENERIC__STRING_HH__

#if defined(INTERFACE)
#pragma interface "generic/String.hh"
#endif

#include <cstring>
#include "store/Store.hh"

#define MAX_SIZE(t) \
  (MAX_BIGBLOCKSIZE * sizeof(u_int) / sizeof(t))

class SeamDll String: private Chunk {
public:
  static const u_int maxSize = MAX_SIZE(char);

  using Chunk::ToWord;
  using Chunk::GetSize;
  using Chunk::Hash;

  static String *New(u_int len) {
    return static_cast<String *>(Store::AllocChunk(len));
  }
  static String *New(const char *str, u_int len) {
    Chunk *chunk = Store::AllocChunk(len);
    std::memcpy(chunk->GetBase(), str, len);
    return static_cast<String *>(chunk);
  }
  static String *New(const char *str) {
    return New(str, std::strlen(str));
  }
  static String *FromWord(word x) {
    Chunk *chunk = Store::WordToChunk(x);
    return static_cast<String *>(chunk);
  }
  static String *FromWordDirect(word x) {
    Chunk *chunk = Store::DirectWordToChunk(x);
    return static_cast<String *>(chunk);
  }

  u_char *GetValue() {
    return reinterpret_cast<u_char *>(GetBase());
  }

  char *ExportC() {
    u_int n = GetSize();
    String *s = String::New(n + 1);
    char *p = reinterpret_cast<char *>(s->GetValue());
    std::memcpy(p, GetValue(), n);
    p[n] = '\0';
    return p;
  }
};

#endif
