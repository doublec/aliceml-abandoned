//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000-2001
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__PICKLING__PICKLE_INPUT_STREAM_HH__
#define __GENERIC__PICKLING__PICKLE_INPUT_STREAM_HH__

#if defined(INTERFACE)
#pragma interface "generic/pickling/PickleInputStream.hh"
#endif

#include <cstdio>
#include "store/Store.hh"
#include "store/Handler.hh"
#include "generic/pickling/PrimPickle.hh"

class PickleInputStream: public Block {
private:
  static const u_int FILE_POS = 0;
  static const u_int SIZE = 1;

  static Handler *handler;
public:
  using Block::ToWord;

  static PickleInputStream *New(FILE *file) {
    Block *block =
      Store::AllocBlockWithHandler(PICKLEINPUTSTREAM_LABEL, SIZE, handler);
    block->InitArg(FILE_POS, Store::UnmanagedPointerToWord(file));
    return static_cast<PickleInputStream *>(block);
  }
  static PickleInputStream *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == PICKLEINPUTSTREAM_LABEL);
    return static_cast<PickleInputStream *>(b);
  }
  static PickleInputStream *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == PICKLEINPUTSTREAM_LABEL);
    return static_cast<PickleInputStream *>(b);
  }

  FILE *GetFile() {
    return static_cast<FILE *>
      (Store::DirectWordToUnmanagedPointer(GetArg(FILE_POS)));
  }
  PrimPickle::byte ReadByte() {
    return std::fgetc(GetFile());
  }
};

#endif __GENERIC__PICKLING__PICKLE_INPUT_STREAM_HH__
