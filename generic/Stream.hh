//
// Author:
//   Jens Regenberg <jens@ps.uni-sb.de>
//
// Copyright:
//   Jens Regenberg, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__STREAM_HH__
#define __GENERIC__STREAM_HH__

#if defined(INTERFACE)
#pragma interface "generic/Stream.hh"
#endif

#include "generic/Transients.hh"
#include "generic/Debug.hh"

class EntryList : public Block {
private: 
  static const BlockLabel ENTRY_LIST_LABEL = (BlockLabel) (MIN_DATA_LABEL + 5);
  enum { ENTRY_POS, TAIL_POS, SIZE };

public:
  using Block::ToWord;

  // StreamEntry Constructor
  static EntryList *New(word entry, word tail) {
    Block *b = Store::AllocBlock(ENTRY_LIST_LABEL, SIZE);
    b->InitArg(ENTRY_POS, entry);
    b->InitArg(TAIL_POS,  tail);
    return STATIC_CAST(EntryList *, b);
  }

  // StreamEntry Untagging
  static EntryList *FromWord(word w) {
    Block *b = Store::WordToBlock(w);
    Assert(b == INVALID_POINTER || b->GetLabel() == ENTRY_LIST_LABEL);
    return STATIC_CAST(EntryList *, b);
  }
  static EntryList *FromWordDirect(word w) {
    Block *b = Store::WordToBlock(w);
    Assert(b->GetLabel() == ENTRY_LIST_LABEL);
    return STATIC_CAST(EntryList *, b);
  }

  // StreamEntry Accessors
  word Hd() {
    return GetArg(ENTRY_POS);
  }

  word Tl() {
    return GetArg(TAIL_POS);
  }
};

class Stream : public Block {
private:
  static const BlockLabel STREAM_LABEL = MIN_DATA_LABEL;

  enum { STREAM_POS, SIZE };

public:
  using Block::ToWord;

  // Stream Constructor
  static Stream *New() {
    Block *b = Store::AllocBlock(STREAM_LABEL, SIZE);
    b->InitArg(STREAM_POS, Future::New()->ToWord());
    return STATIC_CAST(Stream *, b);
  }

  // Stream Untagging
  static Stream *FromWord(word w) {
    Block *b = Store::WordToBlock(w);
    Assert(b == INVALID_POINTER || b->GetLabel() == STREAM_LABEL);
    return STATIC_CAST(Stream *, b);
  }
  static Stream *FromWordDirect(word w) {
    Block *b = Store::WordToBlock(w);
    Assert(b->GetLabel() == STREAM_LABEL);
    return STATIC_CAST(Stream *, b);
  }

  // Stream Accessors
  void SendEvent(word event);
  word GetEntryList();
};
#endif
