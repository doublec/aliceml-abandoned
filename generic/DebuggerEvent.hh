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

#if DEBUGGER
#ifndef __GENERIC__DEBUGGER_EVENT_HH__
#define __GENERIC__DEBUGGER_EVENT_HH__

#if defined(INTERFACE)
#pragma interface "generic/DebuggerEvent.hh"
#endif

#include "store/Store.hh"

// Known Event Types
typedef enum {
  MIN_EVENT_LABEL,
  ALICE_EVENT_LABEL,
  GENERIC_EVENT_LABEL,
  MAX_EVENT_LABEL
} EventLabel;


class EventAccessor{
public:
  EventAccessor() {}
  virtual EventLabel GetLabel() = 0;
  virtual word GetEvent(word event) = 0;
};

class SeamDll DebuggerEvent : public Block {
protected:
  enum { ACCESSOR_POS, EVENT_POS, BASE_SIZE };
public:
  using Block::ToWord;

  // DebuggerEvent Constructor
  static DebuggerEvent *New(EventLabel l, EventAccessor *accessor, word event) {
    Block *b = Store::AllocBlock((BlockLabel) l, BASE_SIZE);
    b->InitArg(ACCESSOR_POS, Store::UnmanagedPointerToWord(accessor));
    b->InitArg(EVENT_POS,    event);
    return static_cast<DebuggerEvent *>(b);
  }

  static DebuggerEvent *FromWord(word w);
  static DebuggerEvent *FromWordDirect(word w);
  EventAccessor *GetAccessor() {
    return static_cast<EventAccessor *>
      (Store::WordToUnmanagedPointer(Block::GetArg(ACCESSOR_POS)));
  }

  word GetEvent() {
    return GetAccessor()->GetEvent(Block::GetArg(EVENT_POS));
  }
};
#endif
#endif
