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
#ifndef __ALICE__ALICE_DEBUGGER_EVENT_HH__
#define __ALICE__ALICE_DEBUGGER_EVENT_HH__

#if defined(INTERFACE)
#pragma interface "alice/AliceDebuggerEvent.hh"
#endif

#include "Seam.hh"

class AliceEventAccessor : public EventAccessor {
private:
  AliceEventAccessor();
public:
  static AliceEventAccessor *self;
  static void Init() {
    self = new AliceEventAccessor();
  }

  EventLabel GetLabel() {
    return ALICE_EVENT_LABEL;
  }
  word GetEvent(word w) {
    return w;
  }
};

class AliceDebuggerEvent : public DebuggerEvent {
public:
  using Block::ToWord;

  static AliceDebuggerEvent *New(word event) {
    Block *b = Store::AllocBlock((BlockLabel)ALICE_EVENT_LABEL, BASE_SIZE);
    b->InitArg(ACCESSOR_POS, 
	       Store::UnmanagedPointerToWord(AliceEventAccessor::self));
    b->InitArg(EVENT_POS,    event);
    return STATIC_CAST(AliceDebuggerEvent *, b);
  }
  static AliceDebuggerEvent *FromWord(word w) {
    Block *b = Store::WordToBlock(w);
    Assert(b == INVALID_POINTER || 
	   b->GetLabel() == (BlockLabel) ALICE_EVENT_LABEL);
    return STATIC_CAST(AliceDebuggerEvent *, b);
  }
  static AliceDebuggerEvent *FromWordDirect(word w) {
    Block *b = Store::DirectWordToBlock(w);
    Assert(b->GetLabel() == (BlockLabel) ALICE_EVENT_LABEL);
    return STATIC_CAST(AliceDebuggerEvent *, b);
  }
};
#endif
#endif
