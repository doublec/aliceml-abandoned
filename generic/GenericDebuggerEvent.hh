//
// Author:
//   Jens Regenberg <jens@ps.uni-sb.de>
//
// Copyright:
//   Jens Regenberg, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if DEBUGGER
#ifndef __GENERIC__GENERIC_DEBUGGER_EVENT_HH__
#define __GENERIC__GENERIC_DEBUGGER_EVENT_HH__

#if defined(INTERFACE)
#pragma interface "generic/GenericDebuggerEvent.hh"
#endif

#include "generic/DebuggerEvent.hh"
#include "store/Store.hh"

class Thread;

class SeamDll GenericEventAccessor : public EventAccessor {
private:
  GenericEventAccessor(): EventAccessor() {}
  // to be reviewed
  enum { THREAD_POS, TYPE_POS, EXN_POS, SIZE };
public:

  static GenericEventAccessor *self;
  static void Init() {
    self = new GenericEventAccessor();
  }
  virtual EventLabel GetLabel() {
    return GENERIC_EVENT_LABEL;
  }
  virtual word GetEvent(word w) {
    return w;
  }

  virtual word GetThread(word event) {
    // to be reviewed
    Block *b = Store::DirectWordToBlock(event);
    return b->GetArg(THREAD_POS);
  }

  virtual int GetType(word event) {
    // to be reviewed
    Block *b = Store::DirectWordToBlock(event);
    return Store::WordToInt(b->GetArg(TYPE_POS));
  }

  virtual word GetException(word event) {
    // to be reviewed
    Block *b = Store::DirectWordToBlock(event);
    return b->GetArg(EXN_POS);
  }
};

class GenericDebuggerEvent : public DebuggerEvent {
private:
  enum { THREAD_POS, TYPE_POS, EXN_POS, SIZE };
public:
  using Block::ToWord;
  
  // Known Event Types
  enum { BLOCKED, RUNNABLE, TERMINATED , UNCAUGHT } State;

  static GenericDebuggerEvent *New(int type, word thread, word exn) {
    Block *event = Store::AllocBlock((BlockLabel)GENERIC_EVENT_LABEL, SIZE);
    Block *b = Store::AllocBlock((BlockLabel)GENERIC_EVENT_LABEL,       BASE_SIZE);
    event->InitArg(THREAD_POS, thread);
    event->InitArg(TYPE_POS,   type);
    event->InitArg(EXN_POS,    exn);
    b->InitArg(ACCESSOR_POS, Store::UnmanagedPointerToWord(GenericEventAccessor::self));
    b->InitArg(EVENT_POS,    event->ToWord());

    return STATIC_CAST(GenericDebuggerEvent *, b);
  }

  static GenericDebuggerEvent *FromWord(word w);
  static GenericDebuggerEvent *FromWordDirect(word w);
};
#endif
#endif
