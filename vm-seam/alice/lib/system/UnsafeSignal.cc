//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2004
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/Authoring.hh"

// TODO: This stuff belongs to the generic SEAM Layer

#if HAVE_SIGNAL
#include <signal.h>
#include <sys/time.h>
#endif

#if !HAVE_SIGNAL || HAVE_CONSOLECTRL
#include <windows.h>
#endif

enum { ALICE_SIGINT, ALICE_SIGSTOP };

// TODO: We currently accept both translated and untranslated signals.
// TODO: This needs a design.
static int TranslateSignal(int signal) {
#if HAVE_CONSOLECTRL
  switch (signal) {
  case ALICE_SIGINT:
#if ALICE_SIGINT != CTRL_C_EVENT
  case CTRL_C_EVENT:
#endif
    return CTRL_C_EVENT;
  case ALICE_SIGSTOP:
#if ALICE_SIGSTOP != CTRL_BREAK_EVENT
  case CTRL_BREAK_EVENT:
    return CTRL_BREAK_EVENT;
#endif
  default:
    Error("Unknown Signal");
  }
#elif HAVE_SIGNAL
  switch (signal) {
#if defined(SIGINT)
  case ALICE_SIGINT:
  case SIGINT:
    return SIGINT;
#endif
#if defined(SIGTSTP)
  case ALICE_SIGSTOP:
  case SIGTSTP:
    return SIGTSTP;
#endif
  default:
    Error("Unknown Signal");
  }
#else
  Error("Signal translation failure");
#endif
}

DEFINE2(UnsafeSignal_register) {
  DECLARE_INT(signal, x0);
  word closure = x1;
  SignalHandler::RegisterSignal(TranslateSignal(signal), closure);
  RETURN_UNIT;
} END

AliceDll word UnsafeSignal() {
  Record *record = Record::New(1);
  INIT_STRUCTURE(record, "UnsafeSignal", "register",
		 UnsafeSignal_register, 2);
  RETURN_STRUCTURE("UnsafeSignal$", record);
}
