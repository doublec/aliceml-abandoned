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

enum { ALICE_SIGINT };

static int TranslateSignal(int signal) {
#if HAVE_CONSOLECTRL
  switch (signal) {
  case ALICE_SIGINT:
    return CTRL_C_EVENT;
  default:
    Error("Unknown Signal");
  }
#elif HAVE_SIGNAL
  switch (signal) {
#if defined(SIGINT)
  case ALICE_SIGINT:
    return SIGINT;
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
