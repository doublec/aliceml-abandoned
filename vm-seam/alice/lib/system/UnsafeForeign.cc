//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus and Leif Kornstaedt, 2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/Authoring.hh"

DEFINE2(UnsafeForeign_catch) {
  DECLARE_CLOSURE(closure, x0);
  x1 = x1; // unused: this VM has no foreign exceptions
  Scheduler::nArgs = 0;
  return Scheduler::PushCall(closure->ToWord());
} END

DEFINE1(UnsafeForeign_exnMessage) {
  // can never be called: this VM has no foreign exceptions
  x0 = x0;
  RETURN_UNIT;
} END

AliceDll word UnsafeForeign() {
  Record *record = Record::New(2);
  INIT_STRUCTURE(record, "UnsafeForeign", "catch",
		 UnsafeForeign_catch, 2);
  INIT_STRUCTURE(record, "UnsafeForeign", "exnMessage",
		 UnsafeForeign_exnMessage, 1);
  RETURN_STRUCTURE("UnsafeForeign$", record);
}
