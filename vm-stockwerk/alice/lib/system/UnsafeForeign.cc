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

#include "generic/Tuple.hh"
#include "generic/Transients.hh"
#include "generic/ConcreteCode.hh"
#include "alice/Authoring.hh"

DEFINE2(UnsafeForeign_catch) {
  DECLARE_CLOSURE(closure, x0);
  x1 = x1;
  Scheduler::nArgs = 0;
  return Scheduler::PushCall(closure->ToWord());
} END

DEFINE1(UnsafeForeign_exnMessage) {
  x0 = x0;
  RETURN_UNIT;
} END

word UnsafeForeign() {
  Record *record = Record::New(2);
  INIT_STRUCTURE(record, "UnsafeForeign", "catch",
		 UnsafeForeign_catch, 2, true);
  INIT_STRUCTURE(record, "UnsafeForeign", "exnMessage",
		 UnsafeForeign_exnMessage, 1, true);
  RETURN_STRUCTURE("UnsafeForeign$", record);
}
