//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "generic/Debug.hh"
#include "alice/Authoring.hh"

DEFINE1(UnsafeDebug_print) {
  Debug::Dump(x0);
  RETURN_UNIT;
} END

DEFINE1(UnsafeDebug_unimplemented) {
  Error("UnsafeDebug: unimplemented");
} END

word UnsafeDebug() {
  Record *record = Record::New(9);
  INIT_STRUCTURE(record, "UnsafeDebug", "setPrintDepth",
		 UnsafeDebug_unimplemented, 1);
  INIT_STRUCTURE(record, "UnsafeDebug", "setPrintWidth",
		 UnsafeDebug_unimplemented, 1);
  INIT_STRUCTURE(record, "UnsafeDebug", "toString",
		 UnsafeDebug_unimplemented, 1);
  INIT_STRUCTURE(record, "UnsafeDebug", "print",
		 UnsafeDebug_print, 1);
  INIT_STRUCTURE(record, "UnsafeDebug", "inspect",
		 UnsafeDebug_unimplemented, 1);
  INIT_STRUCTURE(record, "UnsafeDebug", "Print$",
		 UnsafeDebug_unimplemented, 1);
  INIT_STRUCTURE(record, "UnsafeDebug", "Inspect$",
		 UnsafeDebug_unimplemented, 1);
  INIT_STRUCTURE(record, "UnsafeDebug", "InspectType$",
		 UnsafeDebug_unimplemented, 1);
  INIT_STRUCTURE(record, "UnsafeDebug", "InspectSig$",
		 UnsafeDebug_unimplemented, 1);
  RETURN_STRUCTURE("UnsafeDebug$", record);
}
