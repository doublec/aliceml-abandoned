//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "generic/Properties.hh"
#include "alice/primitives/Authoring.hh"

DEFINE0(UnsafeCommandLine_name) {
  RETURN(Properties::rootUrl);
} END

DEFINE0(UnsafeCommandLine_arguments) {
  RETURN(Properties::commandLineArguments);
} END

word UnsafeCommandLine() {
  Record *record = Record::New(2);
  INIT_STRUCTURE(record, "UnsafeCommandLine", "name",
		 UnsafeCommandLine_name, 0, true);
  INIT_STRUCTURE(record, "UnsafeCommandLine", "arguments",
		 UnsafeCommandLine_arguments, 0, true);
  RETURN_STRUCTURE("UnsafeCommandLine$", record);
}
