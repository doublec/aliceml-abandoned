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

#include "emulator/Authoring.hh"
#include "emulator/Properties.hh"

DEFINE0(UnsafeCommandLine_name) {
  RETURN(Properties::rootUrl);
} END

DEFINE0(UnsafeCommandLine_arguments) {
  RETURN(Properties::commandLineArguments);
} END

word UnsafeCommandLine(void) {
  Tuple *t = Tuple::New(2);
  t->Init(0, Primitive::MakeClosure("UnsafeCommandLine.arguments",
				    UnsafeCommandLine_arguments, 0, true));
  t->Init(1, Primitive::MakeClosure("UnsafeCommandLine.name",
				    UnsafeCommandLine_name, 0, true));
  RETURN_STRUCTURE(t);
}
