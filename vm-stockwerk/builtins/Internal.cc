//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "builtins/Authoring.hh"

DEFINE1(Internal_raise) {
  RAISE(x0);
} END

void Primitive::RegisterInternal() {
  Register("Internal.raise", Internal_raise);
};
