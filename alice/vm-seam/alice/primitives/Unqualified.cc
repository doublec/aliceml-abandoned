//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000-2002
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/Authoring.hh"


DEFINE2(opeq) {
  int result = Alice::Compare(x0, x1);
  if (result == -1) {
    PUSH_PRIM_SELF();
    return Worker::REQUEST;
  } else {
    RETURN_BOOL(result);
  }
} END


DEFINE2(opnoteq) {
  int result = Alice::Compare(x0, x1);
  if (result == -1) {
    PUSH_PRIM_SELF();
    return Worker::REQUEST;
  } else {
    RETURN_BOOL(!result);
  }
} END


void PrimitiveTable::RegisterUnqualified() {
  Register("op=", opeq, 2);
  Register("op<>", opnoteq, 2);
}
