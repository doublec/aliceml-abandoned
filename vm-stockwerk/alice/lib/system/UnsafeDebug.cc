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

#include "generic/Debug.hh"
#include "alice/primitives/Authoring.hh"

DEFINE1(UnsafeDebug_print) {
  Debug::Dump(x0);
  RETURN_UNIT;
} END

word UnsafeDebug(void) {
  Tuple *t = Tuple::New(9);
  //--** Inspect
  //--** InspectSig
  //--** InspectType
  //--** Print
  //--** inspect
  t->Init(5, Primitive::MakeClosure("UnsafeDebug_print",
				    UnsafeDebug_print, 1, true));
  //--** setPrintDepth
  //--** setPrintWidth
  //--** toString
  RETURN_STRUCTURE(t);
}
