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

DEFINE1(UnsafeDebug_print) {
  TaskStack::Dump(x0);
  RETURN_UNIT;
} END

word UnsafeDebug(void) {
  Tuple *t = Tuple::New(9);
  //--** Inspect
  //--** InspectSig
  //--** InspectType
  //--** Print
  //--** inspect
  t->Init(5, Primitive::MakeClosure("UnsafeDebug_print", UnsafeDebug_print, 1));
  //--** setPrintDepth
  //--** setPrintWidth
  //--** toString
  RETURN_STRUCTURE(t);
}
