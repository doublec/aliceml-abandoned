//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "java/Data.hh"
#endif

#include "java/ClassInfo.hh"

Class *Class::GetSuperClass() {
  ClassInfo *classInfo = ClassInfo::FromWordDirect(GetArg(CLASS_INFO_POS));
  word wSuper = classInfo->GetSuper();
  if (wSuper == Store::IntToWord(0)) return INVALID_POINTER;
  Class *super = Class::FromWord(wSuper);
  Assert(super != INVALID_POINTER);
  return super;
}
