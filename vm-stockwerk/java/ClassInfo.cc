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
#pragma implementation "java/ClassInfo.hh"
#endif

#include "java/ClassInfo.hh"

u_int MethodInfo::GetNumberOfArguments() {
  JavaString *descriptor = GetDescriptor();
  u_wchar *p = descriptor->GetBase();
  //--**
  return 0;
}

bool ClassInfo::Verify() {
  return true; //--** implement verifier
}
