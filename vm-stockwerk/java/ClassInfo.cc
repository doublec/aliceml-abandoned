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
  Assert(*p == '(');
  p++;
  u_int nArgs = 0;
  while (*p != ')') {
    while (*p == '[') p++;
    switch (*p) {
    case 'B': case 'C': case 'D': case 'F':
    case 'I': case 'J': case 'S': case 'Z':
      p++;
      break;
    case 'L':
      while (*p++ != ';');
      break;
    default:
      Error("invalid method descriptor");
    }
    nArgs++;
  }
  return nArgs;
}

bool ClassInfo::Verify() {
  return true; //--** implement verifier
}
