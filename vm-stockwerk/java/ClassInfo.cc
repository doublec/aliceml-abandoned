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

bool ClassInfo::Verify() {
  return true; //--** implement verifier
}

Class *Class::New(ClassInfo *classInfo) {
  // Precondition: parent class has already been created
  Class *super = Class::FromWord(classInfo->GetSuper());
  Table *fields = classInfo->GetFields();
  u_int nStaticFields = 0, nInstanceFields = 0;
  for (u_int i = fields->GetCount(); i--; ) {
    FieldInfo *fieldInfo = FieldInfo::FromWordDirect(fields->Get(i));
    if (fieldInfo->IsStatic())
      nStaticFields++;
    else
      nInstanceFields++;
  }
  Table *methods = classInfo->GetMethods();
  u_int nSuperVirtualMethods = super->GetNumberOfVirtualMethods();
  u_int nStaticMethods = 0, nVirtualMethods = nSuperVirtualMethods;
  for (u_int j = methods->GetCount(); j--; ) {
    MethodInfo *methodInfo = MethodInfo::FromWordDirect(fields->Get(j));
    if (methodInfo->IsStatic())
      nStaticMethods++;
    else
      nVirtualMethods++;
  }

  Block *virtualTable =
    Store::AllocBlock(JavaLabel::VirtualTable, nVirtualMethods);

  return 0; //--** implement
}

Class *ClassInfo::Prepare() {
  return Class::New(this);
}
