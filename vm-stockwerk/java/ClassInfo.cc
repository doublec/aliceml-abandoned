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
  // Count number of static and instance fields:
  Table *fields = classInfo->GetFields();
  u_int i, nStaticFields = 0, nInstanceFields = 0;
  for (i = fields->GetCount(); i--; ) {
    FieldInfo *fieldInfo = FieldInfo::FromWordDirect(fields->Get(i));
    if (fieldInfo->IsStatic())
      nStaticFields++;
    else
      nInstanceFields++;
  }
  // Count number of static and virtual methods:
  Table *methods = classInfo->GetMethods();
  u_int nSuperVirtualMethods = super->GetNumberOfVirtualMethods();
  u_int nStaticMethods = 0, nVirtualMethods = nSuperVirtualMethods;
  for (i = methods->GetCount(); i--; ) {
    MethodInfo *methodInfo = MethodInfo::FromWordDirect(fields->Get(i));
    if (methodInfo->IsStatic())
      nStaticMethods++;
    else
      nVirtualMethods++;
  }
  // Construct virtual table:
  Block *virtualTable =
    Store::AllocBlock(JavaLabel::VirtualTable, nVirtualMethods);
  Block *superVirtualTable = super->GetVirtualTable();
  for (i = nSuperVirtualMethods; i--; )
    virtualTable->InitArg(i, superVirtualTable->GetArg(i));
  //--** missing: fill new slots

  return 0; //--** implement
}

Class *ClassInfo::Prepare() {
  return Class::New(this);
}
