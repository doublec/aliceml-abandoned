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
    MethodInfo *methodInfo = MethodInfo::FromWordDirect(methods->Get(i));
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
  Block *b = Store::AllocBlock(JavaLabel::Class,
			       BASE_SIZE + nStaticFields + nStaticMethods);
  b->InitArg(CLASS_INFO_POS, classInfo->ToWord());
  b->InitArg(NUMBER_OF_VIRTUAL_METHODS_POS, nVirtualMethods);
  b->InitArg(NUMBER_OF_INSTANCE_FIELDS_POS, nInstanceFields);
  b->InitArg(VIRTUAL_TABLE_POS, virtualTable->ToWord());
  b->InitArg(LOCK_POS, Lock::New()->ToWord());
  for (i = nStaticFields; i--; )
    //--** initialization incorrect for long/float/double
    b->InitArg(BASE_SIZE + i, Store::IntToWord(0));
  // Create method closures:
  RuntimeConstantPool *runtimeConstantPool =
    classInfo->GetRuntimeConstantPool();
  nStaticMethods = 0, nVirtualMethods = 0;
  for (i = methods->GetCount(); i--; ) {
    //--** bind native methods
    MethodInfo *methodInfo = MethodInfo::FromWordDirect(methods->Get(i));
    JavaByteCode *byteCode = methodInfo->GetByteCode();
    if (byteCode != INVALID_POINTER) {
      Closure *closure = Closure::New(byteCode->ToWord(), 1);
      closure->Init(0, runtimeConstantPool->ToWord());
      if (methodInfo->IsStatic()) {
	b->InitArg(BASE_SIZE + nStaticFields + nStaticMethods,
		   closure->ToWord());
	nStaticMethods++;
      } else {
	virtualTable->InitArg(nSuperVirtualMethods + nVirtualMethods,
			      closure->ToWord());
	nVirtualMethods++;
      }
    } else {
      if (methodInfo->IsStatic())
	nStaticMethods++;
      else
	nVirtualMethods++;
    }
  }
  return static_cast<Class *>(b);
}

Class *ClassInfo::Prepare() {
  return Class::New(this);
}
