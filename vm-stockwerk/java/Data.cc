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

Class *Class::New(ClassInfo *classInfo) {
  // Precondition: parent class has already been created
  word wSuper = classInfo->GetSuper();
  Class *super = wSuper == Store::IntToWord(0)?
    INVALID_POINTER: Class::FromWord(wSuper);
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
  u_int nSuperVirtualMethods = super == INVALID_POINTER? 0:
    super->GetNumberOfVirtualMethods();
  u_int nStaticMethods = 0, nVirtualMethods = nSuperVirtualMethods;
  u_int nMethods = methods->GetCount();
  for (i = nMethods; i--; ) {
    MethodInfo *methodInfo = MethodInfo::FromWordDirect(methods->Get(i));
    if (methodInfo->IsStatic())
      nStaticMethods++;
    else
      nVirtualMethods++;
  }
  // Construct virtual table:
  Block *virtualTable =
    Store::AllocBlock(JavaLabel::VirtualTable, nVirtualMethods);
  Block *superVirtualTable = super == INVALID_POINTER? INVALID_POINTER:
    super->GetVirtualTable();
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
  i = 0, nStaticMethods = 0, nVirtualMethods = 0;
  while (i < nMethods) {
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
      //--** bind native methods
      if (methodInfo->IsStatic())
	nStaticMethods++;
      else
	nVirtualMethods++;
    }
    i++;
  }
  return static_cast<Class *>(b);
}

//--** these should be defined in Data.hh
ClassInfo *Class::GetClassInfo() {
  return ClassInfo::FromWordDirect(GetArg(CLASS_INFO_POS));
}

bool Class::IsInterface() {
  return GetClassInfo()->IsInterface();
}

Class *Class::GetSuperClass() {
  word wSuper = GetClassInfo()->GetSuper();
  if (wSuper == Store::IntToWord(0)) return INVALID_POINTER;
  Class *super = Class::FromWord(wSuper);
  Assert(super != INVALID_POINTER);
  return super;
}
