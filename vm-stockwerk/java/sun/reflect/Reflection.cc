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

#include "java/ClassInfo.hh"
#include "java/Authoring.hh"

DEFINE1(getCallerClass) {
  RETURN(null); //--**
} END

//--** duplicated in java/lang/Class.cc, getModifiers
static u_int GetTypeAccessFlags(Type *type) {
  switch (type->GetLabel()) {
  case JavaLabel::Class:
    return static_cast<Class *>(type)->GetClassInfo()->GetAccessFlags();
  case JavaLabel::PrimitiveType:
    // If this object represents an array class, a
    // primitive type or void, then its final modifier is always
    // true and its interface modifier is always false.
    return ClassInfo::ACC_PUBLIC | ClassInfo::ACC_FINAL;
  case JavaLabel::ArrayType:
    {
      Type *elementType = static_cast<ArrayType *>(type)->GetElementType();
      u_int accessFlags = GetTypeAccessFlags(elementType);
      // If the underlying class is an array class, then its
      // public, private and protected modifiers are the same
      // as those of its component type.
      return (accessFlags & ClassInfo::ACC_PUBLIC) | ClassInfo::ACC_FINAL;
    }
  default:
    Error("illegal type");
  }
}

DEFINE1(getClassAccessFlags) {
  DECLARE_OBJECT(object, x0);
  Assert(object != INVALID_POINTER); //--** NullPointerException
  ClassObject *classObject = static_cast<ClassObject *>(object);
  RETURN_JINT(GetTypeAccessFlags(classObject->GetRepresentedType()));
} END

void NativeMethodTable::sun_reflect_Reflection(JavaString *className) {
  Register(className, "getCallerClass", "(I)Ljava/lang/Class;",
	   getCallerClass, 1, false);
  Register(className, "getClassAccessFlags", "(Ljava/lang/Class;)I",
	   getClassAccessFlags, 1, false);
}
