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

#include "java/ClassLoader.hh"
#include "java/Authoring.hh"

DEFINE0(registerNatives) {
  RETURN_VOID;
} END

DEFINE3(forName0) {
  DECLARE_JAVA_STRING(name, x0);
  DECLARE_BOOL(initialize, x1);
  DECLARE_CLASS_LOADER(classLoader, x2);
  u_int length = name->GetLength();
  BaseArray *a = BaseArray::New(PrimitiveType::Char, length);
  for (u_int i = length; i--; ) {
    u_wchar c = name->CharAt(i);
    if (c == '.') c = '/';
    a->StoreChar(i, c);
  }
  JavaString *internalName = JavaString::New(a, 0, length);
  if (classLoader == INVALID_POINTER)
    classLoader = ClassLoader::GetBootstrapClassLoader();
  word wClass = classLoader->ResolveClass(internalName);
  Class *theClass = Class::FromWord(wClass);
  if (theClass == INVALID_POINTER) REQUEST(wClass);
  if (!theClass->IsInitialized() && initialize) {
    Scheduler::PushFrameNoCheck(prim_self);
    return theClass->RunInitializer();
  }
  RETURN(theClass->GetClassObject()->ToWord());
} END

DEFINE1(isInterface) {
  DECLARE_OBJECT(_this, x0);
  ClassObject *classObject = static_cast<ClassObject *>(_this);
  Type *type = classObject->GetRepresentedType();
  RETURN_BOOL(type->GetLabel() == JavaLabel::Class &&
	      static_cast<Class *>(type)->IsInterface());
} END

DEFINE1(isArray) {
  DECLARE_OBJECT(_this, x0);
  ClassObject *classObject = static_cast<ClassObject *>(_this);
  Type *type = classObject->GetRepresentedType();
  if (type->GetLabel() == JavaLabel::ArrayType) {
    RETURN_BOOL(true);
  } else {
    Assert(type->GetLabel() == JavaLabel::Class ||
	   type->GetLabel() == JavaLabel::PrimitiveType);
    RETURN_BOOL(false);
  }
} END

DEFINE1(isPrimitive) {
  DECLARE_OBJECT(_this, x0);
  ClassObject *classObject = static_cast<ClassObject *>(_this);
  Type *type = classObject->GetRepresentedType();
  if (type->GetLabel() == JavaLabel::PrimitiveType) {
    RETURN_BOOL(true);
  } else {
    Assert(type->GetLabel() == JavaLabel::Class ||
	   type->GetLabel() == JavaLabel::ArrayType);
    RETURN_BOOL(false);
  }
} END

DEFINE1(getClassLoader0) {
  DECLARE_OBJECT(_this, x0);
  RETURN(null); //--**
} END

DEFINE1(getComponentType) {
  DECLARE_OBJECT(_this, x0);
  ClassObject *classObject = static_cast<ClassObject *>(_this);
  Type *type = classObject->GetRepresentedType();
  switch (type->GetLabel()) {
  case JavaLabel::Class:
  case JavaLabel::PrimitiveType:
    RETURN(null);
  case JavaLabel::ArrayType:
    RETURN(static_cast<ArrayType *>(type)->GetElementType()->GetClassObject()->
	   ToWord());
  default:
    Error("illegal type");
  }
} END

DEFINE1(getPrimitiveClass) {
  DECLARE_JAVA_STRING(name, x0);
  if (name->Equals("boolean")) {
    RETURN(PrimitiveType::GetClassObject(PrimitiveType::Boolean)->ToWord());
  } else if (name->Equals("byte")) {
    RETURN(PrimitiveType::GetClassObject(PrimitiveType::Byte)->ToWord());
  } else if (name->Equals("char")) {
    RETURN(PrimitiveType::GetClassObject(PrimitiveType::Char)->ToWord());
  } else if (name->Equals("double")) {
    RETURN(PrimitiveType::GetClassObject(PrimitiveType::Double)->ToWord());
  } else if (name->Equals("float")) {
    RETURN(PrimitiveType::GetClassObject(PrimitiveType::Float)->ToWord());
  } else if (name->Equals("int")) {
    RETURN(PrimitiveType::GetClassObject(PrimitiveType::Int)->ToWord());
  } else if (name->Equals("long")) {
    RETURN(PrimitiveType::GetClassObject(PrimitiveType::Long)->ToWord());
  } else if (name->Equals("short")) {
    RETURN(PrimitiveType::GetClassObject(PrimitiveType::Short)->ToWord());
  } else if (name->Equals("void")) {
    RETURN(PrimitiveType::GetClassObject(PrimitiveType::Void)->ToWord());
  } else {
    Error("unknown primitive class");
  }
} END

void NativeMethodTable::java_lang_Class(JavaString *className) {
  Register(className, "registerNatives", "()V", registerNatives, 0, false);
  Register(className, "forName0",
	   "(Ljava/lang/String;ZLjava/lang/ClassLoader;)Ljava/lang/Class;",
	   forName0, 3, false);
  //--** isInstance
  //--** isAssignableFrom
  Register(className, "isInterface", "()Z", isInterface, 1, true);
  Register(className, "isArray", "()Z", isArray, 1, true);
  Register(className, "isPrimitive", "()Z", isPrimitive, 1, true);
  //--** getName
  Register(className, "getClassLoader0", "()Ljava/lang/ClassLoader;",
	   getClassLoader0, 1, true);
  //--** getClassLoader0
  //--** getSuperclass
  //--** getInterfaces
  Register(className, "getComponentType", "()Ljava/lang/Class;",
	   getComponentType, 1, true);
  //--** getModifiers
  //--** getSigners
  //--** setSigners
  //--** getDeclaringClass
  //--** getProtectionDomain0
  //--** setProtectionDomain0
  Register(className, "getPrimitiveClass",
	   "(Ljava/lang/String;)Ljava/lang/Class;",
	   getPrimitiveClass, 1, false);
  //--** getDeclaredFields0
  //--** getDeclaredMethods0
  //--** getDeclaredConstructors0
  //--** getDeclaredClasses0
  //--** desiredAssertionStatus0
}
