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
  //--** return class object
  RETURN(theClass->ToWord());
} END

DEFINE1(getPrimitiveClass) {
  DECLARE_JAVA_STRING(name, x0);
  //--** return class object
  RETURN(null); //--**
} END

void NativeMethodTable::java_lang_Class(JavaString *className) {
  Register(className, "registerNatives", "()V", registerNatives, 0, false);
  Register(className, "forName0",
	   "(Ljava/lang/String;ZLjava/lang/ClassLoader;)Ljava/lang/Class;",
	   forName0, 3, false);
  //--** isInstance
  //--** isAssignableFrom
  //--** isInterface
  //--** isArray
  //--** isPrimitive
  //--** getName
  //--** getClassLoader0
  //--** getSuperclass
  //--** getInterfaces
  //--** getComponentType
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
