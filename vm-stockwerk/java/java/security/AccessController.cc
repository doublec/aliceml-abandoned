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

static word wPrivilegedActionRunMethodRef;

DEFINE1(doPrivileged) {
  DECLARE_OBJECT(object, x0);
  Assert(object != INVALID_POINTER);
  InterfaceMethodRef *methodRef =
    InterfaceMethodRef::FromWord(wPrivilegedActionRunMethodRef);
  if (methodRef == INVALID_POINTER) REQUEST(wPrivilegedActionRunMethodRef);
  Closure *closure = object->GetClass()->
    GetInterfaceMethod(methodRef->GetClass(), methodRef->GetIndex());
  Assert(closure != INVALID_POINTER);
  return Scheduler::PushCall(closure->ToWord());
} END

void NativeMethodTable::java_security_AccessController(JavaString *className) {
  ClassLoader *classLoader = ClassLoader::GetBootstrapClassLoader();
  JavaString *privilegedActionName =
    JavaString::New("java/security/PrivilegedAction");
  word wPrivilegedAction = classLoader->ResolveClass(privilegedActionName);
  wPrivilegedActionRunMethodRef = classLoader->ResolveInterfaceMethodRef
    (wPrivilegedAction, JavaString::New("run"),
     JavaString::New("()Ljava/lang/Object;"));
  RootSet::Add(wPrivilegedActionRunMethodRef);
  Register(className, "doPrivileged",
	   "(Ljava/security/PrivilegedAction;)Ljava/lang/Object;",
	   doPrivileged, 1, false);
}
