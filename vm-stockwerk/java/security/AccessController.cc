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

#include "generic/RootSet.hh"
#include "java/ClassLoader.hh"
#include "java/Authoring.hh"

//static word wPrivilegedActionRunMethodRef;

DEFINE1(doPrivileged) {
  //--**
  RETURN(null);
} END

void NativeMethodTable::java_security_AccessController(JavaString *className) {
  /*
  ClassLoader *classLoader = ClassLoader::GetBootstrapClassLoader();
  JavaString *privilegedActionName =
    JavaString::New("java/security/PrivilegedAction");
  word wPrivilegedAction = classLoader->ResolveClass(privilegedActionName);
  wPrivilegedActionRunMethodRef = classLoader->ResolveInterfaceMethodRef
    (wPrivilegedAction, JavaString::New("run"),
     JavaString::New("()Ljava/lang/Object;"));
  RootSet::Add(wPrivilegedActionRunMethodRef);
  */
  Register(className, "doPrivileged",
	   "(Ljava/security/PrivilegedAction;)Ljava/lang/Object;",
	   doPrivileged, 1, false);
}
