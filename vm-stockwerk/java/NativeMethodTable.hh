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

#ifndef __JAVA__NATIVE_METHOD_TABLE_HH__
#define __JAVA__NATIVE_METHOD_TABLE_HH__

#if defined(INTERFACE)
#pragma interface "java/NativeMethodTable.hh"
#endif

#include "java/Data.hh"

class JavaDll NativeMethodTable {
private:
  static const u_int initialSize = 19;
  static word wTable;

  static void Register(JavaString *className, JavaString *name,
		       JavaString *descriptor, Closure *closure,
		       bool isVirtual);
  static void Register(JavaString *className, const char *name,
		       const char *descriptor, Closure *closure,
		       bool isVirtual);
  static void Register(JavaString *className, const char *name,
		       const char *descriptor,
		       Interpreter::function value, u_int arity,
		       bool isVirtual);

  static void Dump(JavaString *className); //--** for testing
  static void java_lang_Class(JavaString *className);
  static void java_lang_Object(JavaString *className);
  static void java_lang_String(JavaString *className);
  static void java_lang_Throwable(JavaString *className);
  static void java_lang_System(JavaString *className);
  static void java_lang_ClassLoader(JavaString *className);
  static void java_lang_Float(JavaString *className);
  static void java_lang_Double(JavaString *className);
  static void java_lang_StrictMath(JavaString *className);
  static void java_lang_Thread(JavaString *className);
  static void java_io_FileDescriptor(JavaString *className);
  static void java_io_FileInputStream(JavaString *className);
  static void java_io_FileOutputStream(JavaString *className);
  static void java_io_ObjectStreamClass(JavaString *className);
  static void java_security_AccessController(JavaString *className);
  static void sun_misc_Unsafe(JavaString *className);
  static void sun_misc_AtomicLong(JavaString *className);
  static void sun_reflect_Reflection(JavaString *className);
  static void sun_reflect_NativeConstructorAccessorImpl(JavaString *className);
public:
  static void Init();

  static Closure *Lookup(JavaString *className, JavaString *name,
			 JavaString *descriptor);
};

#endif
