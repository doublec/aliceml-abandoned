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

#include "adt/HashTable.hh"
#include "generic/RootSet.hh"
#include "generic/Interpreter.hh"
#include "generic/Closure.hh"
#include "java/Data.hh"

class DllExport NativeMethodTable {
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

  static void java_lang_Object(JavaString *className);
  static void java_lang_Double(JavaString *className);
public:
  static void Init();

  static Closure *Lookup(JavaString *className, JavaString *name,
			 JavaString *descriptor);
};

#endif
