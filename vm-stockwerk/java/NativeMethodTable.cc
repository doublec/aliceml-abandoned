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
#pragma implementation "java/NativeMethodTable.hh"
#endif

#include "java/NativeMethodTable.hh"

static word MakeKey(JavaString *className, JavaString *name,
		    JavaString *descriptor) {
  JavaString *s = className->Concat("#")->Concat(name)->Concat(descriptor);
  return s->ToArray()->ToWord();
}

word NativeMethodTable::wTable;

void NativeMethodTable::Init() {
  wTable = ChunkMap::New(initialSize)->ToWord();
  RootSet::Add(wTable);
  Dump(JavaString::New("Dump"));
  java_lang_Class(JavaString::New("java/lang/Class"));
  java_lang_Object(JavaString::New("java/lang/Object"));
  java_lang_String(JavaString::New("java/lang/String"));
  java_lang_Throwable(JavaString::New("java/lang/Throwable"));
  java_lang_System(JavaString::New("java/lang/System"));
  java_lang_ClassLoader(JavaString::New("java/lang/ClassLoader"));
  java_lang_Float(JavaString::New("java/lang/Float"));
  java_lang_Double(JavaString::New("java/lang/Double"));
  java_lang_StrictMath(JavaString::New("java/lang/StrictMath"));
  java_lang_Thread(JavaString::New("java/lang/Thread"));
  java_io_FileDescriptor(JavaString::New("java/io/FileDescriptor"));
  java_io_FileInputStream(JavaString::New("java/io/FileInputStream"));
  java_io_FileOutputStream(JavaString::New("java/io/FileOutputStream"));
  java_io_ObjectStreamClass(JavaString::New("java/io/ObjectStreamClass"));
  java_security_AccessController
    (JavaString::New("java/security/AccessController"));
  sun_misc_Unsafe(JavaString::New("sun/misc/Unsafe"));
  sun_misc_AtomicLong(JavaString::New("sun/misc/AtomicLong"));
  sun_reflect_Reflection(JavaString::New("sun/reflect/Reflection"));
  sun_reflect_NativeConstructorAccessorImpl
    (JavaString::New("sun/reflect/NativeConstructorAccessorImpl"));
}

void NativeMethodTable::Register(JavaString *className, JavaString *name,
				 JavaString *descriptor, Closure *closure,
				 bool /*--** isVirtual */) {
  ChunkMap *table = ChunkMap::FromWordDirect(wTable);
  word key = MakeKey(className, name, descriptor);
  Assert(!table->IsMember(key));
  table->Put(key, closure->ToWord());
}

void NativeMethodTable::Register(JavaString *className, const char *name,
				 const char *descriptor, Closure *closure,
				 bool isVirtual) {
  Register(className, JavaString::New(name),
	   JavaString::New(descriptor), closure, isVirtual);
}

void NativeMethodTable::Register(JavaString *className, const char *name,
				 const char *descriptor,
				 Interpreter::function value, u_int arity,
				 bool isVirtual) {
  //--** support abstract representations
  word function =
    Primitive::MakeFunction(name, value, arity, INVALID_POINTER);
  Closure *closure = Closure::New(function, 0);
  Register(className, name, descriptor, closure, isVirtual);
}

Closure *NativeMethodTable::Lookup(JavaString *className, JavaString *name,
				   JavaString *descriptor) {
  ChunkMap *table = ChunkMap::FromWordDirect(wTable);
  word key = MakeKey(className, name, descriptor);
  if (!table->IsMember(key)) return INVALID_POINTER;
  return Closure::FromWordDirect(table->Get(key));
}
