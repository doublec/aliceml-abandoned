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

#ifndef __JAVA__AUTHORING_HH__
#define __JAVA__AUTHORING_HH__

#include "java/Data.hh"
#include "java/ThrowWorker.hh"
#include "java/NativeMethodTable.hh"

#define BOOL_TO_WORD(b) JavaInt::ToWord((b)? 1: 0)

#define DECLARE_JINT(i, x)				\
  s_int32 i;						\
  {							\
    Transient *transient = Store::WordToTransient(x);	\
    if (transient != INVALID_POINTER) { REQUEST(x); }	\
    i = JavaInt::FromWord(x);				\
  }
#define DECLARE_BOOL(b, x)				\
  bool b;						\
  {							\
    Transient *transient = Store::WordToTransient(x);	\
    if (transient != INVALID_POINTER) { REQUEST(x); }	\
    b = JavaInt::FromWord(x) != 0;			\
  }

#define DECLARE_BLOCKTYPE_OR_NULL(t, a, x)		\
  t *a = t::FromWord(x);				\
  if (a == INVALID_POINTER) {				\
    if (Store::WordToInt(x) != 0) { REQUEST(x); }	\
  } else {}

#define DECLARE_LONG(javaLong, x) DECLARE_BLOCKTYPE(JavaLong, javaLong, x)
#define DECLARE_OBJECT(object, x) DECLARE_BLOCKTYPE_OR_NULL(Object, object, x)
#define DECLARE_JAVA_STRING(string, x) \
  DECLARE_BLOCKTYPE_OR_NULL(JavaString, string, x)
#define DECLARE_AARRAY(array, x) \
  DECLARE_BLOCKTYPE_OR_NULL(ObjectArray, array, x)
#define DECLARE_CLASS_LOADER(classLoader, x) \
  DECLARE_BLOCKTYPE_OR_NULL(ClassLoader, classLoader, x)

#define RETURN_VOID RETURN0
#define RETURN_JINT(i) RETURN(JavaInt::ToWord(i))
#define RETURN_BOOL(b) RETURN(BOOL_TO_WORD(b))
#define DRETURN(x) RETURN2(x, null)

#define THROW(Class, message) {						\
  PUSH_PRIM_SELF();				\
  ThrowWorker::PushFrame(ThrowWorker::Class, JavaString::New(message));	\
  RETURN_VOID;								\
}

#endif
