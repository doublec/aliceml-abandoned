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

#include "generic/Authoring.hh"
#include "java/Data.hh"
#include "java/ThrowWorker.hh"
#include "java/NativeMethodTable.hh"

#define BOOL_TO_WORD(b) Store::IntToWord((b)? 1: 0)

#define DECLARE_BOOL(b, x)					\
  bool b;							\
  {								\
    s_int i = Store::WordToInt(x);				\
    if (i == INVALID_INT) { REQUEST(x); } else b = i != 0;	\
  }

#define DECLARE_LONG(javaLong, x) DECLARE_BLOCKTYPE(JavaLong, javaLong, x)
#define DECLARE_OBJECT(object, x) DECLARE_BLOCKTYPE(Object, object, x)
#define DECLARE_JAVA_STRING(string, x) DECLARE_BLOCKTYPE(JavaString, string, x)
#define DECLARE_AARRAY(array, x) DECLARE_BLOCKTYPE(ObjectArray, array, x)

#define RETURN_VOID RETURN0

#define THROW(Class, message) {						\
  Scheduler::PushFrameNoCheck(prim_self);				\
  ThrowWorker::PushFrame(ThrowWorker::Class, JavaString::New(message));	\
  RETURN_VOID;								\
}

#define DRETURN(x) RETURN2(x, null)

#endif
