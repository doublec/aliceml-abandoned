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
#pragma implementation "java/JavaByteCode.hh"
#endif

#include "java/JavaByteCode.hh"
#include "java/ClassInfo.hh"

JavaByteCode *JavaByteCode::New(MethodInfo *methodInfo, u_int maxStack,
				u_int maxLocals, Chunk *code,
				Table *exceptionTable) {
  ConcreteCode *concreteCode =
    ConcreteCode::New(ByteCodeInterpreter::self, SIZE);
  concreteCode->Init(METHOD_INFO_POS, methodInfo->ToWord());
  concreteCode->Init(MAX_STACK_POS, Store::IntToWord(maxStack));
  concreteCode->Init(MAX_LOCALS_POS, Store::IntToWord(maxLocals));
  concreteCode->Init(CODE_POS, code->ToWord());
  concreteCode->Init(EXCEPTION_TABLE_POS, exceptionTable->ToWord());
  return static_cast<JavaByteCode *>(concreteCode);
}
