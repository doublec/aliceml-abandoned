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
#pragma implementation "java/Data.hh"
#endif

#include "java/Data.hh"

ExceptionTableEntry *ExceptionTableEntry::New(u_int startPC, u_int endPC,
					      u_int handlerPC,
					      ClassInfo *catchType) {
    ExceptionTableEntry *entry = NewInternal(startPC, endPC, handlerPC);
    entry->InitArg(CATCH_TYPE_POS, catchType->ToWord());
    return entry;
}

ClassInfo *ClassInfo::New(u_int accessFlags, JavaString *name,
			  ClassInfo *super, Array *interfaces, Array *fields,
			  Array *methods, ConstantPool *constantPool) {
  Assert(((accessFlags & ACC_INTERFACE) == 0 &&
	  ((accessFlags & ACC_FINAL) != 0) +
	  ((accessFlags & ACC_ABSTRACT) != 0) <= 1) ||
	 (accessFlags & ACC_ABSTRACT) != 0);
  Assert(accessFlags & ACC_SUPER); // else not supported by this implementation
  Block *b = Store::AllocBlock(JavaLabel::ClassInfo, SIZE);
  b->InitArg(ACCESS_FLAGS_POS, accessFlags);
  b->InitArg(NAME_POS, name->ToWord());
  b->InitArg(SUPER_POS, super->ToWord());
  b->InitArg(INTERFACES_POS, interfaces->ToWord());
  b->InitArg(FIELDS_POS, fields->ToWord());
  b->InitArg(METHODS_POS, methods->ToWord());
  b->InitArg(CONSTANT_POOL_POS, constantPool->ToWord());
  return static_cast<ClassInfo *>(b);
}
