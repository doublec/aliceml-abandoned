//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus and Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __JAVA__CLASS_INFO_HH__
#define __JAVA__CLASS_INFO_HH__

#if defined(INTERFACE)
#pragma interface "java/ClassInfo.hh"
#endif

#include "java/Data.hh"

class DllExport Array: private Block {
protected:
  enum {
    SIZE_POS, // int
    BASE_SIZE
    // ... elements
  };
public:
  using Block::ToWord;

  static Array *New(u_int length) {
    Block *b = Store::AllocBlock(JavaLabel::Array, BASE_SIZE + length);
    b->InitArg(SIZE_POS, Store::IntToWord(length));
    for (u_int i = length; i--; ) b->InitArg(BASE_SIZE + i, null);
    return static_cast<Array *>(b);
  }
  static Array *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == JavaLabel::Array);
    return static_cast<Array *>(b);
  }
  static Array *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == JavaLabel::Array);
    return static_cast<Array *>(b);
  }

  void Init(u_int index, word value) {
    Assert(index < Store::DirectWordToInt(GetArg(SIZE_POS)));
    InitArg(BASE_SIZE + index, value);
  }
  void Assign(u_int index, word value) {
    Assert(index < Store::DirectWordToInt(GetArg(SIZE_POS)));
    ReplaceArg(BASE_SIZE + index, value);
  }
  word Get(u_int index) {
    Assert(index < Store::DirectWordToInt(GetArg(SIZE_POS)));
    return GetArg(BASE_SIZE + index);
  }
};

class DllExport FieldInfo: private Block {
public:
  enum access_flags {
    ACC_PUBLIC    = 0x0001,
    ACC_PRIVATE   = 0x0002,
    ACC_PROTECTED = 0x0004,
    ACC_STATIC    = 0x0008,
    ACC_FINAL     = 0x0010,
    ACC_VOLATILE  = 0x0040,
    ACC_TRANSIENT = 0x0080
  };
protected:
  enum {
    ACCESS_FLAGS_POS, // access_flags
    NAME_POS, // JavaString
    DESCRIPTOR_POS, // JavaString
    HAS_CONSTANT_VALUE_POS, // bool
    CONSTANT_VALUE_POS, // word
    SIZE
  };
private:
  static FieldInfo *NewInternal(u_int accessFlags, JavaString *name,
				JavaString *descriptor) {
    Assert(((accessFlags & ACC_PUBLIC) != 0) +
	   ((accessFlags & ACC_PRIVATE) != 0) +
	   ((accessFlags & ACC_PROTECTED) != 0) <= 1);
    Assert(((accessFlags & ACC_FINAL) != 0) +
	   ((accessFlags & ACC_VOLATILE) != 0) <= 1);
    Block *b = Store::AllocBlock(JavaLabel::FieldInfo, SIZE);
    b->InitArg(ACCESS_FLAGS_POS, accessFlags);
    b->InitArg(NAME_POS, name->ToWord());
    b->InitArg(DESCRIPTOR_POS, descriptor->ToWord());
    return static_cast<FieldInfo *>(b);
  }
public:
  using Block::ToWord;

  static FieldInfo *New(u_int accessFlags, JavaString *name,
			JavaString *descriptor) {
    FieldInfo *fieldInfo = NewInternal(accessFlags, name, descriptor);
    fieldInfo->InitArg(HAS_CONSTANT_VALUE_POS, false);
    return fieldInfo;
  }
  static FieldInfo *New(u_int accessFlags, JavaString *name,
			JavaString *descriptor, word constantValue) {
    FieldInfo *fieldInfo = NewInternal(accessFlags, name, descriptor);
    fieldInfo->InitArg(HAS_CONSTANT_VALUE_POS, true);
    fieldInfo->InitArg(CONSTANT_VALUE_POS, constantValue);
    return fieldInfo;
  }
};

class DllExport ExceptionTableEntry: private Block {
protected:
  enum {
    START_PC_POS, // int
    END_PC_POS, // int
    HANDLER_PC_POS, // int
    CATCH_TYPE_POS, // Future(Class) | int(0)
    SIZE
  };
private:
  static ExceptionTableEntry *NewInternal(u_int startPC, u_int endPC,
					  u_int handlerPC) {
    Block *b = Store::AllocBlock(JavaLabel::ExceptionTableEntry, SIZE);
    b->InitArg(START_PC_POS, startPC);
    b->InitArg(END_PC_POS, endPC);
    b->InitArg(HANDLER_PC_POS, handlerPC);
    return static_cast<ExceptionTableEntry *>(b);
  }
public:
  using Block::ToWord;

  static ExceptionTableEntry *New(u_int startPC, u_int endPC,
				  u_int handlerPC) {
    ExceptionTableEntry *entry = NewInternal(startPC, endPC, handlerPC);
    entry->InitArg(CATCH_TYPE_POS, 0);
    return entry;
  }
  static ExceptionTableEntry *New(u_int startPC, u_int endPC,
				  u_int handlerPC, word catchType) {
    ExceptionTableEntry *entry = NewInternal(startPC, endPC, handlerPC);
    entry->InitArg(CATCH_TYPE_POS, catchType);
    return entry;
  }
};

class DllExport JavaByteCode: private Block {
protected:
  enum {
    MAX_STACK_POS, // int
    MAX_LOCALS_POS, // int
    CODE_POS, // Chunk
    EXCEPTION_TABLE_POS, // Array(ExceptionTableEntry)
    SIZE
  };
public:
  using Block::ToWord;

  static JavaByteCode *New(u_int maxStack, u_int maxLocals, Chunk *code,
			   Array *exceptionTable) {
    Block *b = Store::AllocBlock(JavaLabel::JavaByteCode, SIZE);
    b->InitArg(MAX_STACK_POS, maxStack);
    b->InitArg(MAX_LOCALS_POS, maxLocals);
    b->InitArg(CODE_POS, code->ToWord());
    b->InitArg(EXCEPTION_TABLE_POS, exceptionTable->ToWord());
    return static_cast<JavaByteCode *>(b);
  }
};

class DllExport MethodInfo: private Block {
public:
  enum access_flags {
    ACC_PUBLIC       = 0x0001,
    ACC_PRIVATE      = 0x0002,
    ACC_PROTECTED    = 0x0004,
    ACC_STATIC       = 0x0008,
    ACC_FINAL        = 0x0010,
    ACC_SYNCHRONIZED = 0x0020,
    ACC_NATIVE       = 0x0100,
    ACC_ABSTRACT     = 0x0400,
    ACC_STRICT       = 0x0800
  };
protected:
  enum {
    ACCESS_FLAGS_POS, // access_flags
    NAME_POS, // JavaString
    DESCRIPTOR_POS, // JavaString
    BYTE_CODE_POS, // JavaByteCode | int(0)
    SIZE
  };
private:
  static MethodInfo *NewInternal(u_int accessFlags, JavaString *name,
				 JavaString *descriptor) {
    Assert(((accessFlags & ACC_PUBLIC) != 0) +
	   ((accessFlags & ACC_PRIVATE) != 0) +
	   ((accessFlags & ACC_PROTECTED) != 0) <= 1);
    Assert((accessFlags & ACC_ABSTRACT) == 0 ||
	   (accessFlags & (ACC_FINAL|ACC_NATIVE|ACC_PRIVATE|ACC_STATIC|
			   ACC_STRICT|ACC_SYNCHRONIZED)) == 0);
    Block *b = Store::AllocBlock(JavaLabel::FieldInfo, SIZE);
    b->InitArg(ACCESS_FLAGS_POS, accessFlags);
    b->InitArg(NAME_POS, name->ToWord());
    b->InitArg(DESCRIPTOR_POS, descriptor->ToWord());
    return static_cast<MethodInfo *>(b);
  }
public:
  using Block::ToWord;

  static MethodInfo *New(u_int accessFlags, JavaString *name,
			 JavaString *descriptor) {
    MethodInfo *methodInfo = NewInternal(accessFlags, name, descriptor);
    methodInfo->InitArg(BYTE_CODE_POS, 0);
    return methodInfo;
  }
  static MethodInfo *New(u_int accessFlags, JavaString *name,
			 JavaString *descriptor, JavaByteCode *byteCode) {
    MethodInfo *methodInfo = NewInternal(accessFlags, name, descriptor);
    methodInfo->InitArg(BYTE_CODE_POS, byteCode->ToWord());
    return methodInfo;
  }
};

class DllExport ClassInfo: private Block {
public:
  enum access_flags {
    ACC_PUBLIC    = 0x0001,
    ACC_FINAL     = 0x0010,
    ACC_SUPER     = 0x0020,
    ACC_INTERFACE = 0x0200,
    ACC_ABSTRACT  = 0x0400
  };
protected:
  enum {
    ACCESS_FLAGS_POS, // access_flags
    NAME_POS, // JavaString
    SUPER_POS, // Class
    INTERFACES_POS, // Array(Class)
    FIELDS_POS, // Array(FieldInfo)
    METHODS_POS, // Array(MethodInfo)
    CONSTANT_POOL_POS, // Array(word)
    SIZE
  };
public:
  using Block::ToWord;

  static ClassInfo *New(u_int accessFlags, JavaString *name,
			word super, Array *interfaces, Array *fields,
			Array *methods, Array *constantPool) {
    Assert(((accessFlags & ACC_INTERFACE) == 0 &&
	    ((accessFlags & ACC_FINAL) != 0) +
	    ((accessFlags & ACC_ABSTRACT) != 0) <= 1) ||
	   (accessFlags & ACC_ABSTRACT) != 0);
    Assert(accessFlags & ACC_SUPER); // not supported by this implementation
    Block *b = Store::AllocBlock(JavaLabel::ClassInfo, SIZE);
    b->InitArg(ACCESS_FLAGS_POS, accessFlags);
    b->InitArg(NAME_POS, name->ToWord());
    b->InitArg(SUPER_POS, super);
    b->InitArg(INTERFACES_POS, interfaces->ToWord());
    b->InitArg(FIELDS_POS, fields->ToWord());
    b->InitArg(METHODS_POS, methods->ToWord());
    b->InitArg(CONSTANT_POOL_POS, constantPool->ToWord());
    return static_cast<ClassInfo *>(b);
  }
  static ClassInfo *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == JavaLabel::ClassInfo);
    return static_cast<ClassInfo *>(b);
  }
};

#endif
