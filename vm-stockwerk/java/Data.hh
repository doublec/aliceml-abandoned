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

#ifndef __JAVA__DATA_HH__
#define __JAVA__DATA_HH__

#if defined(INTERFACE)
#pragma interface "java/Data.hh"
#endif

#include <cstring>
#include "generic/Closure.hh"

typedef unsigned short u_wchar; //--**

class JavaLabel {
public:
  static const BlockLabel Array               = MIN_DATA_LABEL;
  static const BlockLabel JavaArray           = (BlockLabel) (Array + 1);
  static const BlockLabel Lock                = (BlockLabel) (Array + 2);
  static const BlockLabel FieldInfo           = (BlockLabel) (Array + 3);
  static const BlockLabel ExceptionTableEntry = (BlockLabel) (Array + 4);
  static const BlockLabel JavaByteCode        = (BlockLabel) (Array + 5);
  static const BlockLabel MethodInfo          = (BlockLabel) (Array + 6);
  static const BlockLabel ClassInfo           = (BlockLabel) (Array + 7);
  static const BlockLabel Class               = (BlockLabel) (Array + 8);
  static const BlockLabel Object              = (BlockLabel) (Array + 9);
  static const BlockLabel ConstantPool        = (BlockLabel) (Array + 10);
};

static const word null = Store::IntToWord(0);

class ClassInfo;
class Class;
class ConstantPool;

class DllExport JavaString: private Chunk {
public:
  using Chunk::ToWord;

  u_wchar *GetBase() {
    return reinterpret_cast<u_wchar *>(Chunk::GetBase());
  }
  u_int GetLength() {
    return GetSize() / sizeof(u_wchar);
  }

  static JavaString *New(u_int length) {
    return static_cast<JavaString *>
      (Store::AllocChunk(sizeof(u_wchar) * length));
  }
  static JavaString *New(const u_wchar *s, u_int length) {
    JavaString *string = New(length);
    std::memcpy(string->GetBase(), s, length * sizeof(u_wchar));
    return string;
  }
  static JavaString *New(const char *s, u_int length) {
    JavaString *string = New(length);
    u_wchar *p = string->GetBase();
    for (u_int i = 0; i < length; i++)
      p[i] = s[i];
    return string;
  }
  static JavaString *New(const char *s) {
    return New(s, std::strlen(s));
  }
  static JavaString *FromWord(word x) {
    return static_cast<JavaString *>(Chunk::FromWord(x));
  }
  static JavaString *FromWordDirect(word x) {
    return static_cast<JavaString *>(Chunk::FromWordDirect(x));
  }

  bool Equals(JavaString *string) {
    u_int length = GetLength();
    if (string->GetLength() != length) return false;
    return !std::memcmp(GetBase(), string->GetBase(), 
			length * sizeof(u_wchar));
  }
};

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

class DllExport JavaArray: private Block {
protected:
  enum {
    CLASS_POS, // Class
    SIZE_POS, // int
    BASE_SIZE
    // ... elements
  };
public:
  static JavaArray *New(word type, u_int length) {
    //--** array representation depends on type
    Block *b = Store::AllocBlock(JavaLabel::JavaArray, BASE_SIZE + length);
    b->InitArg(CLASS_POS, type);
    b->InitArg(SIZE_POS, Store::IntToWord(length));
    for (u_int i = length; i--; ) b->InitArg(BASE_SIZE + i, null);
    return static_cast<JavaArray *>(b);
  }
  static JavaArray *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == JavaLabel::JavaArray);
    return static_cast<JavaArray *>(b);
  }
  static JavaArray *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == JavaLabel::JavaArray);
    return static_cast<JavaArray *>(b);
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

class DllExport Lock: private Block {
  // to be determined
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
    CATCH_TYPE_POS, // Class | int(0)
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
				  u_int handlerPC, ClassInfo *catchType);
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
    CONSTANT_POOL_POS, // ConstantPool
    SIZE
  };
public:
  using Block::ToWord;

  static ClassInfo *New(u_int accessFlags, JavaString *name,
			ClassInfo *super, Array *interfaces, Array *fields,
			Array *methods, ConstantPool *constantPool);
  static ClassInfo *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == JavaLabel::ClassInfo);
    return static_cast<ClassInfo *>(b);
  }

  bool Verify();
  Class *Prepare();
};

class DllExport Class: private Block {
protected:
  enum {
    CLASS_INFO_POS, // ClassInfo
    VIRTUAL_TABLE_POS, // Block(Closure ... Closure)
    LOCK_POS,
    INITIALIZATION_THREAD_POS, // Thread | int(0)
    BASE_SIZE
    // ... static fields
    // ... static methods
  };
public:
  word GetStaticField(u_int index);
  Closure *GetStaticMethod(u_int index);
};

class DllExport Object: private Block {
protected:
  enum {
    CLASS_POS, // Class
    BASE_SIZE
    // ... instance fields
  };
public:
  word GetInstanceField(u_int index);
  Closure *GetVirtualMethod(u_int index);
};

//
// Constant Pool Entries: (Formerly Symbolic) References
//

class DllExport StaticFieldRef: private Block {
public:
  Class *GetClass();
  u_int GetIndex();
};

class DllExport InstanceFieldRef: private Block {
public:
  u_int GetIndex();
};

class DllExport StaticMethodRef: private Block {
private:
  enum {
    CLASS_POS, // Class
    INDEX_POS, // int
    SIZE
  };
public:
  Class *GetClass();
  u_int GetIndex();
};

class DllExport VirtualMethodRef: private Block {
private:
public:
  u_int GetIndex();
};

class DllExport ConstantPool: private Block {
private:
  enum { SIZE_POS, BASE_SIZE };
public:
  using Block::ToWord;

  static ConstantPool *New(u_int size) {
    Block *b = Store::AllocBlock(JavaLabel::ConstantPool, BASE_SIZE + size);
    b->InitArg(SIZE_POS, size);
    return static_cast<ConstantPool *>(b);
  }
  static ConstantPool *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == JavaLabel::ConstantPool);
    return static_cast<ConstantPool *>(b);
  }

  u_int GetCount() {
    return Store::DirectWordToInt(GetArg(SIZE_POS));
  }
  void Init(u_int offset, word constant) {
    Assert(offset >= 1 && offset <= GetCount());
    InitArg(offset - 1 + BASE_SIZE, constant);
  }
  word Get(u_int offset) {
    Assert(offset >= 1 && offset <= GetCount());
    return GetArg(offset - 1 + BASE_SIZE);
  }

  JavaString *GetString(u_int offset) {
    return JavaString::FromWordDirect(Get(offset));
  }
  ClassInfo *GetClassInfo(u_int offset) {
    return ClassInfo::FromWordDirect(Get(offset));
  }
};

#endif
