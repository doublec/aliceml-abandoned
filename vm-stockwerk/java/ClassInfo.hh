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
#include "java/JavaByteCode.hh"
#include "java/ClassLoader.hh"

class JavaDll FieldInfo: private Block {
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
  u_int GetAccessFlags() {
    return Store::DirectWordToInt(GetArg(ACCESS_FLAGS_POS));
  }
public:
  using Block::ToWord;

  static FieldInfo *New(u_int accessFlags, JavaString *name,
			JavaString *descriptor) {
    FieldInfo *fieldInfo = NewInternal(accessFlags, name, descriptor);
    fieldInfo->InitArg(HAS_CONSTANT_VALUE_POS, false);
    return fieldInfo;
  }
  static FieldInfo *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == JavaLabel::FieldInfo);
    return static_cast<FieldInfo *>(b);
  }

  bool IsStatic() {
    return (GetAccessFlags() & ACC_STATIC) != 0;
  }
  JavaString *GetName() {
    return JavaString::FromWordDirect(GetArg(NAME_POS));
  }
  JavaString *GetDescriptor() {
    return JavaString::FromWordDirect(GetArg(DESCRIPTOR_POS));
  }
  bool IsTheField(JavaString *name, JavaString *descriptor) {
    return name->Equals(GetName()) && descriptor->Equals(GetDescriptor());
  }
  u_int GetNumberOfRequiredSlots();
  bool HasConstantValue() {
    return Store::DirectWordToInt(GetArg(HAS_CONSTANT_VALUE_POS));
  }
  void InitConstantValue(word constantValue) {
    //Assert(IsStatic()); //--** too strong
    Assert(!HasConstantValue());
    InitArg(HAS_CONSTANT_VALUE_POS, true);
    InitArg(CONSTANT_VALUE_POS, constantValue);
  }
  word GetConstantValue() {
    Assert(HasConstantValue());
    return GetArg(CONSTANT_VALUE_POS);
  }
};

class JavaDll MethodInfo: private Block {
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
    CLASS_NAME_POS, // JavaString
    NAME_POS, // JavaString
    DESCRIPTOR_POS, // JavaString
    BYTE_CODE_POS, // JavaByteCode | int(0)
    SIZE
  };
public:
  using Block::ToWord;

  static MethodInfo *New(u_int accessFlags, JavaString *className,
			 JavaString *name, JavaString *descriptor) {
    Assert(((accessFlags & ACC_PUBLIC) != 0) +
	   ((accessFlags & ACC_PRIVATE) != 0) +
	   ((accessFlags & ACC_PROTECTED) != 0) <= 1);
    Assert((accessFlags & ACC_ABSTRACT) == 0 ||
	   (accessFlags & (ACC_FINAL|ACC_NATIVE|ACC_PRIVATE|ACC_STATIC|
			   ACC_STRICT|ACC_SYNCHRONIZED)) == 0);
    Block *b = Store::AllocBlock(JavaLabel::MethodInfo, SIZE);
    b->InitArg(ACCESS_FLAGS_POS, accessFlags);
    b->InitArg(CLASS_NAME_POS, className->ToWord());
    b->InitArg(NAME_POS, name->ToWord());
    b->InitArg(DESCRIPTOR_POS, descriptor->ToWord());
    b->InitArg(BYTE_CODE_POS, null);
    return static_cast<MethodInfo *>(b);
  }
  static MethodInfo *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == JavaLabel::MethodInfo);
    return static_cast<MethodInfo *>(b);
  }

  u_int GetAccessFlags() {
    return Store::DirectWordToInt(GetArg(ACCESS_FLAGS_POS));
  }
  bool IsPublic() {
    return (GetAccessFlags() & ACC_PUBLIC) != 0;
  }
  bool IsStatic() {
    return (GetAccessFlags() & ACC_STATIC) != 0;
  }
  bool IsAbstract() {
    return (GetAccessFlags() & ACC_ABSTRACT) != 0;
  }
  bool IsNative() {
    return (GetAccessFlags() & ACC_NATIVE) != 0;
  }
  JavaString *GetClassName() {
    return JavaString::FromWordDirect(GetArg(CLASS_NAME_POS));
  }
  JavaString *GetName() {
    return JavaString::FromWordDirect(GetArg(NAME_POS));
  }
  JavaString *GetDescriptor() {
    return JavaString::FromWordDirect(GetArg(DESCRIPTOR_POS));
  }
  u_int GetNumberOfArguments();
  bool IsTheMethod(JavaString *name, JavaString *descriptor) {
    return name->Equals(GetName()) && descriptor->Equals(GetDescriptor());
  }
  JavaByteCode *GetByteCode() {
    return JavaByteCode::FromWord(GetArg(BYTE_CODE_POS));
  }
  void InitByteCode(JavaByteCode *byteCode) {
    Assert((GetAccessFlags() & (ACC_NATIVE | ACC_ABSTRACT)) == 0);
    Assert(GetByteCode() == INVALID_POINTER);
    InitArg(BYTE_CODE_POS, byteCode->ToWord());
  }
};

class JavaDll ClassInfo: private Block {
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
    CLASS_LOADER_POS, // ClassLoader
    ACCESS_FLAGS_POS, // access_flags
    NAME_POS, // JavaString
    SUPER_POS, // Class | int(0)
    INTERFACES_POS, // Table(Class)
    FIELDS_POS, // Table(FieldInfo)
    METHODS_POS, // Table(MethodInfo)
    RUNTIME_CONSTANT_POOL_POS, // RuntimeConstantPool
    SIZE
  };
public:
  using Block::ToWord;

  static ClassInfo *New(ClassLoader *classLoader, u_int accessFlags,
			JavaString *name, word super, Table *interfaces,
			Table *fields, Table *methods,
			RuntimeConstantPool *runtimeConstantPool) {
    Assert(((accessFlags & ACC_INTERFACE) == 0 &&
	    ((accessFlags & ACC_FINAL) != 0) +
	    ((accessFlags & ACC_ABSTRACT) != 0) <= 1) ||
	   (accessFlags & ACC_ABSTRACT) != 0);
    // class files with ACC_SUPER unset not supported by this implementation
    Assert((accessFlags & (ACC_INTERFACE | ACC_SUPER)) != 0);
    Block *b = Store::AllocBlock(JavaLabel::ClassInfo, SIZE);
    b->InitArg(CLASS_LOADER_POS, classLoader->ToWord());
    b->InitArg(ACCESS_FLAGS_POS, accessFlags);
    b->InitArg(NAME_POS, name->ToWord());
    b->InitArg(SUPER_POS, super);
    b->InitArg(INTERFACES_POS, interfaces->ToWord());
    b->InitArg(FIELDS_POS, fields->ToWord());
    b->InitArg(METHODS_POS, methods->ToWord());
    b->InitArg(RUNTIME_CONSTANT_POOL_POS, runtimeConstantPool->ToWord());
    return static_cast<ClassInfo *>(b);
  }
  static ClassInfo *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == JavaLabel::ClassInfo);
    return static_cast<ClassInfo *>(b);
  }

  u_int GetAccessFlags() {
    return Store::DirectWordToInt(GetArg(ACCESS_FLAGS_POS));
  }
  bool IsInterface() {
    return (GetAccessFlags() & ACC_INTERFACE) != 0;
  }
  JavaString *GetName() {
    return JavaString::FromWordDirect(GetArg(NAME_POS));
  }
  word GetSuper() {
    return GetArg(SUPER_POS);
  }
  Table *GetInterfaces() {
    return Table::FromWordDirect(GetArg(INTERFACES_POS));
  }
  Table *GetFields() {
    return Table::FromWordDirect(GetArg(FIELDS_POS));
  }
  Table *GetMethods() {
    return Table::FromWordDirect(GetArg(METHODS_POS));
  }
  RuntimeConstantPool *GetRuntimeConstantPool() {
    word w = GetArg(RUNTIME_CONSTANT_POOL_POS);
    return RuntimeConstantPool::FromWordDirect(w);
  }

  bool Verify();
  Class *Prepare() {
    return Class::New(this);
  }
};

#endif
