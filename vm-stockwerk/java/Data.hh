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

typedef unsigned short u_wchar; //--** ensure that this is always 16-bit

class JavaLabel {
private:
  static const u_int base = MIN_DATA_LABEL;
public:
  static const BlockLabel ClassLoader         = (BlockLabel) (base + 0);
  // Symbolic class representations
  static const BlockLabel ConstantPool        = (BlockLabel) (base + 1);
  static const BlockLabel Table               = (BlockLabel) (base + 2);
  static const BlockLabel FieldInfo           = (BlockLabel) (base + 3);
  static const BlockLabel MethodInfo          = (BlockLabel) (base + 4);
  static const BlockLabel ClassInfo           = (BlockLabel) (base + 5);
  // Code
  static const BlockLabel ExceptionTableEntry = (BlockLabel) (base + 6);
  static const BlockLabel VirtualTable        = (BlockLabel) (base + 7);
  // Types
  static const BlockLabel Class               = (BlockLabel) (base + 8);
  static const BlockLabel ObjectArrayType     = (BlockLabel) (base + 9);
  static const BlockLabel BaseArrayType       = (BlockLabel) (base + 10);
  // Data layer
  static const BlockLabel Lock                = (BlockLabel) (base + 11);
  static const BlockLabel Object              = (BlockLabel) (base + 12);
  static const BlockLabel ObjectArray         = (BlockLabel) (base + 13);
};

//
// Types
//

class DllExport Type: public Block {};

class DllExport Class: private Type {
protected:
  enum {
    CLASS_INFO_POS, // ClassInfo
    NUMBER_OF_VIRTUAL_METHODS_POS, // int
    NUMBER_OF_INSTANCE_FIELDS_POS, // int
    VIRTUAL_TABLE_POS, // Block(Closure ... Closure)
    LOCK_POS,
    INITIALIZATION_THREAD_POS, // Thread | int(0)
    BASE_SIZE
    // ... static fields
    // ... static methods
  };
public:
  using Block::ToWord;

  static Class *New(class ClassInfo *classInfo);
  static Class *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER && b->GetLabel() == JavaLabel::Class);
    return static_cast<Class *>(b);
  }
  static Class *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == JavaLabel::Class);
    return static_cast<Class *>(b);
  }

  bool IsInterface();
  Class *GetSuperClass();
  u_int GetNumberOfInstanceFields() {
    return Store::DirectWordToInt(GetArg(NUMBER_OF_INSTANCE_FIELDS_POS));
  }
  u_int GetNumberOfVirtualMethods() {
    return Store::DirectWordToInt(GetArg(NUMBER_OF_VIRTUAL_METHODS_POS));
  }
  Block *GetVirtualTable() {
    return Store::DirectWordToBlock(GetArg(VIRTUAL_TABLE_POS));
  }
  word GetStaticField(u_int index) {
    return GetArg(BASE_SIZE + index);
  }
  Closure *GetStaticMethod(u_int index) {
    return Closure::FromWordDirect(GetArg(BASE_SIZE + index));
  }
};

class DllExport ObjectArrayType: private Type {
protected:
  enum {
    CLASS_POS, // Class
    DIMENSIONS_POS, // int
    SIZE
  };
public:
  using Block::ToWord;

  static ObjectArrayType *New(word classType, u_int dimensions) {
    Block *b = Store::AllocBlock(JavaLabel::ObjectArrayType, SIZE);
    b->InitArg(CLASS_POS, classType);
    b->InitArg(DIMENSIONS_POS, dimensions);
    return static_cast<ObjectArrayType *>(b);
  }
};

class DllExport BaseType {
public:
  enum type { Byte, Char, Double, Float, Int, Long, Short, Boolean };
};

class DllExport BaseArrayType: private Type {
protected:
  enum {
    BASE_TYPE_POS, // int(BaseType)
    DIMENSIONS_POS, // int
    SIZE
  };
public:
  using Block::ToWord;

  static BaseArrayType *New(BaseType::type baseType, u_int dimensions) {
    Assert(dimensions > 0);
    Block *b = Store::AllocBlock(JavaLabel::BaseArrayType, SIZE);
    b->InitArg(BASE_TYPE_POS, baseType);
    b->InitArg(DIMENSIONS_POS, dimensions);
    return static_cast<BaseArrayType *>(b);
  }
};

//
// Data Layer
//

static const word null = Store::IntToWord(0);

class DllExport Lock: private Block {
  //--** to be determined
};

class DllExport Object: private Block {
protected:
  enum {
    CLASS_POS, // Class
    BASE_SIZE
    // ... instance fields
  };
public:
  static Object *New(Class *theClass) {
    u_int size = theClass->GetNumberOfInstanceFields();
    Block *b = Store::AllocBlock(JavaLabel::Object, BASE_SIZE + size);
    b->InitArg(0, theClass->ToWord());
    for (u_int i = size; i--; ) b->InitArg(BASE_SIZE + i, null);
    return static_cast<Object *>(b);
  }

  Class *GetClass() {
    return Class::FromWordDirect(GetArg(CLASS_POS));
  }
  bool IsInstanceOf(Class *aClass) {
    Assert(!aClass->IsInterface());
    Class *theClass = GetClass();
  loop:
    if (theClass == aClass) return true;
    theClass = theClass->GetSuperClass();
    if (theClass == INVALID_POINTER) return false;
    goto loop;
  }
  word GetInstanceField(u_int index) {
    return GetArg(BASE_SIZE + index);
  }
  Closure *GetVirtualMethod(u_int index) {
    Block *vTable = GetClass()->GetVirtualTable();
    return Closure::FromWordDirect(vTable->GetArg(index));
  }
};

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
  JavaString *Intern() {
    return this; //--**
  }

  char *ExportC() {
    u_int n = GetLength();
    Chunk *chunk = Store::AllocChunk(n + 1);
    char *p = chunk->GetBase();
    u_wchar *q = GetBase();
    for (u_int i = n; i--; ) p[i] = q[i];
    p[n] = '\0';
    return p;
  }
};

class DllExport ObjectArray: private Block {
protected:
  enum {
    TYPE_POS, // ObjectArrayType
    LENGTH_POS, // int
    BASE_SIZE
    // ... elements
  };
public:
  using Block::ToWord;

  static ObjectArray *New(ObjectArrayType *type, u_int length) {
    //--** support multiple dimensions
    Block *b = Store::AllocBlock(JavaLabel::ObjectArray, BASE_SIZE + length);
    b->InitArg(TYPE_POS, type->ToWord());
    b->InitArg(LENGTH_POS, Store::IntToWord(length));
    for (u_int i = length; i--; ) b->InitArg(BASE_SIZE + i, null);
    return static_cast<ObjectArray *>(b);
  }
  static ObjectArray *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == JavaLabel::ObjectArray);
    return static_cast<ObjectArray *>(b);
  }
  static ObjectArray *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == JavaLabel::ObjectArray);
    return static_cast<ObjectArray *>(b);
  }

  u_int GetLength() {
    return Store::DirectWordToInt(GetArg(LENGTH_POS));
  }
  void Init(u_int index, word value) {
    Assert(index < GetLength());
    InitArg(BASE_SIZE + index, value);
  }
  void Assign(u_int index, word value) {
    Assert(index < GetLength());
    ReplaceArg(BASE_SIZE + index, value);
  }
  word Get(u_int index) {
    Assert(index < GetLength());
    return GetArg(BASE_SIZE + index);
  }
};

//
// Resolved Constant Pool Entries
//

class DllExport FieldRef: private Block {
public:
  u_int GetIndex();
};

class DllExport StaticFieldRef: public FieldRef {
public:
  Class *GetClass();
};

class DllExport InstanceFieldRef: public FieldRef {
};

class DllExport MethodRef: private Block {
public:
  Class *GetClass();
  u_int GetIndex();
  u_int GetNumberOfArguments();
};

class DllExport StaticMethodRef: public MethodRef {
};

class DllExport VirtualMethodRef: public MethodRef {
};

#endif
