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
  static const BlockLabel ClassLoader         = (BlockLabel) (Array + 10);
  static const BlockLabel ConstantPool        = (BlockLabel) (Array + 11);
};

static const word null = Store::IntToWord(0);

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

class DllExport JavaArray: private Block {
protected:
  enum {
    CLASS_POS, // Class
    SIZE_POS, // int
    BASE_SIZE
    // ... elements
  };
public:
  using Block::ToWord;

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
  u_int GetLength() {
    return Store::DirectWordToInt(GetArg(SIZE_POS));
  }
};

class DllExport Lock: private Block {
  // to be determined
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
  u_int GetIndex();
};

class DllExport StaticMethodRef: public MethodRef {
public:
  Class *GetClass();
};

class DllExport VirtualMethodRef: public MethodRef {
};

#endif
