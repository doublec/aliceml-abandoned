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
#include "generic/Transients.hh"
#include "generic/Worker.hh"

typedef u_char u_int8;
typedef short s_int16; //--** ensure that this is always 16-bit
typedef unsigned short u_int16; //--** ensure that this is always 16-bit
typedef s_int s_int32; //--** ensure that this is always 32-bit
typedef u_int u_int32; //--** ensure that this is always 32-bit
typedef u_int16 u_wchar;

class JavaLabel {
private:
  static const u_int base = MIN_DATA_LABEL;
public:
  static const BlockLabel ClassLoader         = (BlockLabel) (base + 0);
  // Symbolic class representation
  static const BlockLabel ConstantPool        = (BlockLabel) (base + 1);
  static const BlockLabel Table               = (BlockLabel) (base + 2);
  static const BlockLabel FieldInfo           = (BlockLabel) (base + 3);
  static const BlockLabel MethodInfo          = (BlockLabel) (base + 4);
  static const BlockLabel ClassInfo           = (BlockLabel) (base + 5);
  // Runtime class representation
  static const BlockLabel RuntimeConstantPool = (BlockLabel) (base + 6);
  static const BlockLabel VirtualTable        = (BlockLabel) (base + 7);
  static const BlockLabel StaticFieldRef      = (BlockLabel) (base + 8);
  static const BlockLabel InstanceFieldRef    = (BlockLabel) (base + 9);
  static const BlockLabel StaticMethodRef     = (BlockLabel) (base + 10);
  static const BlockLabel VirtualMethodRef    = (BlockLabel) (base + 11);
  // Code
  static const BlockLabel ExceptionTableEntry = (BlockLabel) (base + 12);
  // Types
  static const BlockLabel Class               = (BlockLabel) (base + 13);
  static const BlockLabel BaseType            = (BlockLabel) (base + 14);
  static const BlockLabel ArrayType           = (BlockLabel) (base + 15);
  // Data layer
  static const BlockLabel Lock                = (BlockLabel) (base + 16);
  static const BlockLabel Object              = (BlockLabel) (base + 17);
  static const BlockLabel ObjectArray         = (BlockLabel) (base + 18);
  static const BlockLabel BaseArray           = (BlockLabel) (base + 19);
};

//
// Types
//

class DllExport Type: public Block {
public:
  static Type *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == JavaLabel::Class ||
	   b->GetLabel() == JavaLabel::BaseType ||
	   b->GetLabel() == JavaLabel::ArrayType);
    return static_cast<Type *>(b);
  }
  static Type *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == JavaLabel::Class ||
	   b->GetLabel() == JavaLabel::BaseType ||
	   b->GetLabel() == JavaLabel::ArrayType);
    return static_cast<Type *>(b);
  }
};

class DllExport Class: protected Type {
protected:
  enum {
    CLASS_INFO_POS, // ClassInfo
    NUMBER_OF_VIRTUAL_METHODS_POS, // int
    NUMBER_OF_INSTANCE_FIELDS_POS, // int
    VIRTUAL_TABLE_POS, // Block(Closure ... Closure)
    LOCK_POS, // Lock
    CLASS_INITIALIZER_POS, // Closure | int(0)
    BASE_SIZE
    // ... static fields
    // ... static methods
  };
public:
  using Block::ToWord;

  static void Init();

  static Class *New(class ClassInfo *classInfo);
  static Class *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == JavaLabel::Class);
    return static_cast<Class *>(b);
  }
  static Class *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == JavaLabel::Class);
    return static_cast<Class *>(b);
  }

  class ClassInfo *GetClassInfo();
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
  Closure *GetVirtualMethod(u_int index) {
    return Closure::FromWordDirect(GetVirtualTable()->GetArg(index));
  }
  class Lock *GetLock();
  Closure *GetClassInitializer() {
    word wClassInitializer = GetArg(CLASS_INITIALIZER_POS);
    if (wClassInitializer == Store::IntToWord(0)) return INVALID_POINTER;
    return Closure::FromWordDirect(wClassInitializer);
  }
  word GetStaticField(u_int index) {
    return GetArg(BASE_SIZE + index);
  }
  void PutStaticField(u_int index, word value) {
    ReplaceArg(BASE_SIZE + index, value);
  }
  Closure *GetStaticMethod(u_int index) {
    return Closure::FromWordDirect(GetArg(BASE_SIZE + index));
  }
  Worker::Result Initialize(Future *future);
};

class DllExport BaseType: protected Type {
public:
  enum type { Byte, Char, Double, Float, Int, Long, Short, Boolean };
protected:
  enum {
    BASE_TYPE_POS, // int(BaseType)
    SIZE
  };
public:
  using Block::ToWord;

  static BaseType *New(type baseType) {
    Block *b = Store::AllocBlock(JavaLabel::BaseType, SIZE);
    b->InitArg(BASE_TYPE_POS, baseType);
    return static_cast<BaseType *>(b);
  }
};

class DllExport ArrayType: protected Type {
protected:
  enum {
    TYPE_POS, // Type
    SIZE
  };
public:
  using Block::ToWord;

  static ArrayType *New(word type) {
    Block *b = Store::AllocBlock(JavaLabel::ArrayType, SIZE);
    b->InitArg(TYPE_POS, type);
    return static_cast<ArrayType *>(b);
  }
};

//
// Data Layer
//

static const word null = Store::IntToWord(0);

class DllExport JavaLong: private Chunk {
protected:
  static const u_int SIZE = 64 / sizeof(char);
public:
  using Block::ToWord;

  static JavaLong *New(s_int32 high, s_int32 low) {
    Chunk *chunk = Store::AllocChunk(SIZE);
    char *p = chunk->GetBase();
    p[0] = high >> 24; p[1] = high >> 16; p[2] = high >> 8; p[3] = high;
    p[4] = low >> 24; p[5] = low >> 16; p[6] = low >> 8; p[7] = low;
    return static_cast<JavaLong *>(chunk);
  }
};

class DllExport Lock: private Block {
protected:
  enum { COUNT_POS, THREAD_POS, FUTURE_POS, SIZE };
public:
  using Block::ToWord;

  static Lock *New() {
    Block *b = Store::AllocBlock(JavaLabel::Lock, SIZE);
    b->InitArg(COUNT_POS, 0);
    return static_cast<Lock *>(b);
  }
  static Lock *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == JavaLabel::Lock);
    return static_cast<Lock *>(b);
  }

  Future *Acquire() {
    u_int count = Store::DirectWordToInt(GetArg(COUNT_POS));
    if (count == 0) {
      ReplaceArg(COUNT_POS, 1);
      ReplaceArg(THREAD_POS, Scheduler::GetCurrentThread()->ToWord());
      ReplaceArg(FUTURE_POS, 0);
      return INVALID_POINTER;
    } else {
      Thread *thread = Thread::FromWordDirect(GetArg(THREAD_POS));
      if (thread == Scheduler::GetCurrentThread()) {
	ReplaceArg(COUNT_POS, count + 1);
	return INVALID_POINTER;
      } else {
	word wFuture = GetArg(FUTURE_POS);
	Future *future;
	if (wFuture == Store::IntToWord(0)) {
	  future = Future::New();
	  ReplaceArg(FUTURE_POS, future->ToWord());
	} else {
	  future = static_cast<Future *>
	    (Store::DirectWordToTransient(wFuture));
	}
	future->AddToWaitQueue(thread);
	Scheduler::currentData = future->ToWord();
	return future;
      }
    }
  }
  void Release() {
    u_int count = Store::DirectWordToInt(GetArg(COUNT_POS));
    Assert(count > 0);
    Assert(Thread::FromWordDirect(GetArg(THREAD_POS)) ==
	   Scheduler::GetCurrentThread());
    if (count > 1) {
      ReplaceArg(COUNT_POS, count - 1);
    } else {
      word wFuture = GetArg(FUTURE_POS);
      if (wFuture != Store::IntToWord(0)) {
	Future *future = static_cast<Future *>
	  (Store::DirectWordToTransient(wFuture));
	future->ScheduleWaitingThreads();
      }
      ReplaceArg(COUNT_POS, 0);
    }
  }
};

class DllExport Object: private Block {
protected:
  enum {
    CLASS_POS, // Class
    BASE_SIZE
    // ... instance fields
  };
public:
  using Block::ToWord;

  static Object *New(Class *theClass) {
    u_int size = theClass->GetNumberOfInstanceFields();
    Block *b = Store::AllocBlock(JavaLabel::Object, BASE_SIZE + size);
    b->InitArg(0, theClass->ToWord());
    //--** initialization incorrect for long/float/double
    for (u_int i = size; i--; ) b->InitArg(BASE_SIZE + i, null);
    return static_cast<Object *>(b);
  }
  static Object *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == JavaLabel::Object);
    return static_cast<Object *>(b);
  }
  static Object *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == JavaLabel::Object);
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
  void PutInstanceField(u_int index, word value) {
    ReplaceArg(BASE_SIZE + index, value);
  }
  Closure *GetVirtualMethod(u_int index) {
    return GetClass()->GetVirtualMethod(index);
  }
};

//--** always store in big-endian format
class DllExport JavaString: private Chunk {
public:
  using Chunk::ToWord;

  u_int GetLength() {
    return GetSize() / sizeof(u_wchar);
  }
  u_wchar *GetBase() {
    return reinterpret_cast<u_wchar *>(Chunk::GetBase());
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
  JavaString *Concat(JavaString *otherString) {
    u_int length = GetLength();
    u_int otherLength = otherString->GetLength();
    JavaString *resultString = JavaString::New(length + otherLength);
    u_wchar *p = resultString->GetBase();
    std::memcpy(p, GetBase(), length * sizeof(u_wchar));
    std::memcpy(p + length, otherString->GetBase(),
		otherLength * sizeof(u_wchar));
    return resultString;
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
    TYPE_POS, // ArrayType(Type != BaseType)
    LENGTH_POS, // int
    BASE_SIZE
    // ... elements
  };
public:
  using Block::ToWord;

  static ObjectArray *New(Type *type, u_int length) {
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

  Type *GetType() {
    return Type::FromWordDirect(GetArg(TYPE_POS));
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

class BaseArray: private Block {
protected:
  enum {
    TYPE_POS, // ArrayType (Type == BaseType);
    LENGTH_POS, // int
    BASE_SIZE
    // ... elements
  };
public:
  using Block::ToWord;

  static BaseArray *New(BaseType *type, u_int length) {
    Block *b = Store::AllocBlock(JavaLabel::BaseArray, BASE_SIZE + length);
    b->InitArg(TYPE_POS, type->ToWord());
    b->InitArg(LENGTH_POS, Store::IntToWord(length));
    // to be done: proper initialization
    for (u_int i = length; i--; ) b->InitArg(BASE_SIZE + i, null);
    return static_cast<BaseArray *>(b);
  }
  static BaseArray *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == JavaLabel::BaseArray);
    return static_cast<BaseArray *>(b);
  }
  static BaseArray *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == JavaLabel::BaseArray);
    return static_cast<BaseArray *>(b);
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
// Runtime Constant Pool Entries
//

class DllExport RuntimeConstantPool: private Block {
public:
  using Block::ToWord;

  static RuntimeConstantPool *New(u_int size) {
    Block *b = Store::AllocBlock(JavaLabel::RuntimeConstantPool, size);
    return static_cast<RuntimeConstantPool *>(b);
  }
  static RuntimeConstantPool *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == JavaLabel::RuntimeConstantPool);
    return static_cast<RuntimeConstantPool *>(b);
  }

  void Init(u_int index, word value) {
    InitArg(index - 1, value);
  }
  word Get(u_int index) {
    return GetArg(index - 1);
  }
};

class DllExport FieldRef: public Block {
public:
  static FieldRef *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == JavaLabel::StaticFieldRef ||
	   b->GetLabel() == JavaLabel::InstanceFieldRef);
    return static_cast<FieldRef *>(b);
  }
};

class DllExport StaticFieldRef: private FieldRef {
protected:
  enum { CLASS_POS, INDEX_POS, SIZE };
public:
  using Block::ToWord;

  static StaticFieldRef *New(Class *theClass, u_int index) {
    Block *b = Store::AllocBlock(JavaLabel::StaticFieldRef, SIZE);
    b->InitArg(CLASS_POS, theClass->ToWord());
    b->InitArg(INDEX_POS, index);
    return static_cast<StaticFieldRef *>(b);
  }
  static StaticFieldRef *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == JavaLabel::StaticFieldRef);
    return static_cast<StaticFieldRef *>(b);
  }

  Class *GetClass() {
    return Class::FromWordDirect(GetArg(CLASS_POS));
  }
  u_int GetIndex() {
    return Store::DirectWordToInt(GetArg(INDEX_POS));
  }
};

class DllExport InstanceFieldRef: private FieldRef {
protected:
  enum { INDEX_POS, SIZE };
public:
  using Block::ToWord;

  static InstanceFieldRef *New(u_int index) {
    Block *b = Store::AllocBlock(JavaLabel::InstanceFieldRef, SIZE);
    b->InitArg(INDEX_POS, index);
    return static_cast<InstanceFieldRef *>(b);
  }
  static InstanceFieldRef *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == JavaLabel::InstanceFieldRef);
    return static_cast<InstanceFieldRef *>(b);
  }

  u_int GetIndex() {
    return Store::DirectWordToInt(GetArg(INDEX_POS));
  }
};

class DllExport MethodRef: public Block {
public:
  static MethodRef *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == JavaLabel::StaticMethodRef ||
	   b->GetLabel() == JavaLabel::VirtualMethodRef);
    return static_cast<MethodRef *>(b);
  }
};

class DllExport StaticMethodRef: private MethodRef {
protected:
  enum { CLASS_POS, INDEX_POS, NUMBER_OF_ARGUMENTS_POS, SIZE };
public:
  using Block::ToWord;

  static StaticMethodRef *New(Class *theClass, u_int index, u_int nArgs) {
    Block *b = Store::AllocBlock(JavaLabel::StaticMethodRef, SIZE);
    b->InitArg(CLASS_POS, theClass->ToWord());
    b->InitArg(INDEX_POS, index);
    b->InitArg(NUMBER_OF_ARGUMENTS_POS, nArgs);
    return static_cast<StaticMethodRef *>(b);
  }
  static StaticMethodRef *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == JavaLabel::StaticMethodRef);
    return static_cast<StaticMethodRef *>(b);
  }

  Class *GetClass() {
    return Class::FromWordDirect(GetArg(CLASS_POS));
  }
  u_int GetIndex() {
    return Store::DirectWordToInt(GetArg(INDEX_POS));
  }
  u_int GetNumberOfArguments() {
    return Store::DirectWordToInt(GetArg(NUMBER_OF_ARGUMENTS_POS));
  }
};

class DllExport VirtualMethodRef: private MethodRef {
protected:
  enum { CLASS_POS, INDEX_POS, NUMBER_OF_ARGUMENTS_POS, SIZE };
public:
  using Block::ToWord;

  static VirtualMethodRef *New(Class *theClass, u_int index, u_int nArgs) {
    Block *b = Store::AllocBlock(JavaLabel::VirtualMethodRef, SIZE);
    b->InitArg(CLASS_POS, theClass->ToWord());
    b->InitArg(INDEX_POS, index);
    b->InitArg(NUMBER_OF_ARGUMENTS_POS, nArgs);
    return static_cast<VirtualMethodRef *>(b);
  }
  static VirtualMethodRef *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == JavaLabel::VirtualMethodRef);
    return static_cast<VirtualMethodRef *>(b);
  }

  Class *GetClass() {
    return Class::FromWordDirect(GetArg(CLASS_POS));
  }
  u_int GetIndex() {
    return Store::DirectWordToInt(GetArg(INDEX_POS));
  }
  u_int GetNumberOfArguments() {
    return Store::DirectWordToInt(GetArg(NUMBER_OF_ARGUMENTS_POS));
  }
};

//
// Implementation of Inline `Class' Methods
//

inline Lock *Class::GetLock() {
  return Lock::FromWordDirect(GetArg(LOCK_POS));
}

#endif
