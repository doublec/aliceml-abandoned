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
};

static const word null = Store::IntToWord(0);

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
  bool IsInitialized() {
    return GetArg(CLASS_INITIALIZER_POS) == null;
  }
  Worker::Result RunInitializer();
};

class DllExport BaseType: protected Type {
public:
  enum type { Byte, Char, Double, Float, Int, Long, Short, Boolean };
protected:
  enum {
    BASE_TYPE_POS, // int (type)
    SIZE
  };
public:
  using Block::ToWord;

  static BaseType *New(type baseType) {
    Block *b = Store::AllocBlock(JavaLabel::BaseType, SIZE);
    b->InitArg(BASE_TYPE_POS, baseType);
    return static_cast<BaseType *>(b);
  }

  type GetBaseType() {
    return static_cast<type>(Store::DirectWordToInt(GetArg(BASE_TYPE_POS)));
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
class DllExport JavaLong: public Chunk {
public:
  static JavaLong *New(s_int32 high, s_int32 low) {
    Chunk *chunk = Store::AllocChunk(8);
    char *p = chunk->GetBase();
    p[0] = high >> 24; p[1] = high >> 16; p[2] = high >> 8; p[3] = high;
    p[4] = low >> 24; p[5] = low >> 16; p[6] = low >> 8; p[7] = low;
    return static_cast<JavaLong *>(chunk);
  }
  static JavaLong *New(u_char *p) {
    Chunk *chunk = Store::AllocChunk(8);
    std::memcpy(chunk->GetBase(), p, 8);
    return static_cast<JavaLong *>(chunk);
  }
  static JavaLong *FromWordDirect(word x) {
    return static_cast<JavaLong *>(Store::DirectWordToChunk(x));
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
  void AssertAcquired() {
    Assert(Thread::FromWordDirect(GetArg(THREAD_POS)) ==
	   Scheduler::GetCurrentThread());
  }
};

class DllExport Object: private Block {
protected:
  enum {
    CLASS_POS, // Class
    BASE_SIZE
    // ... instance fields
  };

  static Object *New(word wClass, u_int size) {
    Block *b = Store::AllocBlock(JavaLabel::Object, BASE_SIZE + size);
    b->InitArg(0, wClass);
    //--** initialization incorrect for long/float/double
    for (u_int i = size; i--; ) b->InitArg(BASE_SIZE + i, null);
    return static_cast<Object *>(b);
  }
public:
  using Block::ToWord;

  static Object *New(Class *theClass) {
    return New(theClass->ToWord(), theClass->GetNumberOfInstanceFields());
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
    //--** may fail for strings
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
  void InitInstanceField(u_int index, word value) {
    ReplaceArg(BASE_SIZE + index, value);
  }
  void PutInstanceField(u_int index, word value) {
    ReplaceArg(BASE_SIZE + index, value);
  }
  word GetInstanceField(u_int index) {
    return GetArg(BASE_SIZE + index);
  }
  Closure *GetVirtualMethod(u_int index) {
    return GetClass()->GetVirtualMethod(index);
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
  word Load(u_int index) {
    Assert(index < GetLength());
    return GetArg(BASE_SIZE + index);
  }
  void Store(u_int index, word value) {
    Assert(index < GetLength());
    ReplaceArg(BASE_SIZE + index, value);
  }
};

class BaseArray: private Chunk {
  friend class JavaString;
protected:
  enum {
    BASE_TYPE_POS, // byte (BaseType::type);
    BASE_SIZE
    // ... elements
  };
  u_char *GetElementPointer(u_int index, u_int elemSize) {
    Assert(index < (GetSize() - BASE_SIZE) / elemSize);
    return reinterpret_cast<u_char *>
      (GetBase() + BASE_SIZE + index * elemSize);
  }
public:
  using Block::ToWord;

  static BaseArray *New(BaseType::type baseType, u_int length) {
    u_int elemSize;
    switch (baseType) {
    case BaseType::Boolean:
    case BaseType::Byte:
      elemSize = 1;
      break;
    case BaseType::Char:
    case BaseType::Short:
      elemSize = 2;
      break;
    case BaseType::Int:
    case BaseType::Float:
      elemSize = 4;
      break;
    case BaseType::Long:
    case BaseType::Double:
      elemSize = 8;
      break;
    default:
      Error("invalid base type");
    }
    Chunk *chunk = Store::AllocChunk(BASE_SIZE + length * elemSize);
    char *p = chunk->GetBase();
    for (u_int i = length * elemSize; i--; ) p[BASE_SIZE + i] = 0;
    p[BASE_TYPE_POS] = baseType;
    return static_cast<BaseArray *>(chunk);
  }
  static BaseArray *FromWord(word x) {
    return static_cast<BaseArray *>(Store::WordToChunk(x));
  }
  static BaseArray *FromWordDirect(word x) {
    return static_cast<BaseArray *>(Store::DirectWordToChunk(x));
  }

  BaseType::type GetBaseType() {
    return static_cast<BaseType::type>(GetBase()[BASE_TYPE_POS]);
  }
  u_int GetLength() {
    switch (GetBaseType()) {
    case BaseType::Boolean:
    case BaseType::Byte:
      return GetSize() - BASE_SIZE;
    case BaseType::Char:
    case BaseType::Short:
      return (GetSize() - BASE_SIZE) / 2;
    case BaseType::Int:
    case BaseType::Float:
      return (GetSize() - BASE_SIZE) / 4;
    case BaseType::Long:
    case BaseType::Double:
      return (GetSize() - BASE_SIZE) / 8;
    default:
      Error("invalid base type");
    }
  }

  u_int LoadBoolean(u_int index) {
    Assert(GetBaseType() == BaseType::Boolean);
    return GetElementPointer(index, 1)[0];
  }
  u_int LoadByte(u_int index) {
    Assert(GetBaseType() == BaseType::Byte);
    return GetElementPointer(index, 1)[0];
  }
  u_int LoadChar(u_int index) {
    Assert(GetBaseType() == BaseType::Char);
    u_char *p = GetElementPointer(index, 2);
    return (p[0] << 8) | p[1];
  }
  u_int LoadShort(u_int index) {
    Assert(GetBaseType() == BaseType::Short);
    u_char *p = GetElementPointer(index, 2);
    return (p[0] << 8) | p[1];
  }
  u_int LoadInt(u_int index) {
    Assert(GetBaseType() == BaseType::Short);
    u_char *p = GetElementPointer(index, 4);
    return (p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3];
  }
  JavaLong *LoadLong(u_int index) {
    Assert(GetBaseType() == BaseType::Short);
    return JavaLong::New(GetElementPointer(index, 8));
  }
  //--** LoadFloat
  //--** LoadDouble
  word Load(u_int index) {
    //--** remove
    switch (GetBaseType()) {
    case BaseType::Boolean:
      return Store::IntToWord(LoadBoolean(index));
    case BaseType::Byte:
      return Store::IntToWord(LoadByte(index));
    case BaseType::Char:
      return Store::IntToWord(LoadChar(index));
    case BaseType::Short:
      return Store::IntToWord(LoadShort(index));
    case BaseType::Int:
      return Store::IntToWord(LoadInt(index));
    case BaseType::Long:
      return LoadLong(index)->ToWord();
    case BaseType::Float:
    case BaseType::Double:
      Error("unimplemented"); //--**
    default:
      Error("invalid base type");
    }
  }

  void StoreBoolean(u_int index, u_int value) {
    Assert(GetBaseType() == BaseType::Boolean);
    GetElementPointer(index, 1)[0] = value & 1;
  }
  void StoreByte(u_int index, u_int value) {
    Assert(GetBaseType() == BaseType::Byte);
    GetElementPointer(index, 1)[0] = value;
  }
  void StoreChar(u_int index, u_int value) {
    Assert(GetBaseType() == BaseType::Char);
    u_char *p = GetElementPointer(index, 2);
    p[0] = value >> 8;
    p[1] = value;
  }
  void StoreShort(u_int index, u_int value) {
    Assert(GetBaseType() == BaseType::Short);
    u_char *p = GetElementPointer(index, 2);
    p[0] = value >> 8;
    p[1] = value;
  }
  void StoreInt(u_int index, u_int value) {
    Assert(GetBaseType() == BaseType::Short);
    u_char *p = GetElementPointer(index, 4);
    p[0] = value >> 24;
    p[1] = value >> 16;
    p[2] = value >> 8;
    p[3] = value;
  }
  void StoreLong(u_int index, JavaLong *value) {
    Assert(GetBaseType() == BaseType::Short);
    std::memcpy(GetElementPointer(index, 8), value->GetBase(), 8);
  }
  //--** StoreFloat
  //--** StoreDouble
  void Store(u_int index, word value) {
    //--** remove
    switch (GetBaseType()) {
    case BaseType::Boolean:
      StoreBoolean(index, Store::DirectWordToInt(value));
      break;
    case BaseType::Byte:
      StoreByte(index, Store::DirectWordToInt(value));
      break;
    case BaseType::Char:
      StoreChar(index, Store::DirectWordToInt(value));
      break;
    case BaseType::Short:
      StoreShort(index, Store::DirectWordToInt(value));
      break;
    case BaseType::Int:
      StoreInt(index, Store::DirectWordToInt(value));
      break;
    case BaseType::Long:
      StoreLong(index, JavaLong::FromWordDirect(value));
      break;
    case BaseType::Float:
    case BaseType::Double:
      Error("unimplemented"); //--**
    default:
      Error("invalid base type");
    }
  }
};

class DllExport JavaString: public Object {
private:
  static word wClass;

  enum {
    VALUE_INDEX, // BaseArray(Char)
    OFFSET_INDEX, // int
    COUNT_INDEX, // int
    SIZE
  };

  BaseArray *GetValue() {
    return BaseArray::FromWordDirect(GetInstanceField(VALUE_INDEX));
  }
  u_int GetOffset() {
    return Store::DirectWordToInt(GetInstanceField(OFFSET_INDEX));
  }
  u_int GetCount() {
    return Store::DirectWordToInt(GetInstanceField(COUNT_INDEX));
  }
  u_char *GetBase(u_int offset) {
    Assert(offset < GetValue()->GetLength());
    return GetValue()->GetElementPointer(offset, 2);
  }
  u_char *GetBase() {
    return GetBase(GetOffset());
  }
public:
  static void Init();

  static JavaString *New(BaseArray *array, u_int offset, u_int length) {
    Object *object = Object::New(wClass, SIZE);
    object->InitInstanceField(VALUE_INDEX, array->ToWord());
    object->InitInstanceField(OFFSET_INDEX, Store::IntToWord(offset));
    object->InitInstanceField(COUNT_INDEX, Store::IntToWord(length));
    return static_cast<JavaString *>(object);
  }
  static JavaString *New(u_int length) {
    return New(BaseArray::New(BaseType::Char, length), 0, length);
  }
  static JavaString *New(const u_wchar *s, u_int length) {
    JavaString *string = New(length);
    u_char *p = string->GetBase(0);
    for (u_int i = length; i--; ) {
      u_int i = *s++;
      *p++ = i >> 8;
      *p++ = i;
    }
    return string;
  }
  static JavaString *New(const char *s, u_int length) {
    JavaString *string = New(length);
    u_char *p = string->GetBase(0);
    for (u_int i = length; i--; ) { *p++ = 0; *p++ = *s++; }
    return string;
  }
  static JavaString *New(const char *s) {
    return New(s, std::strlen(s));
  }
  static JavaString *FromWord(word x) {
    return static_cast<JavaString *>(Object::FromWord(x));
  }
  static JavaString *FromWordDirect(word x) {
    return static_cast<JavaString *>(Object::FromWordDirect(x));
  }

  u_int GetLength() {
    return GetCount();
  }
  u_wchar CharAt(u_int index) {
    u_char *p = GetBase(GetOffset() + index);
    return (p[0] << 8) | p[1];
  }
  bool Equals(JavaString *otherString) {
    u_int length = GetLength();
    if (otherString->GetLength() != length) return false;
    return !std::memcmp(GetBase(), otherString->GetBase(),
			length * sizeof(u_wchar));
  }
  JavaString *Concat(JavaString *otherString) {
    u_int length = GetLength();
    u_int otherLength = otherString->GetLength();
    JavaString *resultString = JavaString::New(length + otherLength);
    u_char *p = resultString->GetBase(0);
    std::memcpy(p, GetBase(), length * 2);
    std::memcpy(p + length * 2, otherString->GetBase(), otherLength * 2);
    return resultString;
  }
  JavaString *Concat(const char *s) {
    u_int length = GetLength();
    u_int otherLength = std::strlen(s);
    JavaString *resultString = JavaString::New(length + otherLength);
    u_char *p = resultString->GetBase(0);
    std::memcpy(p, GetBase(), length * 2);
    p += length * 2;
    for (u_int i = otherLength; i--; ) { *p++ = 0; *p++ = *s++; }
    return resultString;
  }
  JavaString *Substring(u_int beginIndex, u_int endIndex) {
    u_int offset = GetOffset();
    return JavaString::New(GetValue(), offset + beginIndex,
			   endIndex - beginIndex);
  }
  JavaString *Intern() {
    return this; //--**
  }
  BaseArray *ToArray() {
    BaseArray *array = GetValue();
    u_int offset = GetOffset();
    u_int length = GetLength();
    if (offset == 0 && length == array->GetLength()) return array;
    BaseArray *newArray = BaseArray::New(BaseType::Char, length);
    for (u_int i = length; i--; )
      newArray->StoreChar(i, array->LoadChar(offset + i));
    return newArray;
  }

  char *ExportC() {
    u_int n = GetLength();
    Chunk *chunk = Store::AllocChunk(n + 1);
    char *p = chunk->GetBase();
    u_char *q = GetBase();
    for (u_int i = n; i--; ) p[i] = q[i * 2 + 1];
    p[n] = '\0';
    return p;
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
