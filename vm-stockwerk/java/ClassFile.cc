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
#pragma implementation "java/ClassFile.hh"
#endif

#include <cstdio>
#include "java/ClassFile.hh"
#include "java/ClassLoader.hh"

enum CONSTANT_tag {
  // Tags for constant pool entries as defined by the JVM specification
  CONSTANT_Class              = 7,
  CONSTANT_Fieldref           = 9,
  CONSTANT_Methodref          = 10,
  CONSTANT_InterfaceMethodref = 11,
  CONSTANT_String             = 8,
  CONSTANT_Integer            = 3,
  CONSTANT_Float              = 4,
  CONSTANT_Long               = 5,
  CONSTANT_Double             = 6,
  CONSTANT_NameAndType        = 12,
  CONSTANT_Utf8               = 1,
  // Tags for unusable constant pool entries
  CONSTANT_Long_unusable      = MAX_DATA_LABEL - 1,
  CONSTANT_Double_unusable    = MAX_DATA_LABEL
};

//
// Symbolic Constant Pool Entries
//
class ConstantPoolEntry: private Block {
public:
  using Block::ToWord;
  using Block::InitArg;
  using Block::GetArg;

  static ConstantPoolEntry *New(CONSTANT_tag tag, u_int size) {
    return static_cast<ConstantPoolEntry *>
      (Store::AllocBlock(static_cast<BlockLabel>(MIN_DATA_LABEL + tag), size));
  }
  static ConstantPoolEntry *FromWordDirect(word x) {
    return static_cast<ConstantPoolEntry *>(Store::DirectWordToBlock(x));
  }

  CONSTANT_tag GetTag() {
    return static_cast<CONSTANT_tag>(GetLabel() - MIN_DATA_LABEL);
  }
  word Resolve(ConstantPool *constantPool, ClassLoader *classLoader);
};

class ConstantPool: private Block {
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

  u_int GetSize() {
    return Store::DirectWordToInt(GetArg(SIZE_POS));
  }
  void Init(u_int index, ConstantPoolEntry *entry) {
    Assert(index >= 1 && index <= GetSize());
    InitArg(index - 1 + BASE_SIZE, entry->ToWord());
  }
  ConstantPoolEntry *Get(u_int index) {
    Assert(index >= 1 && index <= GetSize());
    return ConstantPoolEntry::FromWordDirect(GetArg(index - 1 + BASE_SIZE));
  }
  JavaString *GetUtf8(u_int index) {
    ConstantPoolEntry *entry = Get(index);
    Assert(entry->GetTag() == CONSTANT_Utf8);
    return JavaString::FromWordDirect(entry->GetArg(0));
  }
  word GetClass(u_int index, ClassLoader *classLoader) {
    ConstantPoolEntry *entry = Get(index);
    Assert(entry->GetTag() == CONSTANT_Class);
    JavaString *name = GetUtf8(Store::DirectWordToInt(entry->GetArg(0)));
    return classLoader->ResolveClass(name);
  }
  JavaString *GetNameAndType(u_int index, JavaString *&descriptor) {
    ConstantPoolEntry *entry = Get(index);
    Assert(entry->GetTag() == CONSTANT_NameAndType);
    descriptor = GetUtf8(Store::DirectWordToInt(entry->GetArg(1)));
    return GetUtf8(Store::DirectWordToInt(entry->GetArg(0)));
  }
};

word ConstantPoolEntry::Resolve(ConstantPool *constantPool,
				ClassLoader *classLoader) {
  switch (GetTag()) {
  case CONSTANT_Class:
    {
      u_int nameIndex = Store::DirectWordToInt(GetArg(0));
      return classLoader->ResolveClass(constantPool->GetUtf8(nameIndex));
    }
  case CONSTANT_Fieldref:
    {
      //--** All fields of interfaces must have their ACC_PUBLIC, ACC_STATIC,
      //--** and ACC_FINAL flags set and may not have any of the other flags
      //--** in Table 4.4 set
      u_int classIndex = Store::DirectWordToInt(GetArg(0));
      u_int nameAndTypeIndex = Store::DirectWordToInt(GetArg(1));
      word theClass = constantPool->GetClass(classIndex, classLoader);
      JavaString *descriptor;
      JavaString *name =
	constantPool->GetNameAndType(nameAndTypeIndex, descriptor);
      return classLoader->ResolveFieldRef(theClass, name, descriptor);
    }
  case CONSTANT_Methodref:
    {
      u_int classIndex = Store::DirectWordToInt(GetArg(0));
      u_int nameAndTypeIndex = Store::DirectWordToInt(GetArg(1));
      word theClass = constantPool->GetClass(classIndex, classLoader);
      JavaString *descriptor;
      JavaString *name =
	constantPool->GetNameAndType(nameAndTypeIndex, descriptor);
      return classLoader->ResolveMethodRef(theClass, name, descriptor);
    }
  case CONSTANT_InterfaceMethodref:
    {
      u_int classIndex = Store::DirectWordToInt(GetArg(0));
      u_int nameAndTypeIndex = Store::DirectWordToInt(GetArg(1));
      word theClass = constantPool->GetClass(classIndex, classLoader);
      JavaString *descriptor;
      JavaString *name =
	constantPool->GetNameAndType(nameAndTypeIndex, descriptor);
      return classLoader->ResolveInterfaceMethodRef(theClass, name,
						    descriptor);
    }
  case CONSTANT_String:
    {
      u_int index = Store::DirectWordToInt(GetArg(0));
      return constantPool->GetUtf8(index)->Intern()->ToWord();
    }
  case CONSTANT_Integer:
  case CONSTANT_Long:
  case CONSTANT_Float:
  case CONSTANT_Double:
    return GetArg(0);
  case CONSTANT_Long_unusable:
  case CONSTANT_Double_unusable:
    return null; // may never be referenced
  case CONSTANT_NameAndType:
  case CONSTANT_Utf8:
    return null; // may only be referenced from constant pool
  default:
    Error("invalid constant pool tag");
  }
}

//
// ClassFile Implementation
//
bool ClassFile::ParseMagic(u_int &offset) {
  return GetU4(offset) == MAGIC;
}

bool ClassFile::ParseVersion(u_int &offset) {
  u_int minor = GetU2(offset);
  u_int major = GetU2(offset);
  return
    (major >= SupportedVersion::majorMin ||
     (major == SupportedVersion::majorMin &&
      minor >= SupportedVersion::minorMin)) &&
    (major <= SupportedVersion::majorMax ||
     (major == SupportedVersion::majorMax &&
      minor <= SupportedVersion::minorMax));
}

ConstantPool *ClassFile::ParseConstantPool(u_int &offset) {
  u_int constantPoolCount = GetU2(offset);
  ConstantPool *constantPool = ConstantPool::New(constantPoolCount - 1);
  for (u_int i = 1; i < constantPoolCount; i++) {
    ConstantPoolEntry *entry = ParseConstantPoolEntry(offset);
    if (entry == INVALID_POINTER) return INVALID_POINTER;
    constantPool->Init(i, entry);
    CONSTANT_tag tag = entry->GetTag();
    switch (tag) { // fix constants that take up two constant pool entries
    case CONSTANT_Long:
      constantPool->Init(++i,
			 ConstantPoolEntry::New(CONSTANT_Long_unusable, 0));
      break;
    case CONSTANT_Double:
      constantPool->Init(++i,
			 ConstantPoolEntry::New(CONSTANT_Double_unusable, 0));
      break;
    default:
      break;
    }
  }
  return constantPool;
}

ConstantPoolEntry *ClassFile::ParseConstantPoolEntry(u_int &offset) {
  CONSTANT_tag tag = static_cast<CONSTANT_tag>(GetU1(offset));
  switch (tag) {
  case CONSTANT_Class:
    {
      ConstantPoolEntry *entry = ConstantPoolEntry::New(tag, 1);
      entry->InitArg(0, GetU2(offset)); // name_index
      return entry;
    }
    break;
  case CONSTANT_Fieldref:
  case CONSTANT_Methodref:
  case CONSTANT_InterfaceMethodref:
    {
      ConstantPoolEntry *entry = ConstantPoolEntry::New(tag, 2);
      entry->InitArg(0, GetU2(offset)); // class_index
      entry->InitArg(1, GetU2(offset)); // name_and_type_index
      return entry;
    }
    break;
  case CONSTANT_String:
    {
      ConstantPoolEntry *entry = ConstantPoolEntry::New(tag, 1);
      entry->InitArg(0, GetU2(offset)); // string_index
      return entry;
    }
    break;
  case CONSTANT_Integer:
    {
      ConstantPoolEntry *entry = ConstantPoolEntry::New(tag, 1);
      entry->InitArg(0, JavaInt::ToWord(GetU4(offset))); // bytes
      return entry;
    }
    break;
  case CONSTANT_Long:
    {
      ConstantPoolEntry *entry = ConstantPoolEntry::New(tag, 1);
      s_int32 high = GetU4(offset);
      s_int32 low = GetU4(offset);
      entry->InitArg(0, JavaLong::New(high, low)->ToWord());
      return entry;
    }
    break;
  case CONSTANT_Float:
    {
      ConstantPoolEntry *entry = ConstantPoolEntry::New(tag, 1);
      u_char s[4];
      for (u_int i = 0; i < 4; i++) s[i] = GetU1(offset);
      Float *flt = Float::NewFromNetworkRepresentation(s);
      entry->InitArg(0, flt->ToWord());
      return entry;
    }
    break;
  case CONSTANT_Double:
    {
      ConstantPoolEntry *entry = ConstantPoolEntry::New(tag, 1);
      u_char s[8];
      for (u_int i = 0; i < 8; i++) s[i] = GetU1(offset);
      Double *dbl = Double::NewFromNetworkRepresentation(s);
      entry->InitArg(0, dbl->ToWord());
      return entry;
    }
    break;
  case CONSTANT_NameAndType:
    {
      ConstantPoolEntry *entry = ConstantPoolEntry::New(tag, 2);
      entry->InitArg(0, GetU2(offset)); // name_index
      entry->InitArg(1, GetU2(offset)); // descriptor_index
      return entry;
    }
    break;
  case CONSTANT_Utf8:
    {
      u_int reprLen = GetU2(offset);
      BaseArray *array =
	BaseArray::New(PrimitiveType::Char, reprLen); // pessimistic assumption
      u_int index = 0;
      u_int reprEnd = offset + reprLen;
      while (offset < reprEnd) {
	u_int x = GetU1(offset);
	if (x & 0x80) {
	  u_int y = GetU1(offset);
	  if ((x & 0xE0) == 0xC0) { // two-byte representation
	    Assert((y & 0xC0) == 0x80);
	    array->StoreChar(index++, ((x & 0x1F) << 6) | (y & 0x3F));
	  } else { // three-byte representation
	    u_int z = GetU1(offset);
	    Assert((x & 0xF0) == 0xE0);
	    Assert((y & 0xC0) == 0x80 && (z & 0xC0) == 0x80);
	    u_int c = ((x & 0xF) << 12) | ((y & 0x3F) << 6) | (z & 0x3F);
	    array->StoreChar(index++, c);
	  }
	} else {
	  Assert(x != 0 && x < 0xF0);
	  array->StoreChar(index++, x);
	}
      }
      JavaString *string = JavaString::New(array, 0, index);
      ConstantPoolEntry *entry = ConstantPoolEntry::New(tag, 2);
      entry->InitArg(0, string->ToWord());
      return entry;
    }
    break;
  default:
    return INVALID_POINTER;
  }
}

RuntimeConstantPool *ClassFile::ResolveConstantPool(ConstantPool *constantPool,
						    ClassLoader *classLoader) {
  u_int constantPoolCount = constantPool->GetSize();
  RuntimeConstantPool *runtimeConstantPool =
    RuntimeConstantPool::New(constantPoolCount);
  for (u_int j = 1; j <= constantPoolCount; j++) {
    word resolved = constantPool->Get(j)->Resolve(constantPool, classLoader);
    runtimeConstantPool->Init(j, resolved);
  }
  return runtimeConstantPool;
}

Table *ClassFile::ParseInterfaces(u_int &offset,
				  RuntimeConstantPool *runtimeConstantPool) {
  u_int interfacesCount = GetU2(offset);
  Table *interfaces = Table::New(interfacesCount);
  for (u_int i = 0; i < interfacesCount; i++)
    interfaces->Init(i, runtimeConstantPool->Get(GetU2(offset)));
  return interfaces;
}

Table *ClassFile::ParseFields(u_int &offset, ConstantPool *constantPool,
			      RuntimeConstantPool *runtimeConstantPool) {
  //--** No two fields in one class file may have the same name and descriptor
  //--** All fields of interfaces must have their ACC_PUBLIC, ACC_STATIC, and
  //--** ACC_FINAL flags set and may not have any of the other flags in
  //--** Table 4.4 set
  u_int fieldsCount = GetU2(offset);
  Table *fields = Table::New(fieldsCount);
  for (u_int i = 0; i < fieldsCount; i++) {
    FieldInfo *fieldInfo =
      ParseFieldInfo(offset, constantPool, runtimeConstantPool);
    if (fieldInfo == INVALID_POINTER) return INVALID_POINTER;
    fields->Init(i, fieldInfo->ToWord());
  }
  return fields;
}

FieldInfo *
ClassFile::ParseFieldInfo(u_int &offset,
			  ConstantPool *constantPool,
			  RuntimeConstantPool *runtimeConstantPool) {
  u_int accessFlags = GetU2(offset);
  if (((accessFlags & FieldInfo::ACC_PUBLIC) != 0) +
      ((accessFlags & FieldInfo::ACC_PRIVATE) != 0) +
      ((accessFlags & FieldInfo::ACC_PROTECTED) != 0) > 1)
    return INVALID_POINTER;
  if (((accessFlags & FieldInfo::ACC_FINAL) != 0) +
      ((accessFlags & FieldInfo::ACC_VOLATILE) != 0) > 1)
    return INVALID_POINTER;
  JavaString *name = constantPool->GetUtf8(GetU2(offset));
  JavaString *descriptor = constantPool->GetUtf8(GetU2(offset));
  FieldInfo *fieldInfo = FieldInfo::New(accessFlags, name, descriptor);
  if (!ParseFieldAttributes(offset, fieldInfo,
			    constantPool, runtimeConstantPool))
    return INVALID_POINTER;
  return fieldInfo;
}

bool
ClassFile::ParseFieldAttributes(u_int &offset, FieldInfo *fieldInfo,
				ConstantPool *constantPool,
				RuntimeConstantPool *runtimeConstantPool) {
  u_int attributesCount = GetU2(offset);
  JavaString *constantValueAttributeName = JavaString::New("ConstantValue");
  bool foundConstantValue = false;
  for (u_int i = attributesCount; i--; ) {
    JavaString *attributeName = constantPool->GetUtf8(GetU2(offset));
    u_int attributeLength = GetU4(offset);
    if (attributeName->Equals(constantValueAttributeName)) {
      //--** is attributeLength != 2 an error or an unknown attribute?
      Assert(attributeLength == 2);
      if (foundConstantValue) return false;
      foundConstantValue = true;
      //if (!fieldInfo->IsStatic()) return false; //--** too strong
      //   (example: java/io/Writer#writeBufferSize)
      //--** must be CONSTANT_Integer, CONSTANT_Long, CONSTANT_Float,
      //--** CONSTANT_Double, or CONSTANT_String
      fieldInfo->InitConstantValue(runtimeConstantPool->Get(GetU2(offset)));
    } else
      offset += attributeLength; // skip info
  }
  return true;
}

Table *ClassFile::ParseMethods(u_int &offset, JavaString *className,
			       ConstantPool *constantPool,
			       RuntimeConstantPool *runtimeConstantPool) {
  //--** No two methods in one class file may have the same name and descriptor
  //--** All interface methods must have their ACC_ABSTRACT and ACC_PUBLIC
  //--** flags set and may not have any of the other flags in Table 4.5 set 
  u_int methodsCount = GetU2(offset);
  Table *methods = Table::New(methodsCount);
  for (u_int i = 0; i < methodsCount; i++) {
    MethodInfo *methodInfo =
      ParseMethodInfo(offset, className, constantPool, runtimeConstantPool);
    if (methodInfo == INVALID_POINTER) return INVALID_POINTER;
    methods->Init(i, methodInfo->ToWord());
  }
  return methods;
}

MethodInfo *
ClassFile::ParseMethodInfo(u_int &offset, JavaString *className,
			   ConstantPool *constantPool,
			   RuntimeConstantPool *runtimeConstantPool) {
  u_int accessFlags = GetU2(offset);
  if (((accessFlags & MethodInfo::ACC_PUBLIC) != 0) +
      ((accessFlags & MethodInfo::ACC_PRIVATE) != 0) +
      ((accessFlags & MethodInfo::ACC_PROTECTED) != 0) > 1)
    return INVALID_POINTER;
  if ((accessFlags & MethodInfo::ACC_ABSTRACT) != 0 &&
      (accessFlags & (MethodInfo::ACC_FINAL | MethodInfo::ACC_NATIVE |
		      MethodInfo::ACC_PRIVATE | MethodInfo::ACC_STATIC |
		      MethodInfo::ACC_STRICT |
		      MethodInfo::ACC_SYNCHRONIZED)) != 0)
    return INVALID_POINTER;
  JavaString *name = constantPool->GetUtf8(GetU2(offset));
  JavaString *descriptor = constantPool->GetUtf8(GetU2(offset));
  MethodInfo *methodInfo =
    MethodInfo::New(accessFlags, className, name, descriptor);
  if (!ParseMethodAttributes(offset, methodInfo, constantPool,
			     runtimeConstantPool))
    return INVALID_POINTER;
  return methodInfo;
}

bool
ClassFile::ParseMethodAttributes(u_int &offset, MethodInfo *methodInfo,
				 ConstantPool *constantPool,
				 RuntimeConstantPool *runtimeConstantPool) {
  //--** implementations must recognize the Exceptions attribute
  u_int attributesCount = GetU2(offset);
  JavaString *codeAttributeName = JavaString::New("Code");
  bool foundByteCode = false;
  for (u_int i = attributesCount; i--; ) {
    JavaString *attributeName = constantPool->GetUtf8(GetU2(offset));
    u_int attributeLength = GetU4(offset);
    if (attributeName->Equals(codeAttributeName)) {
      // Parse Code attribute:
      if (foundByteCode) return false;
      if (methodInfo->IsAbstract() || methodInfo->IsNative()) return false;
      foundByteCode = true;
      u_int startOffset = offset;
      u_int maxStack = GetU2(offset);
      u_int maxLocals = GetU2(offset);
      u_int codeLength = GetU4(offset);
      Chunk *code = Store::AllocChunk(codeLength);
      char *p = code->GetBase();
      for (u_int j = codeLength; j--; ) *p++ = GetU1(offset);
      u_int exceptionTableLength = GetU2(offset);
      Table *exceptionTable = Table::New(exceptionTableLength);
      for (u_int k = 0; k < exceptionTableLength; k++) {
	u_int startPC = GetU2(offset);
	u_int endPC = GetU2(offset);
	u_int handlerPC = GetU2(offset);
	u_int catchType = GetU2(offset);
	ExceptionTableEntry *entry = catchType?
	  ExceptionTableEntry::New(startPC, endPC, handlerPC,
				   runtimeConstantPool->Get(catchType)):
	  ExceptionTableEntry::New(startPC, endPC, handlerPC);
	exceptionTable->Init(k, entry->ToWord());
      }
      SkipAttributes(offset);
      JavaByteCode *byteCode =
	JavaByteCode::New(methodInfo, maxStack, maxLocals, code,
			  exceptionTable);
      methodInfo->InitByteCode(byteCode);
      if (offset != startOffset + attributeLength) return false;
    } else
      offset += attributeLength; // skip info
  }
  return foundByteCode || methodInfo->IsAbstract() || methodInfo->IsNative();
}

void ClassFile::SkipAttributes(u_int &offset) {
  u_int attributesCount = GetU2(offset);
  for (u_int i = attributesCount; i--; ) {
    GetU2(offset); // attributeNameIndex
    u_int attributeLength = GetU4(offset);
    offset += attributeLength; // skip info
  }
}

ClassFile *ClassFile::NewFromFile(JavaString *filename) {
  FILE *f = std::fopen(filename->ExportC(), "rb");
  if (f == NULL) return INVALID_POINTER;
  if (std::fseek(f, 0, SEEK_END)) {
    std::fclose(f);
    return INVALID_POINTER;
  }
  u_int n = std::ftell(f);
  std::rewind(f);
  Chunk *data = Store::AllocChunk(n);
  std::fread(data->GetBase(), 1, n, f);
  std::fclose(f);
  return static_cast<ClassFile *>(data);
}

ClassInfo *ClassFile::Parse(ClassLoader *classLoader) {
  u_int offset = 0;
  if (!ParseMagic(offset)) return INVALID_POINTER;
  if (!ParseVersion(offset))
    return INVALID_POINTER; //--** raise UnsupportedClassVersionError
  ConstantPool *constantPool = ParseConstantPool(offset);
  if (constantPool == INVALID_POINTER) return INVALID_POINTER;
  RuntimeConstantPool *runtimeConstantPool =
    ResolveConstantPool(constantPool, classLoader);
  u_int accessFlags = GetU2(offset);
  //--** check accessFlags validity
  JavaString *name;
  {
    // Retrieve the name of the current class without resolving it:
    ConstantPoolEntry *thisClassEntry = constantPool->Get(GetU2(offset));
    Assert(thisClassEntry->GetTag() == CONSTANT_Class);
    u_int nameIndex = Store::DirectWordToInt(thisClassEntry->GetArg(0));
    ConstantPoolEntry *nameEntry = constantPool->Get(nameIndex);
    Assert(nameEntry->GetTag() == CONSTANT_Utf8);
    name = JavaString::FromWordDirect(nameEntry->GetArg(0));
  }
  u_int superIndex = GetU2(offset);
  word super = superIndex? runtimeConstantPool->Get(superIndex): null;
  //--** For an interface, super must always be the class java/lang/Object
  Table *interfaces = ParseInterfaces(offset, runtimeConstantPool);
  Table *fields = ParseFields(offset, constantPool, runtimeConstantPool);
  if (fields == INVALID_POINTER) return INVALID_POINTER;
  Table *methods =
    ParseMethods(offset, name, constantPool, runtimeConstantPool);
  if (methods == INVALID_POINTER) return INVALID_POINTER;
  SkipAttributes(offset);
  Assert(offset == GetSize());
  return ClassInfo::New(classLoader, accessFlags, name,
			super, interfaces, fields, methods,
			runtimeConstantPool);
}
