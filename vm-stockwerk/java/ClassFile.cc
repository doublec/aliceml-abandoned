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

#include "java/ClassFile.hh"

enum CONSTANT_tag {
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
  CONSTANT_Utf8               = 1
};

class ConstantPoolEntry: private Block {
public:
  using Block::ToWord;
  using Block::InitArg;

  static ConstantPoolEntry *New(CONSTANT_tag tag, u_int size) {
    return static_cast<ConstantPoolEntry *>
      (Store::AllocBlock(static_cast<BlockLabel>(MIN_DATA_LABEL + tag), size));
  }
  static ConstantPoolEntry *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    //--** Assert label
    return static_cast<ConstantPoolEntry *>(b);
  }

  CONSTANT_tag GetTag() {
    return static_cast<CONSTANT_tag>(static_cast<int>(GetLabel()));
  }
  word Fixup(ConstantPool *);
};

word ConstantPoolEntry::Fixup(ConstantPool *constantPool) {
  switch (GetTag()) {
  case CONSTANT_Class:
    {
      JavaString *name =
	constantPool->GetString(Store::DirectWordToInt(GetArg(0)));
      //--** return a Class
      Error("Fixup CONSTANT_Class not implemented");
    }
  case CONSTANT_Fieldref:
    {
      JavaString *className =
	constantPool->GetString(Store::DirectWordToInt(GetArg(0)));
      word nameAndType = constantPool->Get(Store::DirectWordToInt(GetArg(1)));
      //--** return a StaticFieldRef/InstanceFieldRef
      Error("Fixup CONSTANT_Fieldref not implemented");
    }
  case CONSTANT_Methodref:
    {
      JavaString *className =
	constantPool->GetString(Store::DirectWordToInt(GetArg(0)));
      word nameAndType = constantPool->Get(Store::DirectWordToInt(GetArg(1)));
      //--** return a StaticMethodRef/VirtualMethodRef
      Error("Fixup CONSTANT_Methodref not implemented");
    }
  case CONSTANT_InterfaceMethodref:
    {
      JavaString *className =
	constantPool->GetString(Store::DirectWordToInt(GetArg(0)));
      word nameAndType = constantPool->Get(Store::DirectWordToInt(GetArg(1)));
      //--** return an InterfaceMethodRef
      Error("Fixup CONSTANT_InterfaceMethodref not implemented");
    }
  case CONSTANT_String:
    return
      constantPool->GetString(Store::DirectWordToInt(GetArg(0)))->ToWord();
  case CONSTANT_Integer:
    return GetArg(0);
  case CONSTANT_Float:
  case CONSTANT_Long:
  case CONSTANT_Double:
    Error("unimplemented CONSTANT_tag"); //--**
  case CONSTANT_NameAndType:
    {
      JavaString *name =
	constantPool->GetString(Store::DirectWordToInt(GetArg(0)));
      JavaString *descriptor =
	constantPool->GetString(Store::DirectWordToInt(GetArg(1)));
      //--** return a NameAndType
      Error("Fixup CONSTANT_InterfaceMethodref not implemented");
    }
  case CONSTANT_Utf8:
    return
      constantPool->GetString(Store::DirectWordToInt(GetArg(0)))->ToWord();
  default:
    Error("unknown constant pool tag"); //--** raise exception
  }
}

//
// ClassFile Method Implementations
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
  for (u_int i = 1; i < constantPoolCount; i++)
    constantPool->Init(i, ParseConstantPoolEntry(offset)->ToWord());
  for (u_int j = 1; j < constantPoolCount; j++) {
    ConstantPoolEntry *entry =
      ConstantPoolEntry::FromWordDirect(constantPool->Get(j));
    constantPool->Init(j, entry->Fixup(constantPool));
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
      entry->InitArg(0, GetU4(offset)); // bytes
      return entry;
    }
    break;
  case CONSTANT_Float:
  case CONSTANT_Long:
  case CONSTANT_Double:
    Error("unimplemented CONSTANT_tag"); //--**
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
      JavaString *string = JavaString::New(reprLen); // pessimistic assumption
      u_wchar *p = string->GetBase();
      u_int index = 0;
      u_int reprEnd = offset + reprLen;
      while (offset < reprEnd) {
	u_int x = GetU1(offset);
	if (x & 0x80) {
	  u_int y = GetU1(offset);
	  if ((x & 0xE0) == 0xC0) { // two-byte representation
	    Assert((y & 0xC0) == 0x80);
	    p[index++] = ((x & 0x1F) << 6) | (y & 0x3F);
	  } else {
	    u_int z = GetU1(offset);
	    Assert((x & 0xF0) == 0xE0);
	    Assert((y & 0xC0) == 0x80 && (z & 0xC0) == 0x80);
	    p[index++] = ((x & 0xF) << 12) | ((y & 0x3F) << 6) | (z & 0x3F);
	  }
	} else
	  p[index++] = x;
      }
      if (index < reprLen) string = JavaString::New(p, index);
      ConstantPoolEntry *entry = ConstantPoolEntry::New(tag, 2);
      entry->InitArg(0, string->ToWord());
      return entry;
    }
    break;
  default:
    Error("unknown constant pool tag"); //--** raise exception
  }
}

Array *ClassFile::ParseInterfaces(u_int &offset, ConstantPool *constantPool) {
  u_int interfacesCount = GetU2(offset);
  Array *interfaces = Array::New(interfacesCount);
  for (u_int i = 0; i < interfacesCount; i++)
    interfaces->Assign(i, constantPool->Get(GetU2(offset)));
  return interfaces;
}

Array *ClassFile::ParseFields(u_int &offset, ConstantPool *constantPool) {
  u_int fieldsCount = GetU2(offset);
  Array *fields = Array::New(fieldsCount);
  for (u_int i = 0; i < fieldsCount; i++)
    fields->Assign(i, ParseFieldInfo(offset, constantPool)->ToWord());
  return fields;
}

FieldInfo *ClassFile::ParseFieldInfo(u_int &offset,
				     ConstantPool *constantPool) {
  u_int accessFlags = GetU2(offset);
  JavaString *name = constantPool->GetString(GetU2(offset));
  JavaString *descriptor = constantPool->GetString(GetU2(offset));
  SkipAttributes(offset); //--** need to parse some of them
  return FieldInfo::New(accessFlags, name, descriptor);
}

Array *ClassFile::ParseMethods(u_int &offset, ConstantPool *constantPool) {
  u_int methodsCount = GetU2(offset);
  Array *methods = Array::New(methodsCount);
  for (u_int i = 0; i < methodsCount; i++)
    methods->Assign(i, ParseMethodInfo(offset, constantPool)->ToWord());
  return methods;
}

MethodInfo *ClassFile::ParseMethodInfo(u_int &offset,
				       ConstantPool *constantPool) {
  u_int accessFlags = GetU2(offset);
  JavaString *name = constantPool->GetString(GetU2(offset));
  JavaString *descriptor = constantPool->GetString(GetU2(offset));
  SkipAttributes(offset); //--** need to parse some of them
  return MethodInfo::New(accessFlags, name, descriptor);
}

void ClassFile::SkipAttributes(u_int &offset) {
  GetU2(offset); // attributeNameIndex
  u_int attributeLength = GetU4(offset);
  offset += attributeLength; // skip info
}

ClassFile *ClassFile::NewFromFile(char *filename) {
  //--**
}

ClassInfo *ClassFile::Parse() {
  u_int offset = 0;
  if (!ParseMagic(offset)) return INVALID_POINTER;
  if (!ParseVersion(offset)) return INVALID_POINTER;
  ConstantPool *constantPool = ParseConstantPool(offset);
  u_int accessFlags = GetU2(offset);
  u_int thisClassIndex = GetU2(offset);
  ClassInfo *super = constantPool->GetClassInfo(GetU2(offset));
  Array *interfaces = ParseInterfaces(offset, constantPool);
  Array *fields = ParseFields(offset, constantPool);
  Array *methods = ParseMethods(offset, constantPool);
  SkipAttributes(offset); //--** need to parse some of them
  return ClassInfo::New(accessFlags, 0, //--** name
			super, interfaces, fields, methods, constantPool);
}
