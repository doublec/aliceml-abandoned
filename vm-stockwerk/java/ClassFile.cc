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

bool ClassFile::ParseMagic(u_int &offset) {
  return GetU4(offset) == MAGIC;
}

bool ClassFile::ParseVersion(u_int &offset) {
  u_int minor = GetU2(offset);
  u_int major = GetU2(offset);
  return
    (major >= SupportedVersion.majorMin ||
     (major == SupportedVersion.majorMin &&
      minor >= SupportedVersion.minorMin)) &&
    (major <= SupportedVersion.majorMax ||
     (major == SupportedVersion.majorMax &&
      minor <= SupportedVersion.minorMax));
}

ConstantPool *ClassFile::ParseConstantPool(u_int &offset) {
  u_int constantPoolCount = GetU2(offset);
  ConstantPool *constantPool = ConstantPool::New(constantPoolCount - 1);
  for (u_int i = 1; i < constantPoolCount; i++)
    constantPool->Init(i, ParseConstantPoolEntry(offset));
  return constantPool;
}

word ClassFile::ParseConstantPoolEntry(u_int &offset) {
  switch (GetU1(offset)) {
  case CONSTANT_Class:
    {
      u_int nameIndex = GetU2(offset);
      //--**
    }
    break;
  case CONSTANT_Fieldref:
    {
      u_int classIndex = GetU2(offset);
      u_int nameAndTypeIndex = GetU2(offset);
      //--**
    }
    break;
  case CONSTANT_Methodref:
    {
      u_int classIndex = GetU2(offset);
      u_int nameAndTypeIndex = GetU2(offset);
      //--**
    }
    break;
  case CONSTANT_InterfaceMethodref:
    {
      u_int classIndex = GetU2(offset);
      u_int nameAndTypeIndex = GetU2(offset);
      //--**
    }
    break;
  case CONSTANT_String:
    {
      u_int stringIndex = GetU2(offset);
      //--**
    }
    break;
  case CONSTANT_Integer:
    {
      u_int bytes = GetU4(offset);
      //--**
    }
    break;
  case CONSTANT_Float:
  case CONSTANT_Long:
  case CONSTANT_Double:
    Error("unimplemented constant pool tag"); //--**
  case CONSTANT_NameAndType:
    {
      u_int nameIndex = GetU2(offset);
      u_int descriptorIndex = GetU2(offset);
      //--**
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
	u_int x = GetU1();
	if (x & 0x80) {
	  u_int y = GetU1();
	  if ((x & 0xE0) == 0xC0) { // two-byte representation
	    Assert((y & 0xC0) == 0x80);
	    p[index++] = ((x & 0x1F) << 6) | (y & 0x3F);
	  } else {
	    u_int z = GetU1();
	    Assert((x & 0xF0) == 0xE0);
	    Assert((y & 0xC0) == 0x80 && (z & 0xC0) == 0x80);
	    p[index++] = ((x & 0xF) << 12) | ((y & 0x3F) << 6) | (z & 0x3F);
	  }
	} else
	  p[index++] = x;
      }
      if (index < reprLen) string = JavaString::New(p, index);
      //--** store string
    }
    break;
  default:
    Error("unknown constant pool tag"); //--** raise exception
  }
}

ClassFile *ClassFile::NewFromFile(char *filename) {
  //--**
}

ClassInfo *ClassFile::Parse() {
  u_int offset = 0;
  if (!ParseMagic(offset)) return INVALID_POINTER;
  if (!ParseVersion(offset)) return INVALID_POINTER;
  ConstantPool *constantPool = ParseConstantPool(offset);
  //--**
}
