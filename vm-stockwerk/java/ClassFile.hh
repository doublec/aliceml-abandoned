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

#ifndef __JAVA__CLASS_FILE_HH__
#define __JAVA__CLASS_FILE_HH__

#if defined(INTERFACE)
#pragma interface "java/ClassFile.hh"
#endif

#include <cstdlib>
#include "java/ClassInfo.hh"

class ConstantPool;
class ConstantPoolEntry;
class ClassLoader;

class DllExport ClassFile: private Chunk {
private:
  struct SupportedVersion {
    static const u_int majorMin = 45;
    static const u_int minorMin = 0;
    static const u_int majorMax = 46;
    static const u_int minorMax = 0;
  };

  static const u_int MAGIC = 0xCAFEBABE;

  u_char *GetBase() {
    return reinterpret_cast<u_char *>(Chunk::GetBase());
  }
  u_char GetU1(u_int &offset) {
    return GetBase()[offset++];
  }
  u_int GetU2(u_int &offset) {
    u_char *p = GetBase();
    u_char a = p[offset++];
    return (a << 8) | p[offset++];
  }
  u_int GetU4(u_int &offset) {
    u_int a = GetU2(offset);
    return (a << 16) | GetU2(offset);
  }

  bool ParseMagic(u_int &offset);
  bool ParseVersion(u_int &offset);
  ConstantPool *ParseConstantPool(u_int &offset);
  ConstantPoolEntry *ParseConstantPoolEntry(u_int &offset);
  Array *ResolveConstantPool(ConstantPool *constantPoolS,
			     ClassLoader *classLoader);
  Array *ParseInterfaces(u_int &offset, Array *constantPoolR);
  Array *ParseFields(u_int &offset, ConstantPool *constantPoolS,
		     Array *constantPoolR);
  FieldInfo *ParseFieldInfo(u_int &offset, ConstantPool *constantPoolS,
			    Array *constantPoolR);
  bool ParseFieldAttributes(u_int &offset, ConstantPool *constantPoolS,
			    Array *constantPoolR, word &constantValue);
  Array *ParseMethods(u_int &offset, ConstantPool *constantPoolS,
		      Array *constantPoolR);
  MethodInfo *ParseMethodInfo(u_int &offset, ConstantPool *constantPoolS,
			      Array *constantPoolR);
  bool ParseMethodAttributes(u_int &offset, ConstantPool *constantPoolS,
			     Array *constantPoolR, JavaByteCode *&byteCode);
  void SkipAttributes(u_int &offset);
public:
  using Chunk::ToWord;

  static ClassFile *NewFromFile(char *filename);

  ClassInfo *Parse(ClassLoader *classLoader);
};

#endif
