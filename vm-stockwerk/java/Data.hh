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

#include "store/Store.hh"

class JavaLabel {
public:
  static const BlockLabel Array               = MIN_DATA_LABEL;
  static const BlockLabel Lock                = (BlockLabel) (Array + 1);
  static const BlockLabel FieldInfo           = (BlockLabel) (Array + 2);
  static const BlockLabel ExceptionTableEntry = (BlockLabel) (Array + 3);
  static const BlockLabel JavaByteCode        = (BlockLabel) (Array + 4);
  static const BlockLabel MethodInfo          = (BlockLabel) (Array + 5);
  static const BlockLabel ClassInfo           = (BlockLabel) (Array + 6);
  static const BlockLabel Class               = (BlockLabel) (Array + 7);
  static const BlockLabel Object              = (BlockLabel) (Array + 8);
};

static const word null = Store::IntToWord(0);

class Class;

class Array: private Block {
protected:
  enum {
    CLASS_POS, // Class
    SIZE_POS, // int
    BASE_SIZE
  };
public:
  static Array *New(word type, u_int length) {
    //--** represent arrays depending on type
    Block *b = Store::AllocBlock(JavaLabel::Array, BASE_SIZE + length);
    b->InitArg(CLASS_POS, type);
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
};

class Lock: private Block {
  // to be determined
};

class Float: public Chunk {
  // see Alice
};

class FieldInfo: private Block {
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
    NAME_POS, // String
    TYPE_POS, // String
    SIZE
  };
};

class ExceptionTableEntry: private Block {
protected:
  enum {
    START_PC_POS, // int
    END_PC_POS, // int
    HANDLER_PC_POS, // int
    CATCH_TYPE_POS, // Class | int(0)
    SIZE
  };
};

class JavaByteCode: private Block {
protected:
  enum {
    MAX_STACK_POS, // int
    MAX_LOCALS_POS, // int
    CODE_POS, // Chunk
    EXCEPTION_TABLE_POS, // Array(ExceptionTableEntry)
    SIZE
  };
};

class MethodInfo: private Block {
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
    NAME_POS, // String
    TYPE_POS, // String
    BYTE_CODE_POS, // JavaByteCode | int(0)
    SIZE
  };
};

class ClassInfo: private Block {
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
    NAME_POS, // String
    SUPER_POS, // Class
    INTERFACES_POS, // Array(Class)
    FIELDS_POS, // Array(FieldInfo)
    METHODS_POS, // Array(MethodInfo)
    BASE_SIZE
  };
};

class Class: public ClassInfo {
protected:
  enum {
    VIRTUAL_TABLE_POS, // Block(Closure)
    LOCK_POS,
    SIZE
  };
};

class Object: private Block {
protected:
  enum {
    CLASS_POS, // Class
    BASE_SIZE
  };
};
