//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "java/ByteCodeInterpreter.hh"
#endif

#include <cstdio>
#include "generic/Scheduler.hh"
#include "generic/Backtrace.hh"
#include "generic/Closure.hh"
#include "generic/ConcreteCode.hh"

#include "java/Data.hh"
#include "java/StackFrame.hh"
#include "java/JavaByteCode.hh"
#include "java/ByteCodeInterpreter.hh"

//
// Interpreter Opcodes
//
class Instr {
public:
  enum Opcode {
    AALOAD          = 0x32,
    AASTORE         = 0x53,
    ACONST_NULL     = 0x01,
    ALOAD           = 0x19,
    ALOAD_0         = 0x2a,
    ALOAD_1         = 0x2b,
    ALOAD_2         = 0x2c,
    ALOAD_3         = 0x2d,
    ANEWARRAY       = 0xbd,
    ARETURN         = 0xb0,
    ARRAYLENGTH     = 0xbe,
    ASTORE          = 0x3a,
    ASTORE_0        = 0x4b,
    ASTORE_1        = 0x4c,
    ASTORE_2        = 0x4d,
    ASTORE_3        = 0x4e,
    ATHROW          = 0xbf,
    BALOAD          = 0x33,
    BASTORE         = 0x54,
    BIPUSH          = 0x10,
    CALOAD          = 0x34,
    CASTORE         = 0x55,
    CHECKCAST       = 0xc0,
    D2F             = 0x90,
    D2I             = 0x8e,
    D2L             = 0x8f,
    DADD            = 0x63,
    DALOAD          = 0x31,
    DASTORE         = 0x52,
    DCMPG           = 0x98,
    DCMPL           = 0x97,
    DCONST_0        = 0x0e,
    DCONST_1        = 0x0f,
    DDIV            = 0x6f,
    DLOAD           = 0x18,
    DLOAD_0         = 0x26,
    DLOAD_1         = 0x27,
    DLOAD_2         = 0x28,
    DLOAD_3         = 0x29,
    DMUL            = 0x6b,
    DNEG            = 0x77,
    DREM            = 0x73,
    DRETURN         = 0xaf,
    DSTORE          = 0x39,
    DSTORE_0        = 0x47,
    DSTORE_1        = 0x48,
    DSTORE_2        = 0x49,
    DSTORE_3        = 0x4A,
    DSUB            = 0x67,
    DUP             = 0x59,
    DUP_X1          = 0x5a,
    DUP_X2          = 0x5b,
    DUP2            = 0x5c,
    DUP2_X1         = 0x5d,
    DUP2_X2         = 0x5e,
    F2D             = 0x8d,
    F2I             = 0x8b,
    F2L             = 0x8c,
    FADD            = 0x62,
    FALOAD          = 0x30,
    FASTORE         = 0x51,
    FCMPG           = 0x96,
    FCMPL           = 0x95,
    FCONST_0        = 0x0b,
    FCONST_1        = 0x0c,
    FCONST_2        = 0x0d,
    FDIV            = 0x6e,
    FLOAD           = 0x17,
    FLOAD_0         = 0x22,
    FLOAD_1         = 0x23,
    FLOAD_2         = 0x24,
    FLOAD_3         = 0x25,
    FMUL            = 0x6a,
    FNEG            = 0x76,
    FREM            = 0x72,
    FRETURN         = 0xae,
    FSTORE          = 0x38,
    FSTORE_0        = 0x43,
    FSTORE_1        = 0x44,
    FSTORE_2        = 0x45,
    FSTORE_3        = 0x46,
    FSUB            = 0x66,
    GETFIELD        = 0xb4,
    GETSTATIC       = 0xb2,
    GOTO            = 0xa7,
    GOTO_W          = 0xc8,
    I2B             = 0x91,
    I2C             = 0x92,
    I2D             = 0x87,
    I2F             = 0x86,
    I2L             = 0x85,
    I2S             = 0x93,
    IADD            = 0x60,
    IALOAD          = 0x2e,
    IAND            = 0x7e,
    IASTORE         = 0x4f,
    ICONST_M1       = 0x02,
    ICONST_0        = 0x03,
    ICONST_1        = 0x04,
    ICONST_2        = 0x05,
    ICONST_3        = 0x06,
    ICONST_4        = 0x07,
    ICONST_5        = 0x08,
    IDIV            = 0x6c,
    IF_ACMPEQ       = 0xa5,
    IF_ACMPNE       = 0xa6,
    IF_ICMPEQ       = 0x9f,
    IF_ICMPNE       = 0xa0,
    IF_ICMPLT       = 0xa1,
    IF_ICMPGE       = 0xa2,
    IF_ICMPGT       = 0xa3,
    IF_ICMPLE       = 0xa4,
    IFEQ            = 0x99,
    IFNE            = 0x9a,
    IFLT            = 0x9b,
    IFGE            = 0x9c,
    IFGT            = 0x9d,
    IFLE            = 0x9e,
    IFNONNULL       = 0xc7,
    IFNULL          = 0xc6,
    IINC            = 0x84,
    ILOAD           = 0x15,
    ILOAD_0         = 0x1a,
    ILOAD_1         = 0x1b,
    ILOAD_2         = 0x1c,
    ILOAD_3         = 0x1d,
    IMUL            = 0x68,
    INEG            = 0x74,
    INSTANCEOF      = 0xc1,
    INVOKEINTERFACE = 0xb9,
    INVOKESPECIAL   = 0xb7,
    INVOKESTATIC    = 0xb8,
    INVOKEVIRTUAL   = 0xb6,
    IOR             = 0x80,
    IREM            = 0x70,
    IRETURN         = 0xac,
    ISHL            = 0x78,
    ISHR            = 0x7a,
    ISTORE          = 0x36,
    ISTORE_0        = 0x3b,
    ISTORE_1        = 0x3c,
    ISTORE_2        = 0x3d,
    ISTORE_3        = 0x3e,
    ISUB            = 0x64,
    IUSHR           = 0x7c,
    IXOR            = 0x82,
    JSR             = 0xa8,
    JSR_W           = 0xc9,
    L2D             = 0x8a,
    L2F             = 0x89,
    L2I             = 0x88,
    LADD            = 0x61,
    LALOAD          = 0x2f,
    LAND            = 0x7f,
    LASTORE         = 0x50,
    LCMP            = 0x94,
    LCONST_0        = 0x09,
    LCONST_1        = 0x0a,
    LDC             = 0x12,
    LDC_W           = 0x13,
    LDC2_W          = 0x14,
    LDIV            = 0x6d,
    LLOAD           = 0x16,
    LLOAD_0         = 0x1e,
    LLOAD_1         = 0x1f,
    LLOAD_2         = 0x20,
    LLOAD_3         = 0x21,
    LMUL            = 0x69,
    LNEG            = 0x75,
    LOOKUPSWITCH    = 0xab,
    LOR             = 0x81,
    LREM            = 0x71,
    LRETURN         = 0xad,
    LSHL            = 0x79,
    LSHR            = 0x7b,
    LSTORE          = 0x37,
    LSTORE_0        = 0x3f,
    LSTORE_1        = 0x40,
    LSTORE_2        = 0x41,
    LSTORE_3        = 0x42,
    LSUB            = 0x65,
    LUSHR           = 0x7d,
    LXOR            = 0x83,
    MONITORENTER    = 0xc2,
    MONITOREXIT     = 0xc3,
    MULTIANEWARRAY  = 0xc5,
    NEW             = 0xbb,
    NEWARRAY        = 0xbc,
    NOP             = 0x00,
    POP             = 0x57,
    POP2            = 0x58,
    PUTFIELD        = 0xb5,
    PUTSTATIC       = 0xb3,
    RET             = 0xa9,
    RETURN          = 0xb1,
    SALOAD          = 0x35,
    SASTORE         = 0x56,
    SIPUSH          = 0x11,
    SWAP            = 0x5f,
    TABLESWITCH     = 0xaa,
    WIDE            = 0xc4
  };
};

//
// Interpreter StackFrames
//
class ByteCodeFrame : public StackFrame {
protected:
  enum {
    PC_POS,
    CONT_PC_POS,
    TOP_POS,
    POOL_POS,
    EXN_TABLE_POS,
    CODE_POS,
    BASE_SIZE
  };
  
  void SetTop(u_int top) {
    StackFrame::InitArg(TOP_POS, Store::IntToWord(top));
  }
  u_int GetTop() {
    return Store::DirectWordToInt(StackFrame::GetArg(TOP_POS));
  }
public:
  using Block::ToWord;

  // ByteCodeFrame Accessors
  int GetPC() {
    return Store::DirectWordToInt(StackFrame::GetArg(PC_POS));
  }
  void SetPC(int pc) {
    StackFrame::InitArg(PC_POS, Store::IntToWord(pc));
  }
  int GetContPC() {
    return Store::DirectWordToInt(StackFrame::GetArg(CONT_PC_POS));
  }
  void SetContPC(int pc) {
    StackFrame::InitArg(CONT_PC_POS, Store::IntToWord(pc));
  }
  Chunk *GetCode() {
    return Store::DirectWordToChunk(StackFrame::GetArg(CODE_POS));
  }
  RuntimeConstantPool *GetRuntimeConstantPool() {
    return RuntimeConstantPool::FromWordDirect(StackFrame::GetArg(POOL_POS));
  }
  Table *GetExceptionTable() {
    return Table::FromWordDirect(StackFrame::GetArg(EXN_TABLE_POS));
  }
  // Environment Accessors
  word GetEnv(u_int i) {
    return StackFrame::GetArg(BASE_SIZE + i);
  }
  void SetEnv(u_int i, word value) {
    StackFrame::ReplaceArg(BASE_SIZE + i, value);
  }
  // Stack Accessors
  word Pop() {
    u_int curTop = GetTop();
    SetTop(curTop - 1);
    return StackFrame::GetArg(curTop - 1);
  }
  void Push(word value) {
    u_int curTop = GetTop();
    SetTop(curTop + 1);
    StackFrame::ReplaceArg(curTop, value);
  }
  // ByteCodeFrame Constructor
  static ByteCodeFrame *New(Interpreter *interpreter,
			    word pc,
			    word contPC,
			    Chunk *code,
			    word runtimeConstantPool,
			    Table *exnTable,
			    u_int nLocals,
			    u_int maxStack) {
    u_int frSize = BASE_SIZE + nLocals + maxStack;
    StackFrame *frame =
      StackFrame::New(JAVA_BYTE_CODE_FRAME, interpreter, frSize);
    frame->InitArg(PC_POS, pc);
    frame->InitArg(CONT_PC_POS, contPC);
    frame->InitArg(TOP_POS, Store::IntToWord(BASE_SIZE + nLocals));
    frame->InitArg(CODE_POS, code->ToWord());
    frame->InitArg(POOL_POS, runtimeConstantPool);
    frame->InitArg(EXN_TABLE_POS, exnTable->ToWord());
    return static_cast<ByteCodeFrame *>(frame);
  }
  // ByteCodeFrame Untagging
  static ByteCodeFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == JAVA_BYTE_CODE_FRAME);
    return static_cast<ByteCodeFrame *>(p);
  }
};

//
// Interpreter Types
//
class JavaInt {
public:
  static word ToWord(int value) {
    return Store::IntToWord(value);
  }
  static int FromWord(word value) {
    return Store::DirectWordToInt(value);
  }
  static word Zero() {
    return Store::IntToWord(0);
  }
  static word Deref(word a) {
    return a;
  }
};

// to be done: this should reside in data
class JavaByteArray : public Chunk {
public:
  unsigned char Get(u_int i) {
    return ((unsigned char *) GetBase())[i];
  }
  void Put(u_int i, unsigned char value) {
    ((unsigned char *) GetBase())[i] = value;
  }
  static JavaByteArray *New(u_int size) {
    return static_cast<JavaByteArray *>(Store::AllocChunk(size));
  }
  static JavaByteArray *FromWord(word array) {
    return static_cast<JavaByteArray *>(Store::WordToChunk(array));
  }
};

//
// Helper Stuff
//
#define GET_BYTE_INDEX() \
  (code[pc + 1])

#define GET_POOL_INDEX() \
  ((code[pc + 1] << 8) | code[pc + 2])

// to be done
#define GET_POOL_VALUE(index) \
  (pool->Get(index))

#define GET_WIDE_INDEX() \
  ((code[pc + 1] << 24) | (code[pc + 2] << 16) \
  | (code[pc + 3] << 8) | code[pc + 4])

#define REQUEST(w) {	      \
  frame->SetPC(pc);           \
  Scheduler::currentData = w; \
  Scheduler::nArgs = 0;	      \
  return Worker::REQUEST;     \
}

#define CHECK_PREEMPT() {			\
  if (StatusWord::GetStatus() != 0)		\
    return Worker::PREEMPT;			\
  else						\
    return Worker::CONTINUE;			\
}

class JavaDebug {
public:
#if defined(STORE_DEBUG)
  static void Print(const char *s) {
    fprintf(stderr, "%s\n", s);
  }
#else
  static void Print(char *) {}
#endif
};


//
// Interpreter Functions
//
ByteCodeInterpreter *ByteCodeInterpreter::self;

Block *
ByteCodeInterpreter::GetAbstractRepresentation(ConcreteRepresentation *) {
  return NULL; // to be done
}

void ByteCodeInterpreter::PushCall(Closure *closure) {
  JavaByteCode *concreteCode =
    JavaByteCode::FromWord(closure->GetConcreteCode());
  ByteCodeFrame *frame =
    ByteCodeFrame::New(ByteCodeInterpreter::self,
		       Store::IntToWord(-1),
		       Store::IntToWord(0),
		       concreteCode->GetCode(),
		       closure->Sub(0), // RuntimeConstantPool
		       concreteCode->GetExceptionTable(),
		       concreteCode->GetMaxLocals(),
		       concreteCode->GetMaxStack());
  Scheduler::PushFrame(frame->ToWord());
}

Worker::Result ByteCodeInterpreter::Run() {
  ByteCodeFrame *frame = ByteCodeFrame::FromWordDirect(Scheduler::GetFrame());
  int pc = frame->GetPC();
  unsigned char *code = (unsigned char *) frame->GetCode()->GetBase();
  RuntimeConstantPool *pool = frame->GetRuntimeConstantPool();
  // to be done: more efficient solution
  if (pc == -1) {
    // Copy arguments to local variables
    // to be done: support large arguments
    for (u_int i = Scheduler::nArgs; i--;)
      frame->SetEnv(i, Scheduler::currentArgs[i]);
    pc = frame->GetContPC();
  }
  else if (pc == -2) {
    // Push Return Value to stack
    if (Scheduler::nArgs != 0)
      frame->Push(Scheduler::currentArgs[0]);
    pc = frame->GetContPC();
  }
  while (1) {
    switch (static_cast<Instr::Opcode>(code[pc])) {
    case Instr::AALOAD:
    case Instr::CALOAD:
    case Instr::DALOAD: // reals are boxed
    case Instr::FALOAD: // reals are boxed
    case Instr::IALOAD:
    case Instr::LALOAD:
    case Instr::SALOAD:
      {
	JavaDebug::Print("(A|C|D||F|I|L|S)ASTORE");
	u_int index        = JavaInt::FromWord(frame->Pop());
	ObjectArray *array = ObjectArray::FromWord(frame->Pop());
	if (array != INVALID_POINTER) {
	  if (index < array->GetLength())
	    frame->Push(array->Get(index));
	  else {
	    // to be done: raise ArrayIndexOutOfBoundsException
	    Error("ArrayIndexOutOfBoundsException");
	  }
	}
	else {
	  // to be done: raise NullPointerException
	  Error("NullPointerException");
	}
	pc += 1;
      }
      break;
    case Instr::AASTORE:
      {
	JavaDebug::Print("AASTORE");
	word value         = frame->Pop();
	u_int index        = Store::DirectWordToInt(frame->Pop());
	ObjectArray *array = ObjectArray::FromWord(frame->Pop());
	if (array != INVALID_POINTER) {
	  if (index < array->GetLength()) {
	    Object *object = Object::FromWord(value);
	    Assert(object != INVALID_POINTER);
	    // to be done: array->GetType();
	    Type *type     = INVALID_POINTER;
	    if ((type->GetLabel() == JavaLabel::Class) &&
		object->IsInstanceOf(static_cast<Class *>(type)))
	      array->Assign(index, value);
	    else {
	      // to be done: raise ArrayStoreException
	      Error("ArrayStoreException");
	    }
	  }
	  else {
	    // to be done: raise ArrayIndexOutOfBoundsException
	    Error("ArrayIndexOutOfBoundsException");
	  }
	}
	else {
	  // to be done: raise NullPointerException
	  Error("NullPointerException");
	}
	pc += 1;
      }
      break;
    case Instr::CASTORE:
    case Instr::DASTORE: // reals are boxed
    case Instr::FASTORE: // reals are boxed
    case Instr::IASTORE:
    case Instr::LASTORE:
    case Instr::SASTORE:
      {
	JavaDebug::Print("(C|D|F|I|L|S)ASTORE");
	word value         = frame->Pop();
	u_int index        = Store::DirectWordToInt(frame->Pop());
	ObjectArray *array = ObjectArray::FromWord(frame->Pop());
	if (array != INVALID_POINTER) {
	  if (index < array->GetLength())
	    array->Assign(index, value);
	  else {
	    // to be done: raise ArrayIndexOutOfBoundsException
	    Error("ArrayIndexOutOfBoundsException");
	  }
	}
	else {
	  // to be done: raise NullPointerException
	  Error("NullPointerException");
	}
	pc += 1;
      }
      break;
    case Instr::ACONST_NULL:
      {
	JavaDebug::Print("ACONST_NULL");
	frame->Push(null);
	pc += 1;
      }
      break;
    case Instr::ALOAD:
    case Instr::DLOAD: // reals are boxed
    case Instr::FLOAD: // reals are boxed
    case Instr::ILOAD:
    case Instr::LLOAD:
      {
	JavaDebug::Print("(A|D|F|I|L)LOAD");
	word value = frame->GetEnv((u_int) GET_BYTE_INDEX()); 
	frame->Push(value);
	pc += 2;
      }
      break;
    case Instr::ALOAD_0:
    case Instr::DLOAD_0:
    case Instr::FLOAD_0:
    case Instr::ILOAD_0:
    case Instr::LLOAD_0:
      {
	JavaDebug::Print("(A|D|F|I|L)LOAD_0");
	word value = frame->GetEnv(0);
	frame->Push(value);
	pc += 1;
      }
      break;
    case Instr::ALOAD_1:
    case Instr::DLOAD_1:
    case Instr::FLOAD_1:
    case Instr::ILOAD_1:
    case Instr::LLOAD_1:
      {
	JavaDebug::Print("(A|D|F|I|L)LOAD_1");
	word value = frame->GetEnv(1);
	frame->Push(value);
	pc += 1;
      }
      break;
    case Instr::ALOAD_2:
    case Instr::DLOAD_2:
    case Instr::FLOAD_2:
    case Instr::ILOAD_2:
    case Instr::LLOAD_2:
      {
	JavaDebug::Print("(A|D|F|I|L)LOAD_2");
	word value = frame->GetEnv(2);
	frame->Push(value);
	pc += 1;
      }
      break;
    case Instr::ALOAD_3:
    case Instr::DLOAD_3:
    case Instr::FLOAD_3:
    case Instr::ILOAD_3:
    case Instr::LLOAD_3:
      {
	JavaDebug::Print("(A|D|F|I|L)LOAD_3");
	word value = frame->GetEnv(3);
	frame->Push(value);
	pc += 1;
      }
      break;
    case Instr::ANEWARRAY:
      {
	JavaDebug::Print("ANEWARRAY");
	word wType = GET_POOL_VALUE(GET_POOL_INDEX());
	Type *type = Type::FromWord(wType);
	if (type == INVALID_POINTER)
	  REQUEST(wType);
	int count = JavaInt::FromWord(frame->Pop());
	if (count >= 0) {
	  Type *arrayType = static_cast<Type *>
	    (ArrayType::New(type->ToWord()));
	  ObjectArray *arr = ObjectArray::New(arrayType, count);
	  for (u_int i = count; i--;)
	    arr->Init(i, null);
	  frame->Push(arr->ToWord());
	}
	else {
	  // to be done: raise NegativeArraySizeException
	  Error("NegativeArraySizeException");
	}
      }
      pc += 3;
      break;
    case Instr::ARETURN:
    case Instr::DRETURN: // reals are boxed
    case Instr::FRETURN: // reals are boxed
    case Instr::IRETURN:
    case Instr::LRETURN:
      {
	JavaDebug::Print("(A|D|F|I|L)RETURN");
	Scheduler::nArgs          = Scheduler::ONE_ARG;
	Scheduler::currentArgs[0] = frame->Pop();
	Scheduler::PopFrame();
	CHECK_PREEMPT();
      }
      break;
    case Instr::ARRAYLENGTH:
      {
	JavaDebug::Print("ARRAYLENGTH");
	ObjectArray *arr = ObjectArray::FromWord(frame->Pop());
	if (arr != INVALID_POINTER) {
	  frame->Push(Store::IntToWord(arr->GetLength()));
	  pc += 1;
	}
	else {
	  //to be done: raise NullPointerException
	  Error("NullPointerException");
	}
      }
      break;
    case Instr::ASTORE:
    case Instr::DSTORE: // reals are boxed
    case Instr::FSTORE: // reals are boxed
    case Instr::ISTORE:
    case Instr::LSTORE:
      {
	JavaDebug::Print("(A|D|F|I|L)STORE");
	word value = frame->Pop();
	frame->SetEnv(GET_BYTE_INDEX(), value);
	pc += 2;
      }
      break;
    case Instr::ASTORE_0:
    case Instr::DSTORE_0: // reals are boxed
    case Instr::FSTORE_0: // reals are boxed
    case Instr::ISTORE_0:
    case Instr::LSTORE_0:
      {
	JavaDebug::Print("(A|D|F|I|L)STORE_0");
	word value = frame->Pop();
	frame->SetEnv(0, value);
	pc += 1;
      }
      break;
    case Instr::ASTORE_1:
    case Instr::DSTORE_1: // reals are boxed
    case Instr::FSTORE_1: // reals are boxed
    case Instr::ISTORE_1:
    case Instr::LSTORE_1:
      {
	JavaDebug::Print("(A|D|F|I|L)STORE_1");
	word value = frame->Pop();
	frame->SetEnv(1, value);
	pc += 1;
      }
      break;
    case Instr::ASTORE_2:
    case Instr::DSTORE_2: // reals are boxed
    case Instr::FSTORE_2: // reals are boxed
    case Instr::ISTORE_2:
    case Instr::LSTORE_2:
      {
	JavaDebug::Print("(A|D|F|I|L)STORE_2");
	word value = frame->Pop();
	frame->SetEnv(2, value);
	pc += 1;
      }
      break;
    case Instr::ASTORE_3:
    case Instr::DSTORE_3: // reals are boxed
    case Instr::FSTORE_3: // reals are boxed
    case Instr::ISTORE_3:
    case Instr::LSTORE_3:
      {
	JavaDebug::Print("(A|D|F|I|L)STORE_3");
	word value = frame->Pop();
	frame->SetEnv(3, value);
	pc += 1;
      }
      break;
    case Instr::ATHROW:
      {
	Error("not implemented");
      }
      break;
    case Instr::BALOAD:
      {
	JavaDebug::Print("(BALOAD");	
	u_int index          = Store::DirectWordToInt(frame->Pop());
	JavaByteArray *array = JavaByteArray::FromWord(frame->Pop());
	if (array != INVALID_POINTER) {
	  if (index < array->GetSize()) {
	    frame->Push(Store::IntToWord(array->Get(index)));
	    pc += 1;
	  }
	  else {
	    // to be done: raise ArrayIndexOutOfBoundsException
	    Error("ArrayIndexOutOfBoundsException");
	  }
	}
	else {
	  // to be done: raise NullPointerException
	  Error("NullPointerException");
	}
      }
      break;
    case Instr::BASTORE:
      {
	JavaDebug::Print("(BASTORE");
	u_int value          = Store::DirectWordToInt(frame->Pop());
	u_int index          = Store::DirectWordToInt(frame->Pop());
	JavaByteArray *array = JavaByteArray::FromWord(frame->Pop());
	if (array != INVALID_POINTER) {
	  if (index < array->GetSize()) {
	    array->Put(index, value);
	    pc += 1;
	  }
	  else {
	    // to be done: raise ArrayIndexOutOfBoundsException
	    Error("ArrayIndexOutOfBoundsException");
	  }
	}
	else {
	  // to be done: raise NullPointerException
	  Error("NullPointerException");
	}
      }
      break;
    case Instr::BIPUSH:
      {
	JavaDebug::Print("BIPUSH");
	frame->Push(Store::IntToWord((int) (char) GET_BYTE_INDEX()));
	pc += 2;
      }
      break;
    case Instr::CHECKCAST:
      {
	JavaDebug::Print("CHECKCAST");
	word wType = GET_POOL_VALUE(GET_POOL_INDEX());
	Type *type = Type::FromWord(wType);
	if (type == INVALID_POINTER)
	  REQUEST(wType);
	word wObject = frame->Pop();
	switch (type->GetLabel()) {
	case JavaLabel::Class:
	  {
	    Class *classObj = static_cast<Class *>(type);
	    Block *p = Store::WordToBlock(wObject);
	    Assert(p != INVALID_POINTER);
	    if ((p->GetLabel() != JavaLabel::Object) ||
		(!(Object::FromWordDirect(wObject)->IsInstanceOf(classObj)))) {
	      // to be done: raise CastClassException
	      Error("CastClassException");
	    }
	  }
	  break;
	case JavaLabel::BaseType:
	  {
	    Error("invalid type");
	  }
	  break;
	case JavaLabel::ArrayType:
	  {
	    Error("not implemented");
	  }
	  break;
	default:
	  Error("unknown type");
	}
	frame->Push(wObject);
	pc += 3;
      }
      break;
    case Instr::D2F:
      {
	Error("not implemented");
      }
      break;
    case Instr::D2I:
      {
	Error("not implemented");
      }
      break;
    case Instr::D2L:
      {
	Error("not implemented");
      }
      break;
    case Instr::DADD:
      {
	Error("not implemented");
      }
      break;
    case Instr::DCMPG:
      {
	Error("not implemented");
      }
      break;
    case Instr::DCMPL:
      {
	Error("not implemented");
      }
      break;
    case Instr::DCONST_0:
      {
	Error("not implemented");
      }
      break;
    case Instr::DCONST_1:
      {
	Error("not implemented");
      }
      break;
    case Instr::DDIV:
      {
	Error("not implemented");
      }
      break;
    case Instr::DMUL:
      {
	Error("not implemented");
      }
      break;
    case Instr::DNEG:
      {
	Error("not implemented");
      }
      break;
    case Instr::DREM:
      {
	Error("not implemented");
      }
      break;
    case Instr::DSUB:
      {
	Error("not implemented");
      }
      break;
    case Instr::DUP:
      {
	JavaDebug::Print("DUP");
	word value = frame->Pop();
	frame->Push(value);
	frame->Push(value);
	pc += 1;
      }
      break;
    case Instr::DUP_X1:
      {
	JavaDebug::Print("DUP_X1");
	word v1 = frame->Pop();
	word v2 = frame->Pop();
	frame->Push(v1);
	frame->Push(v2);
	frame->Push(v1);
	pc += 1;
      }
      break;
    case Instr::DUP_X2:
      {
	JavaDebug::Print("DUP_X2");
	// Always match form 1
	word v1 = frame->Pop();
	word v2 = frame->Pop();
	word v3 = frame->Pop();
	word v4 = frame->Pop();
	frame->Push(v2);
	frame->Push(v1);
	frame->Push(v4);
	frame->Push(v3);
	frame->Push(v2);
	frame->Push(v1);
	pc += 1;
      }
      break;
    case Instr::DUP2:
      {
	JavaDebug::Print("DUP2");
	// Always match from 1
	word v1 = frame->Pop();
	word v2 = frame->Pop();
	frame->Push(v2);
	frame->Push(v1);
	frame->Push(v2);
	frame->Push(v1);
	pc += 1;
      }
      break;
    case Instr::DUP2_X1:
      {
	JavaDebug::Print("DUP2_X1");
	// Always match form 1
	word v1 = frame->Pop();
	word v2 = frame->Pop();
	word v3 = frame->Pop();
	frame->Push(v2);
	frame->Push(v1);
	frame->Push(v3);
	frame->Push(v2);
	frame->Push(v1);
	pc += 1;
      }
      break;
    case Instr::DUP2_X2:
      {
	JavaDebug::Print("DUP2_x2");
	// Always match form 1
	word v1 = frame->Pop();
	word v2 = frame->Pop();
	word v3 = frame->Pop();
	word v4 = frame->Pop();
	frame->Push(v2);
	frame->Push(v1);
	frame->Push(v4);
	frame->Push(v3);
	frame->Push(v2);
	frame->Push(v1);
	pc += 1;
      }
      break;
    case Instr::F2D:
      {
	Error("not implemented");
      }
      break;
    case Instr::F2I:
      {
	Error("not implemented");
      }
      break;
    case Instr::F2L:
      {
	Error("not implemented");
      }
      break;
    case Instr::FADD:
      {
	Error("not implemented");
      }
      break;
    case Instr::FCMPG:
      {
	Error("not implemented");
      }
      break;
    case Instr::FCMPL:
      {
	Error("not implemented");
      }
      break;
    case Instr::FCONST_0:
      {
	Error("not implemented");
      }
      break;
    case Instr::FCONST_1:
      {
	Error("not implemented");
      }
      break;
    case Instr::FCONST_2:
      {
	Error("not implemented");
      }
      break;
    case Instr::FDIV:
      {
	Error("not implemented");
      }
      break;
    case Instr::FMUL:
      {
	Error("not implemented");
      }
      break;
    case Instr::FNEG:
      {
	Error("not implemented");
      }
      break;
    case Instr::FREM:
      {
	Error("not implemented");
      }
      break;
    case Instr::FSUB:
      {
	Error("not implemented");
      }
      break;
    case Instr::GETFIELD:
      {
	JavaDebug::Print("GETFIELD");
	word wObject = frame->Pop();
	if (wObject != null) {
	  word wFieldRef             = GET_POOL_VALUE(GET_POOL_INDEX());
	  InstanceFieldRef *fieldRef = InstanceFieldRef::FromWord(wFieldRef);
	  if (fieldRef == INVALID_POINTER)
	    REQUEST(wFieldRef);
	  Object *object = Object::FromWord(wObject);
	  Assert(object != INVALID_POINTER);
	  word value = object->GetInstanceField(fieldRef->GetIndex()); 
	  frame->Push(value);
	}
	else {
	  // to be done: raise NullPointerException
	  Error("NullPointerException");
	}
	pc += 3;
      }
      break;
    case Instr::GETSTATIC:
      {
	JavaDebug::Print("GETSTATIC");
	word wFieldRef           = GET_POOL_VALUE(GET_POOL_INDEX());
	StaticFieldRef *fieldRef = StaticFieldRef::FromWord(wFieldRef);
	if (fieldRef == INVALID_POINTER)
	  REQUEST(wFieldRef);
	Class *classObj = fieldRef->GetClass();
	Assert(classObj != INVALID_POINTER);
	word value = classObj->GetStaticField(fieldRef->GetIndex()); 
	frame->Push(value);
	pc += 3;
      }
      break;
    case Instr::GOTO:
      {
	JavaDebug::Print("GOTO");
	pc += (short int) GET_POOL_INDEX();
      }
      break;
    case Instr::GOTO_W:
      {
	JavaDebug::Print("GOTO_W");
	pc += (int) GET_WIDE_INDEX();
      }
      break;
    case Instr::I2B:
    case Instr::I2C: // to be done
      {
	JavaDebug::Print("I2(B|C)");
	int i = JavaInt::FromWord(frame->Pop());
	frame->Push(Store::IntToWord((int) ((char) i)));
	pc += 1;
      }
      break;
    case Instr::I2D:
      {
	Error("not implemented");
      }
      break;
    case Instr::I2F:
      {
	Error("not implemented");
      }
      break;
    case Instr::I2L:
      {
	Error("not implemented");
      }
      break;
    case Instr::I2S:
      {
	Error("not implemented");
      }
      break;
    case Instr::IADD:
      {
	JavaDebug::Print("IADD");
	int v2 = JavaInt::FromWord(frame->Pop());
	int v1 = JavaInt::FromWord(frame->Pop());
	frame->Push(JavaInt::ToWord(v1 + v2));
	pc += 1;
      }
      break;
    case Instr::IAND:
      {
	JavaDebug::Print("IAND");
	int v2 = JavaInt::FromWord(frame->Pop());
	int v1 = JavaInt::FromWord(frame->Pop());
	frame->Push(JavaInt::ToWord(v1 & v2));
	pc += 1;
      }
      break;
    case Instr::ICONST_M1:
      {
	JavaDebug::Print("ICONST_M1");
	frame->Push(JavaInt::ToWord(-1));
	pc += 1;
      }
      break;
    case Instr::ICONST_0:
      {
	JavaDebug::Print("ICONST_0");
	frame->Push(JavaInt::ToWord(0));
	pc += 1;
      }
      break;
    case Instr::ICONST_1:
      {
	JavaDebug::Print("ICONST_1");
	frame->Push(JavaInt::ToWord(1));
	pc += 1;
      }
      break;
    case Instr::ICONST_2:
      {
	JavaDebug::Print("ICONST_2");
	frame->Push(JavaInt::ToWord(2));
	pc += 1;
      }
      break;
    case Instr::ICONST_3:
      {
	JavaDebug::Print("ICONST_3");
	frame->Push(JavaInt::ToWord(3));
	pc += 1;
      }
      break;
    case Instr::ICONST_4:
      {
	JavaDebug::Print("ICONST_4");
	frame->Push(JavaInt::ToWord(4));
	pc += 1;
      }
      break;
    case Instr::ICONST_5:
      {
	JavaDebug::Print("ICONST_5");
	frame->Push(JavaInt::ToWord(5));
	pc += 1;
      }
      break;
    case Instr::IDIV:
      {
	JavaDebug::Print("IDIV");
	int v2 = JavaInt::FromWord(frame->Pop());
	int v1 = JavaInt::FromWord(frame->Pop());
	if (v2 != 0)
	  frame->Push(JavaInt::ToWord(v1 / v2));
	else {
	  // to be done: raise ArithmeticException
	  Error("ArithmeticException");
	}
	pc += 1;
      }
      break;
    case Instr::IF_ACMPEQ:
    case Instr::IF_ICMPEQ:
      {
	JavaDebug::Print("IF_(A|I)CMPEQ");
	word v2 = frame->Pop();
	word v1 = frame->Pop();
	if (v1 == v2)
	  pc += (short) GET_POOL_INDEX();
	else
	  pc += 3;
      }
      break;
    case Instr::IF_ACMPNE:
    case Instr::IF_ICMPNE:
      {
	JavaDebug::Print("IF_(A|I)CMPNE");
	word v2 = frame->Pop();
	word v1 = frame->Pop();
	if (v1 != v2)
	  pc += (short) GET_POOL_INDEX();
	else
	  pc += 3;
      }
      break;
    case Instr::IF_ICMPLT:
      {
	JavaDebug::Print("IF_ICMPLT");
	int v2 = JavaInt::FromWord(frame->Pop());
	int v1 = JavaInt::FromWord(frame->Pop());
	if (v1 < v2)
	  pc += (short) GET_POOL_INDEX();
	else
	  pc += 3;
      }
      break;
    case Instr::IF_ICMPGE:
      {
	JavaDebug::Print("IF_ICMPGE");
	int v2 = JavaInt::FromWord(frame->Pop());
	int v1 = JavaInt::FromWord(frame->Pop());
	if (v1 >= v2)
	  pc += (short) GET_POOL_INDEX();
	else
	  pc += 3;
      }
      break;
    case Instr::IF_ICMPGT:
      {
	JavaDebug::Print("IF_ICMPGT");
	int v2 = JavaInt::FromWord(frame->Pop());
	int v1 = JavaInt::FromWord(frame->Pop());
	if (v1 > v2)
	  pc += (short) GET_POOL_INDEX();
	else
	  pc += 3;
      }
      break;
    case Instr::IF_ICMPLE:
      {
	JavaDebug::Print("IF_ICMPLE");
	int v2 = JavaInt::FromWord(frame->Pop());
	int v1 = JavaInt::FromWord(frame->Pop());
	if (v1 <= v2)
	  pc += (short) GET_POOL_INDEX();
	else
	  pc += 3;
      }
      break;
    case Instr::IFEQ:
      {
	JavaDebug::Print("IFEQ");
	if (JavaInt::Deref(frame->Pop()) == JavaInt::Zero())
	  pc += (short) GET_POOL_INDEX();
	else
	  pc += 3;
      }
      break;
    case Instr::IFNE:
      {
	JavaDebug::Print("IFNE");
	if (JavaInt::Deref(frame->Pop()) != JavaInt::Zero())
	  pc += (short) GET_POOL_INDEX();
	else
	  pc += 3;
      }
      break;
    case Instr::IFLT:
      {
	JavaDebug::Print("IFLT");
	if (JavaInt::FromWord(frame->Pop()) < 0)
	  pc += (short) GET_POOL_INDEX();
	else
	  pc += 3;
      }
      break;
    case Instr::IFGE:
      {
	JavaDebug::Print("IFGE");
	if (JavaInt::FromWord(frame->Pop()) >= 0)
	  pc += (short) GET_POOL_INDEX();
	else
	  pc += 3;
      }
      break;
    case Instr::IFGT:
      {
	JavaDebug::Print("IFGT");
	if (JavaInt::FromWord(frame->Pop()) > 0)
	  pc += (short) GET_POOL_INDEX();
	else
	  pc += 3;
      }
      break;
    case Instr::IFLE:
      {
	JavaDebug::Print("IFLE");
	if (JavaInt::FromWord(frame->Pop()) <= 0)
	  pc += (short) GET_POOL_INDEX();
	else
	  pc += 3;
      }
      break;
    case Instr::IFNONNULL:
      {
	JavaDebug::Print("IFNONNULL");
	if (frame->Pop() != null)
	  pc += (short) GET_POOL_INDEX();
	else
	  pc += 3;
      }
      break;
    case Instr::IFNULL:
      {
	JavaDebug::Print("IFNULL");
	if (frame->Pop() == null)
	  pc += (short) GET_POOL_INDEX();
	else
	  pc += 3;
      }
      break;
    case Instr::IINC:
      {
	JavaDebug::Print("IINC");
	u_int index = GET_BYTE_INDEX();
	int v       = JavaInt::FromWord(frame->GetEnv(index));
	v += static_cast<int>(code[pc + 2]);
	frame->SetEnv(index, JavaInt::ToWord(v));
	pc += 3;
      }
      break;
    case Instr::IMUL:
      {
	JavaDebug::Print("IMUL");
	int v2 = JavaInt::FromWord(frame->Pop());
	int v1 = JavaInt::FromWord(frame->Pop());
	frame->Push(JavaInt::ToWord(v1 * v2));
	pc += 1;
      }
      break;
    case Instr::INEG:
      {
	JavaDebug::Print("INEG");
	int v = JavaInt::FromWord(frame->Pop());
	frame->Push(JavaInt::ToWord(0-v));
	pc += 1;
      }
      break;
    case Instr::INSTANCEOF:
      {
	JavaDebug::Print("INSTANCEOF");
	word wType = GET_POOL_VALUE(GET_POOL_INDEX());
	Type *type = Type::FromWord(wType);
	if (type == INVALID_POINTER)
	  REQUEST(wType);
	word wObject = frame->Pop();
	int result = 0;
	switch (type->GetLabel()) {
	case JavaLabel::Class:
	  {
	    Class *classObj = static_cast<Class *>(type);
	    Block *p = Store::WordToBlock(wObject);
	    Assert(p != INVALID_POINTER);
	    result = (p->GetLabel() == JavaLabel::Object &&
		      Object::FromWordDirect(wObject)->IsInstanceOf(classObj));
	  }
	  break;
	case JavaLabel::BaseType:
	  {
	    Error("invalid type");
	  }
	  break;
	case JavaLabel::ArrayType:
	  {
	    Error("not implemented");
	  }
	  break;
	default:
	  Error("unknown type");
	}
	frame->Push(Store::IntToWord(result));
	pc += 3;
      }
      break;
    case Instr::INVOKEINTERFACE:
      {
	Error("not implemented");
      }
      break;
    case Instr::INVOKESPECIAL:
      {
	JavaDebug::Print("INVOKESPECIAL");
	word wMethodRef             = GET_POOL_VALUE(GET_POOL_INDEX());
	VirtualMethodRef *methodRef = VirtualMethodRef::FromWord(wMethodRef);
	if (methodRef == INVALID_POINTER)
	  REQUEST(wMethodRef);
	// Set continuation
	frame->SetPC(-2);
	frame->SetContPC(pc + 3);
	// to be done: support more arguments
	u_int nArgs = methodRef->GetNumberOfArguments();
	Assert(nArgs < Scheduler::maxArgs);
	// self becomes local0
	Scheduler::nArgs = nArgs + 1;
	for (u_int i = nArgs + 1; i--;)
	  Scheduler::currentArgs[i] = frame->Pop();
	// to be done: where is the closure to be found; assuming static ref
	Class *classObj  = methodRef->GetClass();
	Assert(classObj != INVALID_POINTER);
	Closure *closure = classObj->GetVirtualMethod(methodRef->GetIndex());
	Assert(closure != INVALID_POINTER);
	return Scheduler::PushCall(closure->ToWord());
      }
      break;
    case Instr::INVOKESTATIC:
      {
	JavaDebug::Print("INVOKESTATIC");
	word wMethodRef            = GET_POOL_VALUE(GET_POOL_INDEX());
	StaticMethodRef *methodRef = StaticMethodRef::FromWord(wMethodRef);
	if (methodRef == INVALID_POINTER)
	  REQUEST(wMethodRef);
	// Set continuation
	frame->SetPC(-2);
	frame->SetContPC(pc + 3);
	// to be done: support more arguments
	u_int nArgs = methodRef->GetNumberOfArguments();
	Assert(nArgs < Scheduler::maxArgs);
	Scheduler::nArgs = nArgs;
	for (u_int i = nArgs; i--;)
	  Scheduler::currentArgs[i] = frame->Pop();
	Class *classObj = methodRef->GetClass();
	Assert(classObj != INVALID_POINTER);
	Closure *closure = classObj->GetStaticMethod(methodRef->GetIndex());
	Assert(closure != INVALID_POINTER);
	return Scheduler::PushCall(closure->ToWord());
      }
      break;
    case Instr::INVOKEVIRTUAL:
      {
	JavaDebug::Print("INVOKEVIRTUAL");
	word wMethodRef             = GET_POOL_VALUE(GET_POOL_INDEX());
	VirtualMethodRef *methodRef = VirtualMethodRef::FromWord(wMethodRef);
	if (methodRef == INVALID_POINTER)
	  REQUEST(wMethodRef);
	// Set continuation
	frame->SetPC(-2);
	frame->SetContPC(pc + 3);
	// to be done: support more arguments
	u_int nArgs = methodRef->GetNumberOfArguments();
	Assert(nArgs < Scheduler::maxArgs - 1);
	// self becomes local0
	Scheduler::nArgs = nArgs + 1;
	for (u_int i = nArgs + 1; i--;)
	  Scheduler::currentArgs[i] = frame->Pop();
	Object *object = Object::FromWord(Scheduler::currentArgs[0]);
	Assert(object != INVALID_POINTER);
	Closure *closure = object->GetVirtualMethod(methodRef->GetIndex());
	Assert(closure != INVALID_POINTER);
	return Scheduler::PushCall(closure->ToWord());
      }
      break;
    case Instr::IOR:
      {
	JavaDebug::Print("IOR");
	int v2 = JavaInt::FromWord(frame->Pop());
	int v1 = JavaInt::FromWord(frame->Pop());
	frame->Push(JavaInt::ToWord(v1 || v2));
	pc += 1;
      }
      break;
    case Instr::IREM:
      {
	JavaDebug::Print("IREM");
	int v2 = JavaInt::FromWord(frame->Pop());
	int v1 = JavaInt::FromWord(frame->Pop());
	if (v2 != 0)
	  frame->Push(JavaInt::ToWord(v1 % v2));
	else {
	  // to be done: raise ArithmeticException
	  Error("ArithmeticException");
	}
	pc += 1;
      }
      break;
    case Instr::ISHL:
      {
	JavaDebug::Print("ISHL");
	int v2 = JavaInt::FromWord(frame->Pop());
	int v1 = JavaInt::FromWord(frame->Pop());
	frame->Push(JavaInt::ToWord(v1 << v2));
	pc += 1;
      }
      break;
    case Instr::ISHR:
      {
	JavaDebug::Print("IHSR");
	int v2 = JavaInt::FromWord(frame->Pop());
	int v1 = JavaInt::FromWord(frame->Pop());
	frame->Push(JavaInt::ToWord(v1 >> v2));
	pc += 1;
      }
      break;
    case Instr::ISUB:
      {
	JavaDebug::Print("ISUB");
	int v2 = JavaInt::FromWord(frame->Pop());
	int v1 = JavaInt::FromWord(frame->Pop());
	frame->Push(JavaInt::ToWord(v1 - v2));
	pc += 1;
      }
      break;
    case Instr::IUSHR:
      {
	Error("not implemented");
      }
      break;
    case Instr::IXOR:
      {
	JavaDebug::Print("IXOR");
	int v2 = JavaInt::FromWord(frame->Pop());
	int v1 = JavaInt::FromWord(frame->Pop());
	frame->Push(JavaInt::ToWord(v1 ^ v2));
	pc += 1;
      }
      break;
    case Instr::JSR:
      {
	Error("not implemented");
      }
      break;
    case Instr::JSR_W:
      {
	Error("not implemented");
      }
    case Instr::L2D:
      {
	Error("not implemented");
      }
      break;
    case Instr::L2F:
      {
	Error("not implemented");
      }
      break;
    case Instr::L2I:
      {
	Error("not implemented");
      }
      break;
    case Instr::LADD:
      {
	Error("not implemented");
      }
      break;
    case Instr::LAND:
      {
	Error("not implemented");
      }
      break;
    case Instr::LCMP:
      {
	Error("not implemented");
      }
      break;
    case Instr::LCONST_0:
      {
	Error("not implemented");
      }
      break;
    case Instr::LCONST_1:
      {
	Error("not implemented");
      }
      break;
    case Instr::LDC:
      {
	JavaDebug::Print("LDC");
	word value = GET_POOL_VALUE(GET_BYTE_INDEX()); 
	frame->Push(value);
	pc += 2;
      }
      break;
    case Instr::LDC_W:
    case Instr::LDC2_W:
      {
	Error("not implemented");
      }
    case Instr::LDIV:
      {
	Error("not implemented");
      }
      break;
    case Instr::LMUL:
      {
	Error("not implemented");
      }
      break;
    case Instr::LNEG:
      {
	Error("not implemented");
      }
      break;
    case Instr::LOOKUPSWITCH:
      {
	Error("not implemented");
      }
      break;
    case Instr::LOR:
      {
	Error("not implemented");
      }
      break;
    case Instr::LREM:
      {
	Error("not implemented");
      }
      break;
    case Instr::LSHL:
      {
	Error("not implemented");
      }
      break;
    case Instr::LSHR:
      {
	Error("not implemented");
      }
      break;
    case Instr::LSUB:
      {
	Error("not implemented");
      }
      break;
    case Instr::LUSHR:
    case Instr::LXOR:
      {
	Error("not implemented");
      }
      break;
    case Instr::MONITORENTER:
      {
	Error("not implemented");
      }
      break;
    case Instr::MONITOREXIT:
      {
	Error("not implemented");
      }
      break;
    case Instr::MULTIANEWARRAY:
      {
	JavaDebug::Print("MULTIANEWARRAY");
	word wType = GET_POOL_VALUE(GET_POOL_INDEX());
	Type *type = Type::FromWord(wType);
	if (type == INVALID_POINTER)
	  REQUEST(wType);
	u_int nDims = (u_int) code[pc + 3];
	for (u_int i = nDims; i--;) {
	  int count = JavaInt::FromWord(frame->Pop());
	  if (count < 0) {
	    // to be done: raise NegativeArraySizeException
	    Error("NegativeArraySizeException");
	  }
	  ObjectArray *arr = ObjectArray::New(type, count);
	  arr = arr;
	  type = static_cast<Type *>(ArrayType::New(type->ToWord()));
	}
	frame->Push(type->ToWord());
	pc += 4;
      }
      break;
    case Instr::NEW:
      {
	JavaDebug::Print("NEW");
	word wType = GET_POOL_VALUE(GET_POOL_INDEX());
	Type *type = Type::FromWord(wType);
	if (type == INVALID_POINTER)
	  REQUEST(wType);
	switch (type->GetLabel()) {
	case JavaLabel::Class:
	  {
	    Class *classObj = static_cast<Class *>(type);
	    Object *object = Object::New(classObj);
	    Assert(object != INVALID_POINTER);
	    frame->Push(object->ToWord());
	  }
	  break;
	case JavaLabel::BaseType:
	case JavaLabel::ArrayType:
	  {
	    // to be done: raise InstantiationError
	    Error("InstantiationError");
	  }
	  break;
	default:
	  Error("unknown type");
	}
	pc += 3;
      }
      break;
    case Instr::NEWARRAY:
      {
	JavaDebug::Print("NEWARRAY");
	int count = Store::DirectWordToInt(frame->Pop());
	if (count >= 0) {
	  word array;
	  BaseType::type baseType = static_cast<BaseType::type>(code[pc + 1]);
	  switch (baseType) {
	  case BaseType::Byte:
	  case BaseType::Boolean:
	    {
	      array = JavaByteArray::New(count)->ToWord();
	    }
	    break;
	  default:
	    {
	      Type *type = static_cast<Type *>(BaseType::New(baseType));
	      array = ObjectArray::New(type, count)->ToWord();
	    }
	  }
	  frame->Push(array);
	}
	else {
	  // to be done: raise NegativeArraySizeException
	  Error("NegativeArraySizeException");
	}
	pc += 2;
      }
      break;
    case Instr::NOP:
      {
	JavaDebug::Print("NOP");
	pc += 1;
      }
      break;
    case Instr::POP:
      {
	JavaDebug::Print("POP");
	frame->Pop();
	pc += 1;
      }
      break;
    case Instr::POP2:
      {
	JavaDebug::Print("POP2");
	// Always match from 1
	frame->Pop();
	frame->Pop();
	pc += 1;
      }
      break;
    case Instr::PUTFIELD:
      {
	JavaDebug::Print("PUTFIELD");
	word wFieldRef             = GET_POOL_VALUE(GET_POOL_INDEX());
	InstanceFieldRef *fieldRef = InstanceFieldRef::FromWord(wFieldRef);
	if (fieldRef == INVALID_POINTER)
	  REQUEST(wFieldRef);
	word value = frame->Pop();
	Object *object = Object::FromWord(frame->Pop());
	Assert(object != INVALID_POINTER);
	object->PutInstanceField(fieldRef->GetIndex(), value);
	pc += 3;
      }
      break;
    case Instr::PUTSTATIC:
      {
	JavaDebug::Print("PUTSTATIC");
	word wFieldRef           = GET_POOL_VALUE(GET_POOL_INDEX());
	StaticFieldRef *fieldRef = StaticFieldRef::FromWord(wFieldRef);
	if (fieldRef == INVALID_POINTER)
	  REQUEST(wFieldRef);
	Class *classObj = fieldRef->GetClass();
	Assert(classObj != INVALID_POINTER);
	classObj->PutStaticField(fieldRef->GetIndex(), frame->Pop());
	pc += 3;
      }
      break;
    case Instr::RET:
      {
	Error("not implemented");
      }
      break;
    case Instr::RETURN:
      {
	JavaDebug::Print("RETURN");
	Scheduler::nArgs = 0;
	Scheduler::PopFrame();
	CHECK_PREEMPT();
      }
      break;
    case Instr::SIPUSH:
      {
	JavaDebug::Print("SIPUSH");
	short value = GET_POOL_INDEX();
	frame->Push(JavaInt::ToWord(value));
	pc += 3;
      }
    case Instr::SWAP:
      {
	JavaDebug::Print("SWAP");
	word v1 = frame->Pop();
	word v2 = frame->Pop();
	frame->Push(v1);
	frame->Push(v2);
	pc += 1;
      }
      break;
    case Instr::TABLESWITCH:
      {
	Error("not implemented");
      }
      break;
    case Instr::WIDE:
      {
	JavaDebug::Print("WIDE");
	switch (static_cast<Instr::Opcode>(code[++pc])) {
	case Instr::ILOAD:
	case Instr::FLOAD:
	case Instr::ALOAD:
	case Instr::LLOAD:
	  {
	    frame->Push(frame->GetEnv(GET_POOL_INDEX()));
	    pc += 2;
	  }
	  break;
	case Instr::ISTORE:
	case Instr::FSTORE:
	case Instr::ASTORE:
	case Instr::LSTORE:
	  {
	    frame->SetEnv(GET_POOL_INDEX(), frame->Pop());
	    pc += 2;
	  }
	  break;
	case Instr::IINC:
	  {
	    u_int index      = GET_POOL_INDEX();
	    unsigned char c1 = code[pc + 3];
	    unsigned char c2 = code[pc + 4];
	    signed short inc = ((c1 << 8) | c2);
	    int value        = JavaInt::FromWord(frame->GetEnv(index));
	    value += inc;
	    frame->SetEnv(index, JavaInt::ToWord(value));
	    pc += 4;
	  }
	  break;
	case Instr::RET:
	  {
	    Error("not implemented");
	  }
	default:
	  Error("wrong opcode");
	}
      }
      break;
    default:
      Error("invalid opcode");
    }
    // Check for preemption
    if (StatusWord::GetStatus() != 0) {
      frame->SetPC(pc);
      return Worker::PREEMPT;
    }
  }
}

Interpreter::Result ByteCodeInterpreter::Handle() {
  ByteCodeFrame *frame = ByteCodeFrame::FromWordDirect(Scheduler::GetFrame());
  int pc               = frame->GetPC();
  Table *table         = frame->GetExceptionTable();
  u_int count          = table->GetCount();
  Object *object       = Object::FromWord(Scheduler::currentData);
  Assert(object != INVALID_POINTER);
  for (u_int i = 0; i < count; i++) {
    ExceptionTableEntry *entry =
      ExceptionTableEntry::FromWordDirect(table->Get(i));
    int startPC = entry->GetStartPC();
    int endPC   = entry->GetEndPC();
    // Exception handler is within range
    if ((startPC <= pc) && (pc < endPC)) {
      // Check exception type
      word wType = entry->GetCatchType();
      if (wType == Store::IntToWord(0)) {
	frame->SetPC(entry->GetHandlerPC());
	return Worker::CONTINUE;
      }
      Class *typeObj = Class::FromWord(entry->GetCatchType());
      Assert(typeObj != INVALID_POINTER);
      if (object->IsInstanceOf(typeObj)) {
	frame->SetPC(entry->GetHandlerPC());
	return Worker::CONTINUE;
      }
    }
  }
  // to be done: Add to BackTrace
  Scheduler::PopFrame();
  return Worker::RAISE;
}

const char *ByteCodeInterpreter::Identify() {
  return "ByteCodeInterpreter";
}

void ByteCodeInterpreter::DumpFrame(word) {
  //--** nicer output
  std::fprintf(stderr, "Java method\n");
}
