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
    TOP_POS,
    CODE_POS,
    CLOSURE_POS,
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
  Chunk *GetCode() {
    return Store::DirectWordToChunk(StackFrame::GetArg(CODE_POS));
  }
  void SetCode(Chunk *code) {
    StackFrame::ReplaceArg(CODE_POS, code->ToWord());
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
			    Chunk *code,
			    Closure *closure,
			    u_int nbLocals,
			    u_int maxStack) {
    u_int frSize = BASE_SIZE + nbLocals + maxStack;
    StackFrame *frame =
      StackFrame::New(JAVA_BYTE_CODE_FRAME, interpreter, frSize);
    frame->InitArg(PC_POS, pc);
    frame->InitArg(TOP_POS, Store::IntToWord(BASE_SIZE + nbLocals));
    frame->InitArg(CODE_POS, code->ToWord());
    frame->InitArg(CLOSURE_POS, closure->ToWord());
    return static_cast<ByteCodeFrame *>(frame);
  }
  // ByteCodeFrame Untagging
  static ByteCodeFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == JAVA_BYTE_CODE_FRAME);
    return static_cast<ByteCodeFrame *>(p);
  }
};

class ByteCodeHandlerFrame : public StackFrame {
protected:
  enum { PC_POS, FRAME_POS, BASE_SIZE };
public:
  using Block::ToWord;

  // ByteCodeHandlerFrame Accessors
  u_int GetPC() {
    return (u_int) Store::DirectWordToInt(StackFrame::GetArg(PC_POS));
  }
  ByteCodeFrame *GetCodeFrame() {
    return ByteCodeFrame::FromWordDirect(StackFrame::GetArg(FRAME_POS));
  }
  // ByteCodeHandlerFrame Constructor
  static ByteCodeHandlerFrame *New(Interpreter *interpreter,
				   word pc,
				   ByteCodeFrame *codeFrame) {
    StackFrame *frame = StackFrame::New(JAVA_BYTE_CODE_FRAME,
					interpreter, BASE_SIZE);
    frame->InitArg(PC_POS, pc);
    frame->InitArg(FRAME_POS, codeFrame->ToWord());
    return static_cast<ByteCodeHandlerFrame *>(frame);
  }
  // ByteCodeHandlerFrame Untagging
  static ByteCodeHandlerFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == JAVA_BYTE_CODE_FRAME);
    return static_cast<ByteCodeHandlerFrame *>(p);
  }
};

//
// Interpreter Functions
//
ByteCodeInterpreter *ByteCodeInterpreter::self;

Block *
ByteCodeInterpreter::GetAbstractRepresentation(ConcreteRepresentation *b) {
  return NULL; // to be done
}


void ByteCodeInterpreter::PushCall(Closure *) {
  // to be done
}

Worker::Result ByteCodeInterpreter::Run() {
  ByteCodeFrame *frame = ByteCodeFrame::FromWordDirect(Scheduler::GetFrame());
  int pc               = frame->GetPC();
  unsigned char *code  = (unsigned char *) (frame->GetCode()->GetBase());
  while (1) {
    switch (static_cast<Instr::Opcode>(code[pc])) {
    case Instr::AALOAD:
    case Instr::DALOAD: // reals are boxed
    case Instr::FALOAD: // reals are boxed
    case Instr::IALOAD:
    case Instr::LALOAD:
    case Instr::SALOAD:
      {
	u_int index = Store::DirectWordToInt(frame->Pop());
	ObjectArray *arr  = ObjectArray::FromWord(frame->Pop());
	if (arr != INVALID_POINTER) {
	  if (index < arr->GetLength()) {
	    frame->Push(arr->Get(index));
	  }
	  else {
	    // to be done: raise invalid something
	  }
	}
	else {
	  // to be done: raise nullpointerexception
	}
      }
      break;
    case Instr::AASTORE:
    case Instr::DASTORE: // reals are boxed
    case Instr::FASTORE: // reals are boxed
    case Instr::IASTORE:
    case Instr::LASTORE:
    case Instr::SASTORE:
      {
	word value  = frame->Pop();
	u_int index = Store::DirectWordToInt(frame->Pop());
	ObjectArray *arr  = ObjectArray::FromWordDirect(frame->Pop());
	if (index < arr->GetLength()) {
	  arr->Assign(index, value);
	}
	else {
	  // to be done: raise invalid something
	}
      }
      break;
    case Instr::ACONST_NULL:
      {
	frame->Push(Store::IntToWord(0));
      }
      break;
    case Instr::ALOAD:
    case Instr::DLOAD: // reals are boxed
    case Instr::FLOAD: // reals are boxed
    case Instr::ILOAD:
    case Instr::LLOAD:
      {
	u_int index = (u_int) code[++pc];
	frame->Push(frame->GetEnv(index));
      }
      break;
    case Instr::ALOAD_0:
    case Instr::DLOAD_0:
    case Instr::FLOAD_0:
    case Instr::ILOAD_0:
    case Instr::LLOAD_0:
      {
	frame->Push(frame->GetEnv(0));
      }
      break;
    case Instr::ALOAD_1:
    case Instr::DLOAD_1:
    case Instr::FLOAD_1:
    case Instr::ILOAD_1:
    case Instr::LLOAD_1:
      {
	frame->Push(frame->GetEnv(1));
      }
      break;
    case Instr::ALOAD_2:
    case Instr::DLOAD_2:
    case Instr::FLOAD_2:
    case Instr::ILOAD_2:
    case Instr::LLOAD_2:
      {
	frame->Push(frame->GetEnv(2));
      }
      break;
    case Instr::ALOAD_3:
    case Instr::DLOAD_3:
    case Instr::FLOAD_3:
    case Instr::ILOAD_3:
    case Instr::LLOAD_3:
      {
	frame->Push(frame->GetEnv(3));
      }
      break;
    case Instr::ANEWARRAY:
      {
	// to be done: what is with the index bytes?
	int count = Store::DirectWordToInt(frame->Pop());
	if (count >= 0) {
	  ObjectArrayType *type = INVALID_POINTER; // to be done
	  ObjectArray *arr = ObjectArray::New(type, count);
	  for (u_int i = count; i--;)
	    arr->Init(i, Store::IntToWord(0));
	  frame->Push(arr->ToWord());
	}
	else {
	  // to be done: thow invalid something
	}
      }
      break;
    case Instr::ARETURN:
    case Instr::DRETURN: // reals are boxed
    case Instr::FRETURN: // reals are boxed
    case Instr::IRETURN:
    case Instr::LRETURN:
      {
	// to be done
	Scheduler::nArgs          = Scheduler::ONE_ARG;
	Scheduler::currentArgs[0] = frame->Pop();
	Scheduler::PopFrame();
	return Interpreter::CONTINUE;
      }
      break;
    case Instr::ARRAYLENGTH:
      {
	ObjectArray *arr = ObjectArray::FromWord(frame->Pop());
	if (arr != INVALID_POINTER) {
	  frame->Push(Store::IntToWord(arr->GetLength()));
	}
	else {
	  //to be done: raise NullPointerException
	}
      }
      break;
    case Instr::ASTORE:
    case Instr::DSTORE: // reals are boxed
    case Instr::FSTORE: // reals are boxed
    case Instr::ISTORE:
    case Instr::LSTORE:
      {
	u_int index = code[++pc];
	frame->SetEnv(index, frame->Pop());
      }
      break;
    case Instr::ASTORE_0:
    case Instr::DSTORE_0: // reals are boxed
    case Instr::FSTORE_0: // reals are boxed
    case Instr::ISTORE_0:
    case Instr::LSTORE_0:
      {
	frame->SetEnv(0, frame->Pop());
      }
      break;
    case Instr::ASTORE_1:
    case Instr::DSTORE_1: // reals are boxed
    case Instr::FSTORE_1: // reals are boxed
    case Instr::ISTORE_1:
    case Instr::LSTORE_1:
      {
	frame->SetEnv(1, frame->Pop());
      }
      break;
    case Instr::ASTORE_2:
    case Instr::DSTORE_2: // reals are boxed
    case Instr::FSTORE_2: // reals are boxed
    case Instr::ISTORE_2:
    case Instr::LSTORE_2:
      {
	frame->SetEnv(2, frame->Pop());
      }
      break;
    case Instr::ASTORE_3:
    case Instr::DSTORE_3: // reals are boxed
    case Instr::FSTORE_3: // reals are boxed
    case Instr::ISTORE_3:
    case Instr::LSTORE_3:
      {
	frame->SetEnv(3, frame->Pop());
      }
      break;
    case Instr::ATHROW:
      {
	// to be done
      }
      break;
    case Instr::BALOAD:
    case Instr::CALOAD:
      {
	u_int index    = Store::DirectWordToInt(frame->Pop());
	Chunk *byteArr = Store::DirectWordToChunk(frame->Pop());
	if (index < byteArr->GetSize()) {
	  frame->Push(Store::IntToWord(byteArr->GetBase()[index]));
	}
	else {
	  // to be done: throw invalid something
	}
      }
      break;
    case Instr::BASTORE:
    case Instr::CASTORE:
      {
	u_int value    = Store::DirectWordToInt(frame->Pop());
	u_int index    = Store::DirectWordToInt(frame->Pop());
	Chunk *byteArr = Store::DirectWordToChunk(frame->Pop());
	if (index < byteArr->GetSize()) {
	  byteArr->GetBase()[index] = (char) value;
	}
	else {
	  // to be done: throw invalid something
	}
      }
      break;
    case Instr::BIPUSH:
      {
	char byte = code[++pc];
	frame->Push(Store::IntToWord(byte));
      }
      break;
    case Instr::CHECKCAST:
      {
	unsigned char byte1 = code[++pc];
	unsigned char byte2 = code[++pc];
	u_int index = byte1 << 8 | byte2;
	word object = frame->Pop();
	// to be done
      }
      break;
    case Instr::D2F:
      {
	// to be done
      }
      break;
    case Instr::D2I:
      {
	// to be done
      }
      break;
    case Instr::D2L:
      {
	// to be done
      }
      break;
    case Instr::DADD:
      {
	Chunk *v2 = Store::DirectWordToChunk(frame->Pop());
	Chunk *v1 = Store::DirectWordToChunk(frame->Pop());
	Chunk *res = v1;
	// to be done
	frame->Push(res->ToWord());
      }
      break;
    case Instr::DCMPG:
      {
	Chunk *v2 = Store::DirectWordToChunk(frame->Pop());
	Chunk *v1 = Store::DirectWordToChunk(frame->Pop());
	Chunk *res = v1;
	// to bo done
	frame->Push(res->ToWord());
      }
      break;
    case Instr::DCMPL:
      {
	Chunk *v2 = Store::DirectWordToChunk(frame->Pop());
	Chunk *v1 = Store::DirectWordToChunk(frame->Pop());
	Chunk *res = v1;
	// to bo done
	frame->Push(res->ToWord());
      }
      break;
    case Instr::DCONST_0:
      {
	Chunk *res = Store::AllocChunk(0);
	// to be done
	frame->Push(res->ToWord());
      }
      break;
    case Instr::DCONST_1:
      {
	Chunk *res = Store::AllocChunk(0);
	// to be done
	frame->Push(res->ToWord());
      }
      break;
    case Instr::DDIV:
      {
	Chunk *v2 = Store::DirectWordToChunk(frame->Pop());
	Chunk *v1 = Store::DirectWordToChunk(frame->Pop());
	Chunk *res = v1;
	// to bo done
	frame->Push(res->ToWord());
      }
      break;
    case Instr::DMUL:
      {
	Chunk *v2 = Store::DirectWordToChunk(frame->Pop());
	Chunk *v1 = Store::DirectWordToChunk(frame->Pop());
	Chunk *res = v1;
	// to bo done
	frame->Push(res->ToWord());
      }
      break;
    case Instr::DNEG:
      {
	Chunk *v1 = Store::DirectWordToChunk(frame->Pop());
	Chunk *res = v1;
	// to bo done
	frame->Push(res->ToWord());
      }
      break;
    case Instr::DREM:
      {
	Chunk *v1 = Store::DirectWordToChunk(frame->Pop());
	Chunk *res = v1;
	// to bo done
	frame->Push(res->ToWord());
      }
      break;
    case Instr::DSUB:
      {
	Chunk *v2 = Store::DirectWordToChunk(frame->Pop());
	Chunk *v1 = Store::DirectWordToChunk(frame->Pop());
	Chunk *res = v1;
	// to bo done
	frame->Push(res->ToWord());
      }
      break;
    case Instr::DUP:
      {
	// to be done: more efficient solution
	word value = frame->Pop();
	frame->Push(value);
	frame->Push(value);
      }
      break;
    case Instr::DUP_X1:
      {
	word v1 = frame->Pop();
	word v2 = frame->Pop();
	frame->Push(v1);
	frame->Push(v2);
	frame->Push(v1);
      }
      break;
    case Instr::DUP_X2:
      {
	// to de done: handle two or three value form? how?
	word v1 = frame->Pop();
	word v2 = frame->Pop();
	word v3 = frame->Pop();
	frame->Push(v1);
	frame->Push(v2);
	frame->Push(v3);
	frame->Push(v1);
      }
      break;
    case Instr::DUP2:
      {
	// to be done: handle one or two value form? how?
	word v2 = frame->Pop();
	word v1 = frame->Pop();
	frame->Push(v2);
	frame->Push(v1);
	frame->Push(v2);
	frame->Push(v1);
      }
      break;
    case Instr::DUP2_X1:
      {
	// to be done: handle two or three value form? how?
	word v3 = frame->Pop();
	word v2 = frame->Pop();
	word v1 = frame->Pop();
	frame->Push(v2);
	frame->Push(v1);
	frame->Push(v3);
	frame->Push(v2);
	frame->Push(v1);
      }
      break;
    case Instr::DUP2_X2:
      {
	// to be done: handle all four forms
      }
      break;
    case Instr::F2D:
      {
	// floats are reals
      }
      break;
    case Instr::F2I:
      {
	// to be done
      }
      break;
    case Instr::F2L:
      {
	// to be done
      }
      break;
    case Instr::FADD:
      {
	// to be done
      }
      break;
    case Instr::FCMPG:
      {
	// to be done
      }
      break;
    case Instr::FCMPL:
      {
      }
      break;
    case Instr::FCONST_0:
      {
	// to be done
	Chunk *res = Store::AllocChunk(0);
	frame->Push(res->ToWord());
      }
      break;
    case Instr::FCONST_1:
      {
	// to be done
	Chunk *res = Store::AllocChunk(0);
	frame->Push(res->ToWord());
      }
      break;
    case Instr::FCONST_2:
      {
	// to be done
	Chunk *res = Store::AllocChunk(0);
	frame->Push(res->ToWord());
      }
      break;
    case Instr::FDIV:
      {
	Chunk *v2 = Store::DirectWordToChunk(frame->Pop());
	Chunk *v1 = Store::DirectWordToChunk(frame->Pop());
	Chunk *res = v1;
	// to be done
	frame->Push(res->ToWord());
      }
      break;
    case Instr::FMUL:
      {
	Chunk *v2 = Store::DirectWordToChunk(frame->Pop());
	Chunk *v1 = Store::DirectWordToChunk(frame->Pop());
	Chunk *res = v1;
	// to be done
	frame->Push(res->ToWord());
      }
      break;
    case Instr::FNEG:
      {
	Chunk *v1 = Store::DirectWordToChunk(frame->Pop());
	Chunk *res = v1;
	// to be done
	frame->Push(res->ToWord());
      }
      break;
    case Instr::FREM:
      {
	Chunk *v2 = Store::DirectWordToChunk(frame->Pop());
	Chunk *v1 = Store::DirectWordToChunk(frame->Pop());
	Chunk *res = v1;
	// to be done
	frame->Push(res->ToWord());
      }
      break;
    case Instr::FSUB:
      {
	Chunk *v2 = Store::DirectWordToChunk(frame->Pop());
	Chunk *v1 = Store::DirectWordToChunk(frame->Pop());
	Chunk *res = v1;
	// to be done
	frame->Push(res->ToWord());
      }
      break;
    case Instr::GETFIELD:
      {
	unsigned char byte1 = code[++pc];
	unsigned char byte2 = code[++pc];
	u_int index = (byte1 << 8 | byte2);
	word object = frame->Pop();
	word res = object;
	// to be done
	frame->Push(res);
      }
      break;
    case Instr::GETSTATIC:
      {
	unsigned char byte1 = code[++pc];
	unsigned char byte2 = code[++pc];
	u_int index = (byte1 << 8 | byte2);
	word res = Store::IntToWord(0);
	// to be done
	frame->Push(res);
      }
      break;
    case Instr::GOTO:
      {
	unsigned char byte1 = code[pc + 1];
	unsigned char byte2 = code[pc + 2];
	// 16 bit signed offfset
	short int offset = ((byte1 << 8) | byte2);
	pc += offset;
      }
      break;
    case Instr::GOTO_W:
      {
	unsigned char byte1 = code[pc + 1];
	unsigned char byte2 = code[pc + 2];
	unsigned char byte3 = code[pc + 3];
	unsigned char byte4 = code[pc + 4];
	// 32 bit signed offfset
	int offset = ((byte1 << 24) | (byte2 << 16) | (byte3 << 8) | byte4);
	pc += offset;
      }
      break;
    case Instr::I2B:
    case Instr::I2C: // to be done
      {
	int i = Store::DirectWordToInt(frame->Pop());
	char byte = (char) i;
	int res = (int) byte;
	frame->Push(Store::IntToWord(res));
      }
      break;
    case Instr::I2D:
      {
	int i = Store::DirectWordToInt(frame->Pop());
	Chunk *res = Store::AllocChunk(0);
	// to be done
	frame->Push(res->ToWord());
      }
      break;
    case Instr::I2F:
      {
	// to be done
      }
      break;
    case Instr::I2L:
      {
	// to be done
      }
      break;
    case Instr::I2S:
      {
	Error("not implemented");
      }
      break;
    case Instr::IADD:
      {
	int v2 = Store::DirectWordToInt(frame->Pop());
	int v1 = Store::DirectWordToInt(frame->Pop());
	frame->Push(Store::IntToWord(v1 + v2));
      }
      break;
    case Instr::IAND:
      {
	int v2 = Store::DirectWordToInt(frame->Pop());
	int v1 = Store::DirectWordToInt(frame->Pop());
	frame->Push(Store::IntToWord(v1 & v2));
      }
      break;
    case Instr::ICONST_M1:
      {
	frame->Push(Store::IntToWord(-1));
      }
      break;
    case Instr::ICONST_0:
      {
	frame->Push(Store::IntToWord(0));
      }
      break;
    case Instr::ICONST_1:
      {
	frame->Push(Store::IntToWord(1));
      }
      break;
    case Instr::ICONST_2:
      {
	frame->Push(Store::IntToWord(2));
      }
      break;
    case Instr::ICONST_3:
      {
	frame->Push(Store::IntToWord(3));
      }
      break;
    case Instr::ICONST_4:
      {
	frame->Push(Store::IntToWord(4));
      }
      break;
    case Instr::ICONST_5:
      {
	frame->Push(Store::IntToWord(5));
      }
      break;
    case Instr::IDIV:
      {
	int v2 = Store::DirectWordToInt(frame->Pop());
	int v1 = Store::DirectWordToInt(frame->Pop());
	if (v2 != 0)
	  frame->Push(Store::IntToWord(v1 / v2));
	else {
	  // to be done: raise ArithmeticException
	}
      }
      break;
    case Instr::IF_ACMPEQ:
    case Instr::IF_ICMPEQ:
      {
	unsigned char b1    = code[pc + 1];
	unsigned char b2    = code[pc + 2];
	signed short offset = ((b1 << 8) | b2);
	word v2 = frame->Pop();
	word v1 = frame->Pop();
	if (v1 == v2)
	  pc += offset - 1;
	else
	  pc += 2;
      }
      break;
    case Instr::IF_ACMPNE:
    case Instr::IF_ICMPNE:
      {
	unsigned char b1    = code[pc + 1];
	unsigned char b2    = code[pc + 2];
	signed short offset = ((b1 << 8) | b2);
	word v2 = frame->Pop();
	word v1 = frame->Pop();
	if (v1 != v2)
	  pc += offset - 1;
	else
	  pc += 2;
      }
      break;
    case Instr::IF_ICMPLT:
      {
	unsigned char b1    = code[pc + 1];
	unsigned char b2    = code[pc + 2];
	signed short offset = ((b1 << 8) | b2);
	int v2 = Store::DirectWordToInt(frame->Pop());
	int v1 = Store::DirectWordToInt(frame->Pop());
	if (v1 < v2)
	  pc += offset - 1;
	else
	  pc += 2;
      }
      break;
    case Instr::IF_ICMPGE:
      {
	unsigned char b1    = code[pc + 1];
	unsigned char b2    = code[pc + 2];
	signed short offset = ((b1 << 8) | b2);
	int v2 = Store::DirectWordToInt(frame->Pop());
	int v1 = Store::DirectWordToInt(frame->Pop());
	if (v1 >= v2)
	  pc += offset - 1;
	else
	  pc += 2;
      }
      break;
    case Instr::IF_ICMPGT:
      {
	unsigned char b1    = code[pc + 1];
	unsigned char b2    = code[pc + 2];
	signed short offset = ((b1 << 8) | b2);
	int v2 = Store::DirectWordToInt(frame->Pop());
	int v1 = Store::DirectWordToInt(frame->Pop());
	if (v1 > v2)
	  pc += offset - 1;
	else
	  pc += 2;
      }
      break;
    case Instr::IF_ICMPLE:
      {
	unsigned char b1    = code[pc + 1];
	unsigned char b2    = code[pc + 2];
	signed short offset = ((b1 << 8) | b2);
	int v2 = Store::DirectWordToInt(frame->Pop());
	int v1 = Store::DirectWordToInt(frame->Pop());
	if (v1 <= v2)
	  pc += offset - 1;
	else
	  pc += 2;
      }
      break;
    case Instr::IFEQ:
      {
	unsigned char b1    = code[pc + 1];
	unsigned char b2    = code[pc + 2];
	signed short offset = ((b1 << 8) | b2);
	int v = Store::DirectWordToInt(frame->Pop());
	if (v == 0)
	  pc += offset - 1;
	else
	  pc += 2;
      }
      break;
    case Instr::IFNE:
      {
	unsigned char b1    = code[pc + 1];
	unsigned char b2    = code[pc + 2];
	signed short offset = ((b1 << 8) | b2);
	int v = Store::DirectWordToInt(frame->Pop());
	if (v != 0)
	  pc += offset - 1;
	else
	  pc += 2;
      }
      break;
    case Instr::IFLT:
      {
	unsigned char b1    = code[pc + 1];
	unsigned char b2    = code[pc + 2];
	signed short offset = ((b1 << 8) | b2);
	int v = Store::DirectWordToInt(frame->Pop());
	if (v < 0)
	  pc += offset - 1;
	else
	  pc += 2;
      }
      break;
    case Instr::IFGE:
      {
	unsigned char b1    = code[pc + 1];
	unsigned char b2    = code[pc + 2];
	signed short offset = ((b1 << 8) | b2);
	int v = Store::DirectWordToInt(frame->Pop());
	if (v >= 0)
	  pc += offset - 1;
	else
	  pc += 2;
      }
      break;
    case Instr::IFGT:
      {
	unsigned char b1    = code[pc + 1];
	unsigned char b2    = code[pc + 2];
	signed short offset = ((b1 << 8) | b2);
	int v = Store::DirectWordToInt(frame->Pop());
	if (v > 0)
	  pc += offset - 1;
	else
	  pc += 2;
      }
      break;
    case Instr::IFLE:
      {
	unsigned char b1    = code[pc + 1];
	unsigned char b2    = code[pc + 2];
	signed short offset = ((b1 << 8) | b2);
	int v = Store::DirectWordToInt(frame->Pop());
	if (v <= 0)
	  pc += offset - 1;
	else
	  pc += 2;
      }
      break;
    case Instr::IFNONNULL:
      {
	unsigned char b1    = code[pc + 1];
	unsigned char b2    = code[pc + 2];
	signed short offset = ((b1 << 8) | b2);
	word object         = frame->Pop();
	if (object != null)
	  pc += offset - 1;
	else
	  pc += 2;
      }
      break;
    case Instr::IFNULL:
      {
	unsigned char b1    = code[pc + 1];
	unsigned char b2    = code[pc + 2];
	signed short offset = ((b1 << 8) | b2);
	word object         = frame->Pop();
	if (object == null)
	  pc += offset - 1;
	else
	  pc += 2;
      }
      break;
    case Instr::IINC:
      {
	u_int index = code[++pc];
	int inc     = static_cast<int>(code[++pc]);
	int v       = Store::DirectWordToInt(frame->GetEnv(index));
	v += inc;
	frame->SetEnv(index, Store::IntToWord(v));
      }
      break;
    case Instr::IMUL:
      {
	int v2 = Store::DirectWordToInt(frame->Pop());
	int v1 = Store::DirectWordToInt(frame->Pop());
	frame->Push(Store::IntToWord(v1 * v2));
      }
      break;
    case Instr::INEG:
      {
	int v = Store::DirectWordToInt(frame->Pop());
	frame->Push(Store::IntToWord(0-v));
      }
      break;
    case Instr::INSTANCEOF:
      {
	unsigned char b1 = code[++pc];
	unsigned char b2 = code[++pc];
	u_int index      = ((b1 << 8) | b2);
	word object      = frame->Pop();
	// to be done
      }
      break;
    case Instr::INVOKEINTERFACE:
      {
	unsigned char b1    = code[pc + 1];
	unsigned char b2    = code[pc + 2];
	u_int index         = ((b1 << 8) | b2);
	unsigned char count = code[pc + 3];
	unsigned char zero  = code[pc + 4]; // never used
	pc += 4;
	// to be done
      }
      break;
    case Instr::INVOKESPECIAL:
      {
	unsigned char b1 = code[++pc];
	unsigned char b2 = code[++pc];
	u_int index      = ((b1 << 8) | b2);
	// to be done
      }
      break;
    case Instr::INVOKESTATIC:
      {
	unsigned char b1 = code[++pc];
	unsigned char b2 = code[++pc];
	u_int index      = ((b1 << 8) | b2);
	// to be done
      }
      break;
    case Instr::INVOKEVIRTUAL:
      {
	unsigned char b1 = code[++pc];
	unsigned char b2 = code[++pc];
	u_int index      = ((b1 << 8) | b2);
	// to be done
      }
      break;
    case Instr::IOR:
      {
	int v2 = Store::DirectWordToInt(frame->Pop());
	int v1 = Store::DirectWordToInt(frame->Pop());
	frame->Push(Store::IntToWord(v1 || v2));
      }
      break;
    case Instr::IREM:
      {
	int v2 = Store::DirectWordToInt(frame->Pop());
	int v1 = Store::DirectWordToInt(frame->Pop());
	if (v2 != 0)
	  frame->Push(Store::IntToWord(v1 % v2));
	else {
	  // to be done: raise ArithmeticException
	}
      }
      break;
    case Instr::ISHL:
      {
	int v2 = Store::DirectWordToInt(frame->Pop());
	int v1 = Store::DirectWordToInt(frame->Pop());
	frame->Push(Store::IntToWord(v1 << v2));
      }
      break;
    case Instr::ISHR:
      {
	int v2 = Store::DirectWordToInt(frame->Pop());
	int v1 = Store::DirectWordToInt(frame->Pop());
	frame->Push(Store::IntToWord(v1 >> v2));
      }
      break;
    case Instr::ISUB:
      {
	int v2 = Store::DirectWordToInt(frame->Pop());
	int v1 = Store::DirectWordToInt(frame->Pop());
	frame->Push(Store::IntToWord(v1 - v2));
      }
      break;
    case Instr::IUSHR:
      {
	Error("not implemented");
      }
      break;
    case Instr::IXOR:
      {
	int v2 = Store::DirectWordToInt(frame->Pop());
	int v1 = Store::DirectWordToInt(frame->Pop());
	frame->Push(Store::IntToWord(v1 ^ v2));
      }
      break;
    case Instr::JSR:
      {
	unsigned char b1    = code[pc + 1];
	unsigned char b2    = code[pc + 2];
	signed short offset = ((b1 << 8) | b2);
	// to be done
      }
      break;
    case Instr::JSR_W:
      {
	unsigned char b1  = code[pc + 1];
	unsigned char b2  = code[pc + 2];
	unsigned char b3  = code[pc + 3];
	unsigned char b4  = code[pc + 4];
	signed int offset = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
	// to be done
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
    case Instr::LCONST_1:
      {
	Error("not implemented");
      }
      break;
    case Instr::LDC:
      {
	u_int index = code[++pc];
	word value  = Store::IntToWord(0); // to be done
	frame->Push(value);
      }
      break;
    case Instr::LDC_W:
    case Instr::LDC2_W:
      {
	unsigned char b1 = code[++pc];
	unsigned char b2 = code[++pc];
	u_int index      = ((b1 << 8) | b2);
	word value       = Store::IntToWord(0); // to be done
	frame->Push(value);
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
	// Skip the zero padding bytes
	pc++;
	for (u_int i = 3; i--;)
	  if (code[pc] == 0)
	    pc++;
	unsigned char b1 = code[pc++];
	unsigned char b2 = code[pc++];
	unsigned char b3 = code[pc++];
	unsigned char b4 = code[pc++];
	// to be done
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
	word object = frame->Pop();
	// to be done
      }
      break;
    case Instr::MONITOREXIT:
      {
	word object = frame->Pop();
      }
      break;
    case Instr::MULTIANEWARRAY:
      {
	unsigned char b1     = code[++pc];
	unsigned char b2     = code[++pc];
	unsigned char nbDims = code[++pc];
	u_int index          = ((b1 << 8) | b2);
	u_int length         = 0;
	for (u_int i = nbDims; i--;) {
	  u_int curDim = Store::DirectWordToInt(frame->Pop());
	  if (curDim != 0)
	    length *= curDim;
	}
	ObjectArrayType *type = INVALID_POINTER; // to be done
	ObjectArray *arr = ObjectArray::New(type, length);
	frame->Push(arr->ToWord());
      }
      break;
    case Instr::NEW:
      {
	unsigned char b1 = code[++pc];
	unsigned char b2 = code[++pc];
	u_int index      = ((b1 << 8) | b2);
	// to be done
      }
      break;
    case Instr::NEWARRAY:
      {
	pc++; // Ignore ATYPE indicator
	int count = Store::DirectWordToInt(frame->Pop());
	ObjectArrayType *type = INVALID_POINTER; // to be done
	ObjectArray *arr = ObjectArray::New(type, count);
	frame->Push(arr->ToWord());
      }
      break;
    case Instr::NOP:
      break;
    case Instr::POP:
    case Instr::POP2:
      {
	frame->Pop();
      }
      break;
    case Instr::PUTFIELD:
      {
	unsigned char b1 = code[++pc];
	unsigned char b2 = code[++pc];
	u_int index      = ((b1 << 8) | b2);
	word value       = frame->Pop();
	word object      = frame->Pop();
	// to be done
      }
      break;
    case Instr::PUTSTATIC:
      {
	unsigned char b1 = code[++pc];
	unsigned char b2 = code[++pc];
	u_int index      = ((b1 << 8) | b2);
	word value       = frame->Pop();
	// to be done
      }
      break;
    case Instr::RET:
      {
	unsigned char index = code[++pc];
	Error("not implemented");
      }
    case Instr::RETURN:
      {
	Error("not implemented");
      }
    case Instr::SIPUSH:
      {
	unsigned char b1 = code[++pc];
	unsigned char b2 = code[++pc];
	int value        = ((b1 << 8) | b2);
	frame->Push(Store::IntToWord(value));
      }
    case Instr::SWAP:
      {
	word v1 = frame->Pop();
	word v2 = frame->Pop();
	frame->Push(v1);
	frame->Push(v2);
      }
      break;
    case Instr::TABLESWITCH:
      {
	Error("not implemented");
      }
      break;
    case Instr::WIDE:
      {
	switch (static_cast<Instr::Opcode>(code[++pc])) {
	case Instr::ILOAD:
	case Instr::FLOAD:
	case Instr::ALOAD:
	case Instr::LLOAD:
	  {
	    unsigned char b1 = code[++pc];
	    unsigned char b2 = code[++pc];
	    u_int index      = ((b1 << 8) | b2);
	    frame->Push(frame->GetEnv(index));
	  }
	  break;
	case Instr::ISTORE:
	case Instr::FSTORE:
	case Instr::ASTORE:
	case Instr::LSTORE:
	  {
	    unsigned char b1 = code[++pc];
	    unsigned char b2 = code[++pc];
	    u_int index      = ((b1 << 8) | b2);
	    frame->SetEnv(index, frame->Pop());
	  }
	  break;
	case Instr::IINC:
	  {
	    unsigned char b1 = code[++pc];
	    unsigned char b2 = code[++pc];
	    u_int index      = ((b1 << 8) | b2);
	    unsigned char c1 = code[++pc];
	    unsigned char c2 = code[++pc];
	    signed short inc = ((c1 << 8) | c2);
	    int value        = Store::DirectWordToInt(frame->GetEnv(index));
	    value += inc;
	    frame->SetEnv(inc, Store::IntToWord(value));
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
      frame->SetPC(pc + 1);
      return Worker::PREEMPT;
    }
    else
      pc++;
  }
}
