#if defined(INTERFACE)
#pragma implementation "java/ByteCodeInterpreter.hh"
#endif

#include <cstdio>
#include "generic/Scheduler.hh"
#include "generic/Backtrace.hh"
#include "generic/Closure.hh"
#include "generic/ConcreteCode.hh"

#include "java/Data.hh"
#include "java/NativeCodeInterpreter.hh"

//
// Interpreter Opcode
//

class Instr {
public:
  enum Opcode {
    AALOAD = 0x32,
    AASTORE = 0x53,
    ACONST_NULL = 0x01,
    ALOAD = 0x25,
    ALOAD_0 = 0x2a,
    ALOAD_1 = 0x2b,
    ALOAD_2 = 0x2c,
    ALOAD_3 = 0x2d,
    ANEWARRAY = 0xbd,
    ARETURN = 0xb0,
    ARRAYLENGTH = 0xbe,
    ASTORE = 0x3a,
    ASTORE_0 = 04b,
    ASTORE_1 = 04c,
    ASTORE_2 = 04d,
    ASTORE_3 = 04e,
    ATHROW   = 0xbf,
    BALOAD   = 0x33,
    BASTORE  = 0x54,
    BIPUSH   = 0x10,
    CALOAD   = 0x34,
    CASTORE  = 0x55,
    CHECKCAST = 0xc0,
    D2F       = 0x90,
    D2I       = 0x8e,
    DADD      = 0x63,
    DALOAD    = 0x31,
    DASTORE   = 0x52,
    DCMPG     = 0x98,
    DCMPL     = 0x97,
    DCONST_0  = 0x0e,
    DCONST_1  = 0x0f,
    DDIV      = 0x6f,
    DLOAD     = 0x18,
    DLOAD_0   = 0x26,
    DLOAD_1   = 0x27,
    DLOAD_2   = 0x28,
    DLOAD_3   = 0x29,
    DMUL      = 0x6b,
    DNEG      = 0x77,
    DREM      = 0x73,
    DRETURN   = 0xaf,
    DSTORE    = 0x39,
    DSTORE_0  = 0x47,
    DSTORE_1  = 0x48,
    DSTORE_2  = 0x49,
    DSTORE_3  = 0x4A,
    DSUB      = 0x67,
    DUP       = 0x59,
    DUP_X1    = 0x5a,
    DUP_X2    = 0x5b,
    DUP2      = 0x5c,
    DUP2_X1   = 0x5d,
    DUP2_X2   = 0x5e,
    F2D       = 0x8d,
    F2I       = 0x8b,
    F2L       = 0x8c,
    FADD      = 0x62,
    FALOAD    = 0x30,
    FASTORE   = 0x51,
    FCMPG     = 0x96,
    FCMPL     = 0x95,
    FCONST_0  = 0x0b,
    FCONST_1  = 0x0c,
    FCONST_2  = 0x0d,
    FDIV      = 0x6e,
    FLOAD     = 0x17,
    FLOAD_0   = 0x22,
    FLOAD_1   = 0x23,
    FLOAD_2   = 0x24,
    FLOAD_3   = 0x25,
    FMUL      = 0x6a,
    FNEG      = 0x76,
    FREM      = 0x72,
    FRETURN   = 0xae,
    FSTORE    = 0x38,
    FSTORE_0  = 0x43,
    FSTORE_1  = 0x44,
    FSTORE_2  = 0x45,
    FSTORE_3  = 0x46,
    FSUB      = 0x66,
    GETFIELD  = 0xb4,
    GETSTATIC = 0xb2,
    GOTO      = 0xa7,
    GOTO_W    = 0xc8,
    I2B       = 0x91,
    I2C       = 0x92,
    I2D       = 0x87,
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
    StackFrame *frame = StackFrame::New(BYTE_CODE_FRAME, interpreter, frSize);
    frame->InitArg(PC_POS, pc);
    frame->InitArg(TOP_POS, Store::IntToWord(BASE_SIZE + nbLocals));
    frame->InitArg(CODE_POS, code->ToWord());
    frame->InitArg(CLOSURE_POS, closure->ToWord());
    return static_cast<ByteCodeFrame *>(frame);
  }
  // ByteCodeFrame Untagging
  static ByteCodeFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == BYTE_CODE_FRAME);
    return static_cast<ByteCodeFrame *>(p);
  }
};

class ByteCodeHandlerFrame : public StackFrame {
protected:
  enum { PC_POS, FRAME_POS, BASE_SIZE }
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
    StackFrame *frame = StackFrame::New(BYTE_CODE_HANDLER_FRAME,
					interpreter, BASE_SIZE);
    frame->InitArg(PC_POS, pc);
    frame->InitArg(FRAME_POS, codeFrame->ToWord());
    return static_cast<ByteCodeHandlerFrame *>(frame);
  }
  // ByteCodeHandlerFrame Untagging
  static ByteCodeHandlerFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == BYTE_CODE_HANDLER_FRAME);
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


Worker::Result ByteCodeInterpreter::Run() {
  ByteCodeFrame *frame = ByteCodeFrame::FromWordDirect(Scheduler::GetFrame());
  int pc               = frame->GetPC();
  Chunk *codeChunk     = frame->GetCode();
  char *code           = codeChunk->GetBase();
 interpret:
  switch (static_cast<Instr::Opcode>(code[pc])) {
  case Instr::AALOAD:
  case Instr::DALOAD: // reals are boxed
  case Instr::FALOAD: // reals are boxed
    {
      u_int index = Store::DirectWordToInt(frame->Pop());
      Array *arr  = Array::FromWord(frame->Pop());
      if (arr != INVALID_POINTER) {
	if (index < arr->GetSize()) {
	  frame->Push(arr->GetArg(index));
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
    {
      word value  = frame->Pop();
      u_int index = Store::DirectWordToInt(frame->Pop());
      Array *arr  = Array::FromWordDirect(frame->Pop());
      if (index < arr->GetSize()) {
	arr->ReplaceArg(index, value);
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
    {
      u_int index = (u_int) code[++pc];
      frame->Push(frame->GetEnv(index));
    }
    break;
  case Instr::ALOAD_0:
  case Instr::DLOAD_0:
  case Instr::FLOAD_0:
    {
      frame->Push(frame->GetEnv(0));
    }
    break;
  case Instr::ALOAD_1:
  case Instr::DLOAD_1:
  case Instr::FLOAD_1:
    {
      frame->Push(frame->GetEnv(1));
    }
    break;
  case Instr::ALOAD_2:
  case Instr::DLOAD_2:
  case Instr::FLOAD_2:
    {
      frame->Push(frame->GetEnv(2));
    }
    break;
  case Instr::ALOAD_3:
  case Instr::DLOAD_3:
  case Instr::FLOAD_3:
    {
      frame->Push(frame->GetEnv(3));
    }
    break;
  case Instr::ANEWARRAY:
    {
      // to be done: what is with the index bytes?
      int count = Store::DirectWordToInt(frame->Pop());
      if (count >= 0) {
	Arrary *arr = Array::New(count);
	for (u_int i = count; i--;)
	  arr->InitArg(i, Store::IntToWord(0));
	frame->Push(arr->ToWord());
      }
      else {
	to be done: thow invalid something
      }
    }
    break;
  case Instr::ARETURN:
  case Instr::DRETURN: // reals are boxed
  case Instr::FRETURN: // reals are boxed
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
      Array *arr = Array::FromWord(frame->Pop());
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
    {
      u_int index = code[++pc];
      frame->SetEnv(index, frame->Pop());
    }
    break;
  case Instr::ASTORE_0:
  case Instr::DSTORE_0: // reals are boxed
  case Instr::FSTORE_0: // reals are boxed
    {
      frame->SetEnv(0, frame->Pop());
    }
    break;
  case Instr::ASTORE_1:
  case Instr::DSTORE_1: // reals are boxed
  case Instr::FSTORE_1: // reals are boxed
    {
      frame->SetEnv(1, frame->Pop());
    }
    break;
  case Instr::ASTORE_2:
  case Instr::DSTORE_2: // reals are boxed
  case Instr::FSTORE_2: // reals are boxed
    {
      frame->SetEnv(2, frame->Pop());
    }
    break;
  case Instr::ASTORE_3:
  case Instr::DSTORE_3: // reals are boxed
  case Instr::FSTORE_3: // reals are boxed
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
      frame->Push();
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
      if (StatusWord::GetStatus() != 0)
	frame->SetPC(pc);
      else
	goto interpret;
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
      if (StatusWord::GetStatus() != 0)
	frame->SetPC(pc);
      else
	goto interpret;
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
      
    }
    break;
  default:
    Error("invalid opcode");
  }
  if (StatusWord::GetStatus() != 0)
    frame->SetPC(pc + 1);
  else {
    pc++;
    goto interpret;
  }
}
