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
#include <cmath>
#include "generic/Scheduler.hh"
#include "generic/Backtrace.hh"
#include "generic/Closure.hh"
#include "generic/ConcreteCode.hh"

#include "java/Opcodes.hh"
#include "java/Data.hh"
#include "java/ThrowWorker.hh"
#include "java/JavaByteCode.hh"
#include "java/ByteCodeInterpreter.hh"
#include "java/ClassInfo.hh"

static bool IsSubtypeOf(Type *type1, Type *type2) {
  switch (type1->GetLabel()) {
  case JavaLabel::Class:
    return type2->GetLabel() == JavaLabel::Class &&
      static_cast<Class *>(type1)->IsSubtypeOf(static_cast<Class *>(type2));
  case JavaLabel::PrimitiveType:
    switch (type2->GetLabel()) {
    case JavaLabel::Class:
      //--** can be one of the interfaces implemented by arrays (2.15)
      return static_cast<Class *>(type2)->IsJavaLangObject();
    case JavaLabel::PrimitiveType:
      return (static_cast<PrimitiveType *>(type1)->GetType() ==
	      static_cast<PrimitiveType *>(type2)->GetType());
    case JavaLabel::ArrayType:
      return false;
    default:
      Error("invalid type");
    }
  case JavaLabel::ArrayType:
    switch (type2->GetLabel()) {
    case JavaLabel::Class:
      //--** can be one of the interfaces implemented by arrays (2.15)
      return static_cast<Class *>(type2)->IsJavaLangObject();
    case JavaLabel::PrimitiveType:
      return false;
    case JavaLabel::ArrayType:
      {
	Type *elementType1 = static_cast<ArrayType *>(type1)->GetElementType();
	Type *elementType2 = static_cast<ArrayType *>(type2)->GetElementType();
	return IsSubtypeOf(elementType1, elementType2);
      }
    default:
      Error("invalid type");
    }
  default:
    Error("invalid value");
  }
}

static bool IsInstanceOf(word value, Type *type) {
  Block *b = Store::WordToBlock(value);
  if (b == INVALID_POINTER) {
    Assert(Store::WordToInt(value) == 0); // null
    return true;
  }
  switch (b->GetLabel()) {
  case JavaLabel::Object:
    return type->GetLabel() == JavaLabel::Class &&
      static_cast<Object *>(b)->IsInstanceOf(static_cast<Class *>(type));
  case JavaLabel::BaseArray:
    switch (type->GetLabel()) {
    case JavaLabel::Class:
      //--** can be one of the interfaces implemented by arrays (2.15)
      return static_cast<Class *>(type)->IsJavaLangObject();
    case JavaLabel::PrimitiveType:
      return false;
    case JavaLabel::ArrayType:
      {
	Type *elementType = static_cast<ArrayType *>(type)->GetElementType();
	return elementType->GetLabel() == JavaLabel::PrimitiveType &&
	  static_cast<PrimitiveType *>(elementType)->GetType() ==
	  static_cast<BaseArray *>(b)->GetElementType();
      }
    default:
      Error("invalid type");
    }
  case JavaLabel::ObjectArray:
    switch (type->GetLabel()) {
    case JavaLabel::Class:
      //--** can be one of the interfaces implemented by arrays (2.15)
      return static_cast<Class *>(type)->IsJavaLangObject();
    case JavaLabel::PrimitiveType:
      return false;
    case JavaLabel::ArrayType:
      {
	Type *elementType1 = static_cast<ObjectArray *>(b)->GetElementType();
	Type *elementType2 = static_cast<ArrayType *>(type)->GetElementType();
	return IsSubtypeOf(elementType1, elementType2);
      }
    default:
      Error("invalid type");
    }
  default:
    Error("invalid value");
  }
}

//
// Interpreter StackFrames
//
class ByteCodeFrame : public StackFrame {
protected:
  enum {
    SIZE_POS,
    PC_POS,
    CONT_PC_POS,
    TOP_POS,
    POOL_POS,
    BYTE_CODE_POS,
    CODE_POS,
    BASE_SIZE
  };
  
  void SetTop(u_int top) {
    StackFrame::ReplaceArg(TOP_POS, top);
  }
  u_int GetTop() {
    return Store::DirectWordToInt(StackFrame::GetArg(TOP_POS));
  }
public:
  // ByteCodeFrame Accessors
  u_int GetSize() {
    return Store::DirectWordToInt(StackFrame::GetArg(SIZE_POS));
  }
  s_int GetPC() {
    return Store::DirectWordToInt(StackFrame::GetArg(PC_POS));
  }
  void SetPC(s_int pc) {
    StackFrame::InitArg(PC_POS, pc);
  }
  s_int GetContPC() {
    return Store::DirectWordToInt(StackFrame::GetArg(CONT_PC_POS));
  }
  void SetContPC(s_int pc) {
    StackFrame::InitArg(CONT_PC_POS, pc);
  }
  Chunk *GetCode() {
    return Store::DirectWordToChunk(StackFrame::GetArg(CODE_POS));
  }
  RuntimeConstantPool *GetRuntimeConstantPool() {
    return RuntimeConstantPool::FromWordDirect(StackFrame::GetArg(POOL_POS));
  }
  Table *GetExceptionTable() {
    JavaByteCode *byteCode =
      JavaByteCode::FromWord(StackFrame::GetArg(BYTE_CODE_POS));
    return byteCode->GetExceptionTable();
  }
  MethodInfo *GetMethodInfo() {
    JavaByteCode *byteCode =
      JavaByteCode::FromWord(StackFrame::GetArg(BYTE_CODE_POS));
    return byteCode->GetMethodInfo();
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
			    s_int pc,
			    s_int contPC,
			    JavaByteCode *byteCode,
			    word runtimeConstantPool) {
    u_int maxLocals   = byteCode->GetMaxLocals();
    u_int maxStack    = byteCode->GetMaxStack();
    u_int frSize      = BASE_SIZE + maxLocals + maxStack;
    NEW_STACK_FRAME(frame, interpreter, frSize);
    frame->InitArg(SIZE_POS, frame->GetSize() + frSize);
    frame->InitArg(PC_POS, pc);
    frame->InitArg(CONT_PC_POS, contPC);
    frame->InitArg(TOP_POS, BASE_SIZE + maxLocals);
    frame->InitArg(CODE_POS, byteCode->GetCode()->ToWord());
    frame->InitArg(POOL_POS, runtimeConstantPool);
    frame->InitArg(BYTE_CODE_POS, byteCode->ToWord());
    return static_cast<ByteCodeFrame *>(frame);
  }
};

//
// Unlock Worker
//
class UnlockWorker : public Worker {
public:
  static UnlockWorker *self;
  
  static void Init() {
    self = new UnlockWorker();
  }

  static void PushFrame(Lock *lock);
  static void Release(StackFrame *sFrame);
  
  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Result Run(StackFrame *sFrame);
  virtual Result Handle(word data);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

UnlockWorker *UnlockWorker::self;

class UnlockFrame : public StackFrame {
protected:
  enum {
    LOCK_POS, SIZE
  };
public:
  // UnlockFrame Accessors
  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  Lock *GetLock() {
    return Lock::FromWordDirect(StackFrame::GetArg(LOCK_POS));
  }
  // UnlockFrame Constructor
  static UnlockFrame *New(Worker *worker, Lock *lock) {
    NEW_STACK_FRAME(frame, worker, SIZE);
    frame->InitArg(LOCK_POS, lock->ToWord());
    return static_cast<UnlockFrame *>(frame);
  }
};

void UnlockWorker::PushFrame(Lock *lock) {
  UnlockFrame::New(UnlockWorker::self, lock);
  Scheduler::PushHandler(Store::IntToWord(0));
}

void UnlockWorker::Release(StackFrame *sFrame) {
  UnlockFrame *frame = static_cast<UnlockFrame *>(sFrame);
  Assert(sFrame->GetWorker() == UnlockWorker::self);
  Scheduler::PopHandler();
  Lock *lock = frame->GetLock();
  Scheduler::PopFrame(frame->GetSize());
  lock->Release();
}

u_int UnlockWorker::GetFrameSize(StackFrame *sFrame) {
  UnlockFrame *frame = static_cast<UnlockFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result UnlockWorker::Run(StackFrame *sFrame) {
  Release(sFrame);
  return Worker::CONTINUE;
}

Worker::Result UnlockWorker::Handle(word) {
  Release(Scheduler::GetFrame());
  return Worker::RAISE;
}

const char *UnlockWorker::Identify() {
  return "UnlockWorker";
}

void UnlockWorker::DumpFrame(StackFrame *) {
  std::fprintf(stderr, "Unlock\n");
}

//
// Interpreter Types
//
class Word {
public:
  static word Deref(word a) {
    return a;
  }
};

class ReturnAddress {
public:
  static word ToWord(u_int pc) {
    return Store::IntToWord(pc);
  }
  static u_int FromWord(word x) {
    return Store::DirectWordToInt(x);
  }
};

// to be done: check compliance of java spec with c spec
class JavaFloat {
public:
  static word ToWord(double value) {
    return Float::New(value)->ToWord();
  }
  static Float *FromWord(word value) {
    return Float::FromWordDirect(value);
  }
};

// to be done: check compliance of java spec with c spec
class JavaDouble {
public:
  static word ToWord(double value) {
    return Double::New(value)->ToWord();
  }
  static Double *FromWord(word value) {
    return Double::FromWordDirect(value);
  }
};

//
// Helper Stuff
//
#define GET_BYTE_INDEX() \
  (code[pc + 1])

#define GET_POOL_INDEX() \
  ((code[pc + 1] << 8) | code[pc + 2])

#define GET_POOL_VALUE(index) \
  (pool->Get(index))

#define GET_WIDE_INDEX() \
  ((code[pc + 1] << 24) | (code[pc + 2] << 16) \
  | (code[pc + 3] << 8) | code[pc + 4])

#define FILL_SLOT() \
  frame->Push(null);

#define DROP_SLOT() { \
  word value = frame->Pop(); \
  Assert(value == null); value = value; \
}

#define DECLARE_LONG(v) \
  DROP_SLOT(); \
  s_int64 v = JavaLong::FromWordDirect(frame->Pop())->GetValue();

#define PUSH_LONG(v) \
  frame->Push(JavaLong::New(v)->ToWord()); \
  FILL_SLOT()

#define DECLARE_DOUBLE(v) \
  DROP_SLOT(); \
  double v = JavaDouble::FromWord(frame->Pop())->GetValue();

#define PUSH_DOUBLE(v) \
  frame->Push(JavaDouble::ToWord(v)); \
  FILL_SLOT()

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

#define RAISE_EXCEPTION(exn) { \
  word wFrame = frame->Clone(); \
  Scheduler::PopFrame(frame->GetSize()); \
  Scheduler::currentData = exn; \
  Scheduler::currentBacktrace = Backtrace::New(wFrame); \
  return Worker::RAISE; \
}

#define RAISE_VM_EXCEPTION(exn, mesg) { \
  ThrowWorker::PushFrame(ThrowWorker::exn, JavaString::New(mesg)); \
  Scheduler::nArgs = 0; \
  return Worker::CONTINUE; \
}

#define XASTORE(name, op, value) { \
  JavaDebug::Print(name); \
  u_int index      = JavaInt::FromWord(frame->Pop()); \
  BaseArray *array = BaseArray::FromWord(frame->Pop()); \
  if (array != INVALID_POINTER) { \
    if (index < array->GetLength()) \
      array->op(index, value); \
    else { \
      RAISE_VM_EXCEPTION(ArrayIndexOutOfBoundsException, name); \
    } \
  } \
  else { \
    RAISE_VM_EXCEPTION(NullPointerException, name); \
  } \
  pc += 1; \
}

#define XALOAD(name, op, value) { \
  JavaDebug::Print(name); \
  u_int index      = JavaInt::FromWord(frame->Pop()); \
  BaseArray *array = BaseArray::FromWord(frame->Pop()); \
  if (array != INVALID_POINTER) { \
    if (index < array->GetLength()) \
      value = array->op(index); \
    else { \
      RAISE_VM_EXCEPTION(ArrayIndexOutOfBoundsException, name); \
    } \
  } \
  else { \
    RAISE_VM_EXCEPTION(NullPointerException, name); \
  } \
  pc += 1; \
}

class JavaDebug {
public:
#if defined(JAVA_INTERPRETER_DEBUG)
  static void Print(const char *s) {
    std::fprintf(stderr, "%s\n", s);
  }
#else
  static void Print(const char *) {}
#endif
};

//
// Interpreter Functions
//
ByteCodeInterpreter *ByteCodeInterpreter::self;

void ByteCodeInterpreter::Init() {
  UnlockWorker::Init();
  self = new ByteCodeInterpreter();
}

Transform *
ByteCodeInterpreter::GetAbstractRepresentation(ConcreteRepresentation *) {
  return INVALID_POINTER; // to be done
}

void ByteCodeInterpreter::PushCall(Closure *closure) {
  JavaByteCode *concreteCode =
    JavaByteCode::FromWord(closure->GetConcreteCode());
  ByteCodeFrame::New(ByteCodeInterpreter::self,
		     -1,
		     0,
		     concreteCode,
		     closure->Sub(0)); // RuntimeConstantPool
}

u_int ByteCodeInterpreter::GetFrameSize(StackFrame *sFrame) {
  ByteCodeFrame *frame = static_cast<ByteCodeFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result ByteCodeInterpreter::Run(StackFrame *sFrame) {
  ByteCodeFrame *frame = static_cast<ByteCodeFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  s_int pc = frame->GetPC();
  u_int8 *code = reinterpret_cast<u_int8 *>(frame->GetCode()->GetBase());
  RuntimeConstantPool *pool = frame->GetRuntimeConstantPool();
  // to be done: more efficient solution
  if (pc == -1) {
    // Copy arguments to local variables
    // to be done: support large arguments
    if (Scheduler::nArgs == Scheduler::ONE_ARG)
      frame->SetEnv(0, Scheduler::currentArgs[0]);
    else
      for (u_int i = Scheduler::nArgs; i--;)
	frame->SetEnv(i, Scheduler::currentArgs[i]);
    if (frame->GetExceptionTable()->GetCount() != 0)
      Scheduler::PushHandler(Store::IntToWord(0));
    pc = frame->GetContPC();
  }
  else if (pc == -2) {
    // Push Return Value to stack
    if (Scheduler::nArgs == Scheduler::ONE_ARG)
      frame->Push(Scheduler::currentArgs[0]);
    else
      for (u_int i = 0; i < Scheduler::nArgs; i++)
	frame->Push(Scheduler::currentArgs[i]);
    if (frame->GetExceptionTable()->GetCount() != 0)
      Scheduler::PopHandler();
    pc = frame->GetContPC();
  }
  while (true) {
    switch (static_cast<Instr::Opcode>(code[pc])) {
    case Instr::AALOAD:
      {
	JavaDebug::Print("AALOAD");
	u_int index        = JavaInt::FromWord(frame->Pop());
	ObjectArray *array = ObjectArray::FromWord(frame->Pop());
	if (array != INVALID_POINTER) {
	  if (index < array->GetLength())
	    frame->Push(array->Load(index));
	  else {
	    RAISE_VM_EXCEPTION(ArrayIndexOutOfBoundsException, "AALOAD");
	  }
	}
	else {
	  RAISE_VM_EXCEPTION(NullPointerException, "AALOAD");
	}
	pc += 1;
      }
      break;
    case Instr::BALOAD:
      {
	// to be done: distinguish byte and boolean
	u_int value;
	XALOAD("BALOAD", LoadByte, value);
	frame->Push(JavaInt::ToWord(value));
      }
      break;
    case Instr::CALOAD:
      {
	u_int value;
	XALOAD("CALOAD", LoadChar, value);
	frame->Push(JavaInt::ToWord(value));
      }
      break;
    case Instr::FALOAD:
      {
	Float *value;
	XALOAD("FALOAD", LoadFloat, value);
	frame->Push(value->ToWord());
      }
      break;
    case Instr::IALOAD:
      {
	u_int value;
	XALOAD("IALOAD", LoadInt, value);
	frame->Push(JavaInt::ToWord(value));
      }
      break;
    case Instr::SALOAD:
      {
	u_int value;
	XALOAD("SALOAD", LoadShort, value);
	frame->Push(JavaInt::ToWord(value));
      }
      break;
    case Instr::LALOAD:
      {
	JavaLong *value;
	XALOAD("LALOAD", LoadLong, value);
	frame->Push(value->ToWord());
	FILL_SLOT();
      }
      break;
    case Instr::DALOAD:
      {
	Double *value;
	XALOAD("DALOAD", LoadDouble, value);
	frame->Push(value->ToWord());
	FILL_SLOT();
      }
      break;
    case Instr::AASTORE:
      {
	JavaDebug::Print("AASTORE");
	word value         = frame->Pop();
	u_int index        = JavaInt::FromWord(frame->Pop());
	ObjectArray *array = ObjectArray::FromWord(frame->Pop());
	if (array != INVALID_POINTER) {
	  if (index < array->GetLength()) {
	    if (IsInstanceOf(value, array->GetElementType())) {
	      array->Store(index, value);
	    }
	    else {
	      RAISE_VM_EXCEPTION(ArrayStoreException, "AASTORE");
	    }
	  }
	  else {
	    RAISE_VM_EXCEPTION(ArrayIndexOutOfBoundsException, "AASTORE");
	  }
	}
	else {
	  RAISE_VM_EXCEPTION(NullPointerException, "AASTORE");
	}
	pc += 1;
      }
      break;
    case Instr::BASTORE:
      {
	// to be done: distinguish byte and boolean
	u_int value = JavaInt::FromWord(frame->Pop());
	XASTORE("BASTORE", StoreByte, value);
      }
      break;
    case Instr::CASTORE:
      {
	u_int value = JavaInt::FromWord(frame->Pop());
	XASTORE("CASTORE", StoreChar, value);
      }
      break;
    case Instr::FASTORE:
      {
	Float *value = JavaFloat::FromWord(frame->Pop());
	XASTORE("FASTORE", StoreFloat, value);
      }
      break;
    case Instr::IASTORE:
      {
	u_int value = JavaInt::FromWord(frame->Pop());
	XASTORE("IASTORE", StoreInt, value);
      }
      break;
    case Instr::SASTORE:
      {
	u_int value = JavaInt::FromWord(frame->Pop());
	XASTORE("SASTORE", StoreShort, value);
      }
      break;
    case Instr::DASTORE:
      {
	DROP_SLOT();
	Double *value = JavaDouble::FromWord(frame->Pop());
	XASTORE("DASTORE", StoreDouble, value);
      }
      break;
    case Instr::LASTORE:
      {
	DROP_SLOT();
	JavaLong *value = JavaLong::FromWord(frame->Pop());
	XASTORE("LASTORE", StoreLong, value);
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
    case Instr::FLOAD:
    case Instr::ILOAD:
      {
	JavaDebug::Print("(A|F|I)LOAD");
	word value = frame->GetEnv(GET_BYTE_INDEX()); 
	frame->Push(value);
	pc += 2;
      }
      break;
    case Instr::DLOAD:
    case Instr::LLOAD:
      {
	JavaDebug::Print("(D|L)LOAD");
	word value = frame->GetEnv(GET_BYTE_INDEX()); 
	frame->Push(value);
	FILL_SLOT();
	pc += 2;
      }
      break;
    case Instr::ALOAD_0:
    case Instr::FLOAD_0:
    case Instr::ILOAD_0:
      {
	JavaDebug::Print("(A|F|I)LOAD_0");
	word value = frame->GetEnv(0);
	frame->Push(value);
	pc += 1;
      }
      break;
    case Instr::DLOAD_0:
    case Instr::LLOAD_0:
      {
	JavaDebug::Print("(D|L)LOAD_0");
	word value = frame->GetEnv(0);
	frame->Push(value);
	FILL_SLOT();
	pc += 1;
      }
      break;
    case Instr::ALOAD_1:
    case Instr::FLOAD_1:
    case Instr::ILOAD_1:
      {
	JavaDebug::Print("(A|F|I)LOAD_1");
	word value = frame->GetEnv(1);
	frame->Push(value);
	pc += 1;
      }
      break;
    case Instr::DLOAD_1:
    case Instr::LLOAD_1:
      {
	JavaDebug::Print("(D|L)LOAD_1");
	word value = frame->GetEnv(1);
	frame->Push(value);
	FILL_SLOT();
	pc += 1;
      }
      break;
    case Instr::ALOAD_2:
    case Instr::FLOAD_2:
    case Instr::ILOAD_2:
      {
	JavaDebug::Print("(A|F|I)LOAD_2");
	word value = frame->GetEnv(2);
	frame->Push(value);
	pc += 1;
      }
      break;
    case Instr::DLOAD_2:
    case Instr::LLOAD_2:
      {
	JavaDebug::Print("(D|L)LOAD_2");
	word value = frame->GetEnv(2);
	frame->Push(value);
	FILL_SLOT();
	pc += 1;
      }
      break;
    case Instr::ALOAD_3:
    case Instr::FLOAD_3:
    case Instr::ILOAD_3:
      {
	JavaDebug::Print("(A|F|I)LOAD_3");
	word value = frame->GetEnv(3);
	frame->Push(value);
	pc += 1;
      }
      break;
    case Instr::DLOAD_3:
    case Instr::LLOAD_3:
      {
	JavaDebug::Print("(D|L)LOAD_3");
	word value = frame->GetEnv(3);
	frame->Push(value);
	FILL_SLOT();
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
	  frame->Push(ObjectArray::New(type, count)->ToWord());
	}
	else {
	  RAISE_VM_EXCEPTION(NegativeArraySizeException, "ANEWARRAY");
	}
      }
      pc += 3;
      break;
    case Instr::ARETURN:
    case Instr::FRETURN:
    case Instr::IRETURN:
      {
	JavaDebug::Print("(A|F|I)RETURN");
	Scheduler::nArgs          = Scheduler::ONE_ARG;
	Scheduler::currentArgs[0] = frame->Pop();
	Scheduler::PopFrame(frame->GetSize());
	CHECK_PREEMPT();
      }
      break;
    case Instr::DRETURN:
    case Instr::LRETURN:
      {
	JavaDebug::Print("(D|L)RETURN");
	Scheduler::nArgs          = 2;
	Scheduler::currentArgs[1] = frame->Pop();
	Scheduler::currentArgs[0] = frame->Pop();
	Scheduler::PopFrame(frame->GetSize());
	CHECK_PREEMPT();
      }
      break;
    case Instr::ARRAYLENGTH:
      {
	JavaDebug::Print("ARRAYLENGTH");
	Block *p = Store::WordToBlock(frame->Pop());
	if (p != INVALID_POINTER) {
	  u_int length;
	  switch (p->GetLabel()) {
	  case JavaLabel::BaseArray:
	    length = static_cast<BaseArray *>(p)->GetLength();
	    break;
	  case JavaLabel::ObjectArray:
	    length = static_cast<ObjectArray *>(p)->GetLength();
	    break;
	  default:
	    Error("unkown type");
	  }
	  frame->Push(JavaInt::ToWord(length));
	  pc += 1;
	}
	else {
	  RAISE_VM_EXCEPTION(NullPointerException, "ARRAYLENGTH");
	}
      }
      break;
    case Instr::ASTORE:
    case Instr::FSTORE:
    case Instr::ISTORE:
      {
	JavaDebug::Print("(A|F|I)STORE");
	word value = frame->Pop();
	frame->SetEnv(GET_BYTE_INDEX(), value);
	pc += 2;
      }
      break;
    case Instr::LSTORE:
    case Instr::DSTORE:
      {
	JavaDebug::Print("(D|L)STORE");
	DROP_SLOT();
	word value = frame->Pop();
	frame->SetEnv(GET_BYTE_INDEX(), value);
	pc += 2;
      }
      break;
    case Instr::ASTORE_0:
    case Instr::FSTORE_0:
    case Instr::ISTORE_0:
      {
	JavaDebug::Print("(A|F|I)STORE_0");
	word value = frame->Pop();
	frame->SetEnv(0, value);
	pc += 1;
      }
      break;
    case Instr::LSTORE_0:
    case Instr::DSTORE_0:
      {
	JavaDebug::Print("(A|L)STORE_0");
	DROP_SLOT();
	word value = frame->Pop();
	frame->SetEnv(0, value);
	pc += 1;
      }
      break;
    case Instr::ASTORE_1:
    case Instr::FSTORE_1:
    case Instr::ISTORE_1:
      {
	JavaDebug::Print("(A|F|I)STORE_1");
	word value = frame->Pop();
	frame->SetEnv(1, value);
	pc += 1;
      }
      break;
    case Instr::LSTORE_1:
    case Instr::DSTORE_1:
      {
	JavaDebug::Print("(D|L)STORE_1");
	DROP_SLOT();
	word value = frame->Pop();
	frame->SetEnv(1, value);
	pc += 1;
      }
      break;
    case Instr::ASTORE_2:
    case Instr::FSTORE_2:
    case Instr::ISTORE_2:
      {
	JavaDebug::Print("(A|F|I)STORE_2");
	word value = frame->Pop();
	frame->SetEnv(2, value);
	pc += 1;
      }
      break;
    case Instr::LSTORE_2:
    case Instr::DSTORE_2:
      {
	JavaDebug::Print("(D|L)STORE_2");
	DROP_SLOT();
	word value = frame->Pop();
	frame->SetEnv(2, value);
	pc += 1;
      }
      break;
    case Instr::ASTORE_3:
    case Instr::FSTORE_3:
    case Instr::ISTORE_3:
      {
	JavaDebug::Print("(A|F|I)STORE_3");
	word value = frame->Pop();
	frame->SetEnv(3, value);
	pc += 1;
      }
      break;
    case Instr::LSTORE_3:
    case Instr::DSTORE_3:
      {
	JavaDebug::Print("(D|L)STORE_3");
	DROP_SLOT();
	word value = frame->Pop();
	frame->SetEnv(3, value);
	pc += 1;
      }
      break;
    case Instr::ATHROW:
      {
	Object *exn = Object::FromWord(frame->Pop());
	if (exn != INVALID_POINTER) {
	  RAISE_EXCEPTION(exn->ToWord());
	}
	else {
	  RAISE_VM_EXCEPTION(NullPointerException, "ATHROW");
	}
      }
      break;
    case Instr::BIPUSH:
      {
	JavaDebug::Print("BIPUSH");
	frame->Push(JavaInt::ToWord(static_cast<s_int8>(GET_BYTE_INDEX())));
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
	if (IsInstanceOf(wObject, type)) {
	  frame->Push(wObject);
	}
	else {
	  RAISE_VM_EXCEPTION(ClassCastException, "CHECKCAST");
	}
	pc += 3;
      }
      break;
    case Instr::D2F:
      {
	JavaDebug::Print("D2F");
	DECLARE_DOUBLE(value);
	frame->Push(Float::New(static_cast<float>(value))->ToWord());
	pc += 1;
      }
      break;
    case Instr::D2I:
      {
	JavaDebug::Print("D2I");
	DECLARE_DOUBLE(value);
	//--** to be checked: NaN etc.
	frame->Push(JavaInt::ToWord(static_cast<s_int32>(value)));
	pc += 1;
      }
      break;
    case Instr::D2L:
      {
	JavaDebug::Print("D2L");
	DECLARE_DOUBLE(value);
	//--** to be checked: NaN etc.
	PUSH_LONG(static_cast<s_int64>(value));
	pc += 1;
      }
      break;
    case Instr::DADD:
      {
	JavaDebug::Print("DADD");
	DECLARE_DOUBLE(v2);
	DECLARE_DOUBLE(v1);
	PUSH_DOUBLE(v1 + v2);
	pc += 1;
      }
      break;
    case Instr::DCMPG:
      {
	JavaDebug::Print("DCMPG");
	DECLARE_DOUBLE(v2);
	DECLARE_DOUBLE(v1);
	frame->Push(JavaInt::ToWord(v1 > v2? 1: v1 == v2? 0: v1 < v2? -1: 1));
	pc += 1;
      }
      break;
    case Instr::DCMPL:
      {
	JavaDebug::Print("DCMPL");
	DECLARE_DOUBLE(v2);
	DECLARE_DOUBLE(v1);
	frame->Push(JavaInt::ToWord(v1 > v2? 1: v1 == v2? 0: v1 < v2? -1: -1));
	pc += 1;
      }
      break;
    case Instr::DCONST_0:
      {
	JavaDebug::Print("DCONST_0");
	PUSH_DOUBLE(0.0L);
	pc += 1;
      }
      break;
    case Instr::DCONST_1:
      {
	JavaDebug::Print("DCONST_1");
	PUSH_DOUBLE(1.0L);
	pc += 1;
      }
      break;
    case Instr::DDIV:
      {
	JavaDebug::Print("DDIV");
	DECLARE_DOUBLE(v2);
	DECLARE_DOUBLE(v1);
	if (v2 == 0.0L) {
	  PUSH_DOUBLE(0.0L); // to be done: signed extended zero
	}
	else {
	  PUSH_DOUBLE(v1 / v2);
	}
	pc += 1;
      }
      break;
    case Instr::DMUL:
      {
	JavaDebug::Print("DMUL");
	DECLARE_DOUBLE(v2);
	DECLARE_DOUBLE(v1);
	PUSH_DOUBLE(v1 * v2);
	pc += 1;
      }
      break;
    case Instr::DNEG:
      {
	JavaDebug::Print("DNEG");
	DECLARE_DOUBLE(v1);
	PUSH_DOUBLE(-v1);
	pc += 1;
      }
      break;
    case Instr::DREM:
      {
	JavaDebug::Print("DREM");
	DECLARE_DOUBLE(v2);
	DECLARE_DOUBLE(v1);
	if (v2 != 0.0) {
	  double d = v1 / v2;
	  PUSH_DOUBLE(v1 - v2 * floor(d));
	}
	else {
	  PUSH_DOUBLE(0.0); // to be done: handle zero case
	}
	pc += 1;
      }
      break;
    case Instr::DSUB:
      {
	JavaDebug::Print("DSUB");
	DECLARE_DOUBLE(v2);
	DECLARE_DOUBLE(v1);
	PUSH_DOUBLE(v1 - v2);
	pc += 1;
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
	frame->Push(v1);
	frame->Push(v3);
	frame->Push(v2);
	frame->Push(v1);
	pc += 1;
      }
      break;
    case Instr::DUP2:
      {
	JavaDebug::Print("DUP2");
	// Always match form 1
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
	JavaDebug::Print("DUP2_X2");
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
	JavaDebug::Print("F2D");
	Float *f = JavaFloat::FromWord(frame->Pop());
	PUSH_DOUBLE(static_cast<double>(f->GetValue()));
	pc += 1;
      }
      break;
    case Instr::F2I:
      {
	JavaDebug::Print("F2I");
	//--** to be done: NaN etc.
	Float *f = JavaFloat::FromWord(frame->Pop());
	frame->Push(JavaInt::ToWord(static_cast<s_int32>(f->GetValue())));
	pc += 1;
      }
      break;
    case Instr::F2L:
      {
	JavaDebug::Print("F2L");
	//--** to be done: NaN etc.
	Float *f = JavaFloat::FromWord(frame->Pop());
	PUSH_LONG(static_cast<s_int64>(f->GetValue()));
	pc += 1;
      }
      break;
    case Instr::FADD:
      {
	JavaDebug::Print("FADD");
	Float *f2 = JavaFloat::FromWord(frame->Pop());
	Float *f1 = JavaFloat::FromWord(frame->Pop());
	frame->Push(Float::New(f1->GetValue() + f2->GetValue())->ToWord());
	pc += 1;
      }
      break;
    case Instr::FCMPG:
      {
	JavaDebug::Print("FCMPG");
	Float *f2 = JavaFloat::FromWord(frame->Pop());
	Float *f1 = JavaFloat::FromWord(frame->Pop());
	float v2 = f2->GetValue();
	float v1 = f1->GetValue();
	frame->Push(JavaInt::ToWord(v1 > v2? 1: v1 == v2? 0: v1 < v2? -1: 1));
	pc += 1;
      }
      break;
    case Instr::FCMPL:
      {
	JavaDebug::Print("FCMPL");
	Float *f2 = JavaFloat::FromWord(frame->Pop());
	Float *f1 = JavaFloat::FromWord(frame->Pop());
	float v2 = f2->GetValue();
	float v1 = f1->GetValue();
	frame->Push(JavaInt::ToWord(v1 > v2? 1: v1 == v2? 0: v1 < v2? -1: -1));
	pc += 1;
      }
      break;
    case Instr::FCONST_0:
      {
	JavaDebug::Print("FCONST_0");
	frame->Push(Float::New(0.0)->ToWord());
	pc += 1;
      }
      break;
    case Instr::FCONST_1:
      {
	JavaDebug::Print("FCONST_1");
	frame->Push(Float::New(1.0)->ToWord());
	pc += 1;
      }
      break;
    case Instr::FCONST_2:
      {
	JavaDebug::Print("FCONST_2");
	frame->Push(Float::New(2.0)->ToWord());
	pc += 1;
      }
      break;
    case Instr::FDIV:
      {
	JavaDebug::Print("FDIV");
	Float *f2 = JavaFloat::FromWord(frame->Pop());
	Float *f1 = JavaFloat::FromWord(frame->Pop());
	if (f2->GetValue() != 0.0) {
	  frame->Push(Float::New(f1->GetValue() / f2->GetValue())->ToWord());
	}
	else {
	  RAISE_VM_EXCEPTION(ArithmeticException, "FDIV");
	}
	pc += 1;
      }
      break;
    case Instr::FMUL:
      {
	JavaDebug::Print("FMUL");
	Float *f2 = JavaFloat::FromWord(frame->Pop());
	Float *f1 = JavaFloat::FromWord(frame->Pop());
	frame->Push(Float::New(f1->GetValue() * f2->GetValue())->ToWord());
	pc += 1;
      }
      break;
    case Instr::FNEG:
      {
	JavaDebug::Print("FNEG");
	Float *f = JavaFloat::FromWord(frame->Pop());
	frame->Push(Float::New(-f->GetValue())->ToWord());
	pc += 1;
      }
      break;
    case Instr::FREM:
      {
	JavaDebug::Print("FREM");
	Float *f2 = JavaFloat::FromWord(frame->Pop());
	Float *f1 = JavaFloat::FromWord(frame->Pop());
	float v2 = f2->GetValue();
	float v1 = f1->GetValue();
	if (v1 != 0.0) {
	  float d = v1 / v2;
	  frame->Push(Float::New(v1 - v2 * floor(d))->ToWord());
	}
	else {
	  frame->Push(Float::New(0.0)->ToWord());// to be done: handle zero case
	}
	pc += 1;
      }
      break;
    case Instr::FSUB:
      {
	JavaDebug::Print("FSUB");
	Float *f2 = JavaFloat::FromWord(frame->Pop());
	Float *f1 = JavaFloat::FromWord(frame->Pop());
	frame->Push(Float::New(f1->GetValue() - f2->GetValue())->ToWord());
	pc += 1;
      }
      break;
    case Instr::GETFIELD:
      {
	JavaDebug::Print("GETFIELD");
	word wObject = frame->Pop();
	if (wObject != null) {
	  word wFieldRef             = GET_POOL_VALUE(GET_POOL_INDEX());
	  InstanceFieldRef *fieldRef = InstanceFieldRef::FromWord(wFieldRef);
	  if (fieldRef == INVALID_POINTER) {
	    frame->Push(wObject);
	    REQUEST(wFieldRef);
	  }
	  Object *object = Object::FromWord(wObject);
	  Assert(object != INVALID_POINTER);
	  word value = object->GetInstanceField(fieldRef->GetIndex()); 
	  frame->Push(value);
	  if (fieldRef->GetNumberOfRequiredSlots() == 2) {
	    FILL_SLOT();
	  } else {
	    Assert(fieldRef->GetNumberOfRequiredSlots() == 1);
	  }
	}
	else {
	  RAISE_VM_EXCEPTION(NullPointerException, "GETFIELD");
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
	Class *aClass = fieldRef->GetClass();
	Assert(aClass != INVALID_POINTER);
	Lock *lock = aClass->GetLock();
	Future *future = lock->Acquire();
	if (future != INVALID_POINTER)
	  REQUEST(future->ToWord());
	if (!aClass->IsInitialized()) {
	  frame->SetPC(pc);
	  return aClass->RunInitializer();
	}
	word value = aClass->GetStaticField(fieldRef->GetIndex());
	lock->Release();
	frame->Push(value);
	if (fieldRef->GetNumberOfRequiredSlots() == 2) {
	  FILL_SLOT();
	} else {
	  Assert(fieldRef->GetNumberOfRequiredSlots() == 1);
	}
	pc += 3;
      }
      break;
    case Instr::GOTO:
      {
	JavaDebug::Print("GOTO");
	pc += static_cast<s_int16>(GET_POOL_INDEX());
      }
      break;
    case Instr::GOTO_W:
      {
	JavaDebug::Print("GOTO_W");
	pc += static_cast<s_int32>(GET_WIDE_INDEX());
      }
      break;
    case Instr::I2B:
      {
	JavaDebug::Print("I2B");
	s_int32 i = JavaInt::FromWord(frame->Pop());
	frame->Push(JavaInt::ToWord(static_cast<s_int8>(i)));
	pc += 1;
      }
      break;
    case Instr::I2C:
      {
	  JavaDebug::Print("I2C");
	s_int32 i = JavaInt::FromWord(frame->Pop());
	frame->Push(Store::IntToWord(static_cast<s_int16>(i)));
	pc += 1;
      }
      break;
    case Instr::I2D:
      {
	JavaDebug::Print("I2D");
	s_int32 i = JavaInt::FromWord(frame->Pop());
	PUSH_DOUBLE(static_cast<double>(i));
	pc += 1;
      }
      break;
    case Instr::I2F:
      {
	JavaDebug::Print("I2F");
	s_int32 i = JavaInt::FromWord(frame->Pop());
	frame->Push(Float::New(static_cast<float>(i))->ToWord());
	pc += 1;
      }
      break;
    case Instr::I2L:
      {
	JavaDebug::Print("I2L");
	s_int32 i = JavaInt::FromWord(frame->Pop());
	PUSH_LONG(i);
	pc += 1;
      }
      break;
    case Instr::I2S:
      {
	  JavaDebug::Print("I2S");
	  s_int32 i = JavaInt::FromWord(frame->Pop());
	  frame->Push(JavaInt::ToWord(static_cast<s_int16>(i)));
	  pc += 1;
      }
      break;
    case Instr::IADD:
      {
	JavaDebug::Print("IADD");
	s_int v2 = JavaInt::FromWord(frame->Pop());
	s_int v1 = JavaInt::FromWord(frame->Pop());
	frame->Push(JavaInt::ToWord(v1 + v2));
	pc += 1;
      }
      break;
    case Instr::IAND:
      {
	JavaDebug::Print("IAND");
	s_int v2 = JavaInt::FromWord(frame->Pop());
	s_int v1 = JavaInt::FromWord(frame->Pop());
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
	s_int v2 = JavaInt::FromWord(frame->Pop());
	s_int v1 = JavaInt::FromWord(frame->Pop());
	if (v2 != 0)
	  frame->Push(JavaInt::ToWord(v1 / v2));
	else {
	  RAISE_VM_EXCEPTION(ArithmeticException, "IDIV");
	}
	pc += 1;
      }
      break;
    case Instr::IF_ACMPEQ:
      {
	JavaDebug::Print("IF_ACMPEQ");
	word v2 = Word::Deref(frame->Pop());
	word v1 = Word::Deref(frame->Pop());
	if (v1 == v2)
	  pc += static_cast<s_int16>(GET_POOL_INDEX());
	else
	  pc += 3;
      }
      break;
    case Instr::IF_ACMPNE:
      {
	JavaDebug::Print("IF_ACMPNE");
	word v2 = Word::Deref(frame->Pop());
	word v1 = Word::Deref(frame->Pop());
	if (v1 != v2)
	  pc += static_cast<s_int16>(GET_POOL_INDEX());
	else
	  pc += 3;
      }
      break;
    case Instr::IF_ICMPEQ:
      {
	JavaDebug::Print("IF_ICMPEQ");
	s_int v2 = JavaInt::FromWord(frame->Pop());
	s_int v1 = JavaInt::FromWord(frame->Pop());
	if (v1 == v2)
	  pc += static_cast<s_int16>(GET_POOL_INDEX());
	else
	  pc += 3;
      }
      break;
    case Instr::IF_ICMPNE:
      {
	JavaDebug::Print("IF_ICMPNE");
	s_int v2 = JavaInt::FromWord(frame->Pop());
	s_int v1 = JavaInt::FromWord(frame->Pop());
	if (v1 != v2)
	  pc += static_cast<s_int16>(GET_POOL_INDEX());
	else
	  pc += 3;
      }
      break;
    case Instr::IF_ICMPLT:
      {
	JavaDebug::Print("IF_ICMPLT");
	s_int v2 = JavaInt::FromWord(frame->Pop());
	s_int v1 = JavaInt::FromWord(frame->Pop());
	if (v1 < v2)
	  pc += static_cast<s_int16>(GET_POOL_INDEX());
	else
	  pc += 3;
      }
      break;
    case Instr::IF_ICMPGE:
      {
	JavaDebug::Print("IF_ICMPGE");
	s_int v2 = JavaInt::FromWord(frame->Pop());
	s_int v1 = JavaInt::FromWord(frame->Pop());
	if (v1 >= v2)
	  pc += static_cast<s_int16>(GET_POOL_INDEX());
	else
	  pc += 3;
      }
      break;
    case Instr::IF_ICMPGT:
      {
	JavaDebug::Print("IF_ICMPGT");
	s_int v2 = JavaInt::FromWord(frame->Pop());
	s_int v1 = JavaInt::FromWord(frame->Pop());
	if (v1 > v2)
	  pc += static_cast<s_int16>(GET_POOL_INDEX());
	else
	  pc += 3;
      }
      break;
    case Instr::IF_ICMPLE:
      {
	JavaDebug::Print("IF_ICMPLE");
	s_int v2 = JavaInt::FromWord(frame->Pop());
	s_int v1 = JavaInt::FromWord(frame->Pop());
	if (v1 <= v2)
	  pc += static_cast<s_int16>(GET_POOL_INDEX());
	else
	  pc += 3;
      }
      break;
    case Instr::IFEQ:
      {
	JavaDebug::Print("IFEQ");
	if (JavaInt::FromWord(frame->Pop()) == 0)
	  pc += static_cast<s_int16>(GET_POOL_INDEX());
	else
	  pc += 3;
      }
      break;
    case Instr::IFNE:
      {
	JavaDebug::Print("IFNE");
	if (JavaInt::FromWord(frame->Pop()) != 0)
	  pc += static_cast<s_int16>(GET_POOL_INDEX());
	else
	  pc += 3;
      }
      break;
    case Instr::IFLT:
      {
	JavaDebug::Print("IFLT");
	if (JavaInt::FromWord(frame->Pop()) < 0)
	  pc += static_cast<s_int16>(GET_POOL_INDEX());
	else
	  pc += 3;
      }
      break;
    case Instr::IFGE:
      {
	JavaDebug::Print("IFGE");
	if (JavaInt::FromWord(frame->Pop()) >= 0)
	  pc += static_cast<s_int16>(GET_POOL_INDEX());
	else
	  pc += 3;
      }
      break;
    case Instr::IFGT:
      {
	JavaDebug::Print("IFGT");
	if (JavaInt::FromWord(frame->Pop()) > 0)
	  pc += static_cast<s_int16>(GET_POOL_INDEX());
	else
	  pc += 3;
      }
      break;
    case Instr::IFLE:
      {
	JavaDebug::Print("IFLE");
	if (JavaInt::FromWord(frame->Pop()) <= 0)
	  pc += static_cast<s_int16>(GET_POOL_INDEX());
	else
	  pc += 3;
      }
      break;
    case Instr::IFNONNULL:
      {
	JavaDebug::Print("IFNONNULL");
	if (frame->Pop() != null)
	  pc += static_cast<s_int16>(GET_POOL_INDEX());
	else
	  pc += 3;
      }
      break;
    case Instr::IFNULL:
      {
	JavaDebug::Print("IFNULL");
	if (frame->Pop() == null)
	  pc += static_cast<s_int16>(GET_POOL_INDEX());
	else
	  pc += 3;
      }
      break;
    case Instr::IINC:
      {
	JavaDebug::Print("IINC");
	u_int index = GET_BYTE_INDEX();
	s_int v     = JavaInt::FromWord(frame->GetEnv(index));
	v += static_cast<s_int8>(code[pc + 2]);
	frame->SetEnv(index, JavaInt::ToWord(v));
	pc += 3;
      }
      break;
    case Instr::IMUL:
      {
	JavaDebug::Print("IMUL");
	s_int v2 = JavaInt::FromWord(frame->Pop());
	s_int v1 = JavaInt::FromWord(frame->Pop());
	frame->Push(JavaInt::ToWord(v1 * v2));
	pc += 1;
      }
      break;
    case Instr::INEG:
      {
	JavaDebug::Print("INEG");
	s_int v = JavaInt::FromWord(frame->Pop());
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
	frame->Push(JavaInt::ToWord(IsInstanceOf(wObject, type)));
	pc += 3;
      }
      break;
    case Instr::INVOKEINTERFACE:
      {
	JavaDebug::Print("INVOKEINTERFACE");
	word wMethodRef = GET_POOL_VALUE(GET_POOL_INDEX());
	InterfaceMethodRef *methodRef =
	  InterfaceMethodRef::FromWord(wMethodRef);
	if (methodRef == INVALID_POINTER)
	  REQUEST(wMethodRef);
	// Set continuation
	frame->SetPC(-2);
	frame->SetContPC(pc + 5);
	u_int nArgs = methodRef->GetNumberOfArguments();
	// to be done: support more arguments
	Assert(nArgs + 1 < Scheduler::maxArgs);
	// self becomes local0
	if (nArgs == 0)
	  Scheduler::nArgs = Scheduler::ONE_ARG;
	else
	  Scheduler::nArgs = nArgs + 1;
	for (u_int i = nArgs + 1; i--;)
	  Scheduler::currentArgs[i] = frame->Pop();
	Object *object = Object::FromWord(Scheduler::currentArgs[0]);
	if (object == INVALID_POINTER) {
	  RAISE_VM_EXCEPTION(NullPointerException, "INVOKEINTERFACE");
	}
	Closure *closure = object->GetClass()->
	  GetInterfaceMethod(methodRef->GetClass(), methodRef->GetIndex());
	if (closure == INVALID_POINTER) {
	  RAISE_VM_EXCEPTION(IncompatibleClassChangeError, "INVOKEINTERFACE");
	}
	return Scheduler::PushCall(closure->ToWord());
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
	u_int nArgs = methodRef->GetNumberOfArguments();
	// to be done: support more arguments
	Assert(nArgs + 1 < Scheduler::maxArgs);
	// self becomes local0
	if (nArgs == 0)
	  Scheduler::nArgs = Scheduler::ONE_ARG;
	else
	  Scheduler::nArgs = nArgs + 1;
	for (u_int i = nArgs + 1; i--;)
	  Scheduler::currentArgs[i] = frame->Pop();
	Object *object = Object::FromWord(Scheduler::currentArgs[0]);
	if (object == INVALID_POINTER) {
	  RAISE_VM_EXCEPTION(NullPointerException, "INVOKESPECIAL");
	}
	Class *aClass  = methodRef->GetClass();
	Closure *closure = aClass->GetVirtualMethod(methodRef->GetIndex());
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
	Class *aClass = methodRef->GetClass();
	Assert(aClass != INVALID_POINTER);
	Lock *lock = aClass->GetLock();
	Future *future = lock->Acquire();
	if (future != INVALID_POINTER)
	  REQUEST(future->ToWord());
	if (!aClass->IsInitialized()) {
	  frame->SetPC(pc);
	  return aClass->RunInitializer();
	}
	// Set continuation
	frame->SetPC(-2);
	frame->SetContPC(pc + 3);
	// to be done: support more arguments
	u_int nArgs = methodRef->GetNumberOfArguments();
	Assert(nArgs < Scheduler::maxArgs);
	if (nArgs == 1)
	  Scheduler::nArgs = Scheduler::ONE_ARG;
	else
	  Scheduler::nArgs = nArgs;
	for (u_int i = nArgs; i--;)
	  Scheduler::currentArgs[i] = frame->Pop();
	Closure *closure = aClass->GetStaticMethod(methodRef->GetIndex());
	UnlockWorker::PushFrame(lock);
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
	Assert(nArgs + 1 < Scheduler::maxArgs);
	// self becomes local0
	if (nArgs == 0)
	  Scheduler::nArgs = Scheduler::ONE_ARG;
	else
	  Scheduler::nArgs = nArgs + 1;
	for (u_int i = nArgs + 1; i--;)
	  Scheduler::currentArgs[i] = frame->Pop();
	Object *object = Object::FromWord(Scheduler::currentArgs[0]);
	if (object == INVALID_POINTER) {
	  RAISE_VM_EXCEPTION(NullPointerException, "INVOKEVIRTUAL");
	}
	Closure *closure = object->GetVirtualMethod(methodRef->GetIndex());
	return Scheduler::PushCall(closure->ToWord());
      }
      break;
    case Instr::IOR:
      {
	JavaDebug::Print("IOR");
	s_int v2 = JavaInt::FromWord(frame->Pop());
	s_int v1 = JavaInt::FromWord(frame->Pop());
	frame->Push(JavaInt::ToWord(v1 || v2));
	pc += 1;
      }
      break;
    case Instr::IREM:
      {
	JavaDebug::Print("IREM");
	s_int v2 = JavaInt::FromWord(frame->Pop());
	s_int v1 = JavaInt::FromWord(frame->Pop());
	if (v2 != 0)
	  frame->Push(JavaInt::ToWord(v1 % v2));
	else {
	  RAISE_VM_EXCEPTION(ArithmeticException, "IREM");
	}
	pc += 1;
      }
      break;
    case Instr::ISHL:
      {
	JavaDebug::Print("ISHL");
	u_int v2 = JavaInt::FromWord(frame->Pop());
	u_int v1 = JavaInt::FromWord(frame->Pop());
	frame->Push(JavaInt::ToWord(v1 << (v2 % 32)));
	pc += 1;
      }
      break;
    case Instr::ISHR:
      {
	JavaDebug::Print("ISHR");
	s_int v2 = JavaInt::FromWord(frame->Pop());
	u_int v1 = JavaInt::FromWord(frame->Pop());
	frame->Push(JavaInt::ToWord(v1 / (1 << (v2 % 32))));
	pc += 1;
      }
      break;
    case Instr::ISUB:
      {
	JavaDebug::Print("ISUB");
	s_int v2 = JavaInt::FromWord(frame->Pop());
	s_int v1 = JavaInt::FromWord(frame->Pop());
	frame->Push(JavaInt::ToWord(v1 - v2));
	pc += 1;
      }
      break;
    case Instr::IUSHR:
      {
	JavaDebug::Print("IUSHR");
	u_int v2 = JavaInt::FromWord(frame->Pop());
	u_int v1 = JavaInt::FromWord(frame->Pop());
	frame->Push(JavaInt::ToWord(v1 / (1 << (v2 % 32))));
	pc += 1;
      }
      break;
    case Instr::IXOR:
      {
	JavaDebug::Print("IXOR");
	u_int v2 = JavaInt::FromWord(frame->Pop());
	u_int v1 = JavaInt::FromWord(frame->Pop());
	frame->Push(JavaInt::ToWord(v1 ^ v2));
	pc += 1;
      }
      break;
    case Instr::JSR:
      {
	JavaDebug::Print("JSR");
	frame->Push(ReturnAddress::ToWord(pc + 3));
	pc += static_cast<s_int16>(GET_POOL_INDEX());
      }
      break;
    case Instr::JSR_W:
      {
	JavaDebug::Print("JSR_W");
	frame->Push(ReturnAddress::ToWord(pc + 5));
	pc += static_cast<s_int>(GET_WIDE_INDEX());
      }
      break;
    case Instr::L2D:
      {
	  JavaDebug::Print("L2D");
	  DECLARE_LONG(v);
	  PUSH_DOUBLE(static_cast<double>(v));
	  pc += 1;
      }
      break;
    case Instr::L2F:
      {
	JavaDebug::Print("L2D");
	DECLARE_LONG(v);
	frame->Push(Float::New(static_cast<float>(v))->ToWord());
	pc += 1;
      }
      break;
      case Instr::L2I:
	  {
	  JavaDebug::Print("L2I");
	DECLARE_LONG(v);
	frame->Push(JavaInt::ToWord(v));
	pc += 1;
      }
      break;
    case Instr::LADD:
      {
	JavaDebug::Print("LADD");
	DECLARE_LONG(v2);
	DECLARE_LONG(v1);
	PUSH_LONG(v1 + v2);
	pc += 1;
      }
      break;
    case Instr::LAND:
      {
	JavaDebug::Print("LAND");
	DECLARE_LONG(v2);
	DECLARE_LONG(v1);
	PUSH_LONG(v1 & v2);
	pc += 1;
      }
      break;
    case Instr::LCMP:
      {
	JavaDebug::Print("LCMP");
	DECLARE_LONG(v2);
	DECLARE_LONG(v1);
	frame->Push(JavaInt::ToWord(v1 == v2? 0: v1 < v2? -1: 1));
	pc += 1;
      }
      break;
    case Instr::LCONST_0:
      {
	JavaDebug::Print("LCONST_0");
	frame->Push(JavaLong::New(0, 0)->ToWord());
	FILL_SLOT();
	pc += 1;
      }
      break;
    case Instr::LCONST_1:
      {
	JavaDebug::Print("LCONST_1");
	frame->Push(JavaLong::New(0, 1)->ToWord());
	FILL_SLOT();
	pc += 1;
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
      {
	JavaDebug::Print("LDC_W");
	word value = GET_POOL_VALUE(GET_POOL_INDEX());
	frame->Push(value);
	pc += 3;
      }
      break;
    case Instr::LDC2_W:
      {
	JavaDebug::Print("LDC2_W");
	word value = GET_POOL_VALUE(GET_POOL_INDEX());
	frame->Push(value);
	FILL_SLOT();
	pc += 3;
      }
      break;
    case Instr::LDIV:
      {
	JavaDebug::Print("LDIV");
	DECLARE_LONG(v2);
	DECLARE_LONG(v1);
	if (v1 != 0.0) {
	  PUSH_LONG(v1 / v2);
	}
	else {
	  RAISE_VM_EXCEPTION(ArithmeticException, "LDIV");
	}
	pc += 1;
      }
      break;
    case Instr::LMUL:
      {
	JavaDebug::Print("LMUL");
	DECLARE_LONG(v2);
	DECLARE_LONG(v1);
	PUSH_LONG(v1 * v2);
	pc += 1;
      }
      break;
    case Instr::LNEG:
      {
	JavaDebug::Print("LNEG");
	DECLARE_LONG(v);
	PUSH_LONG(0 - v);
	pc += 1;
      }
      break;
    case Instr::LOOKUPSWITCH:
      {
	s_int basePC = pc;
	pc = pc + 4 - (pc & 3);
	s_int defaultOffset = GET_WIDE_INDEX();
	pc += 4;
	s_int nPairs = GET_WIDE_INDEX();
	s_int key = JavaInt::FromWord(frame->Pop());
	for (u_int i = nPairs; i--;) {
	  s_int match = GET_WIDE_INDEX();
	  pc += 4;
	  s_int offset = GET_WIDE_INDEX(); 
	  pc += 4;
	  if (key == match)
	    pc = basePC + offset;
	}
	pc = basePC + defaultOffset;
      }
      break;
    case Instr::LOR:
      {
	JavaDebug::Print("LOR");
	DECLARE_LONG(v2);
	DECLARE_LONG(v1);
	PUSH_LONG(v1 | v2);
	pc += 1;
      }
      break;
    case Instr::LREM:
      {
	JavaDebug::Print("LREM");
	DECLARE_LONG(v2);
	DECLARE_LONG(v1);
	if (v2 != 0) {
	  PUSH_LONG(v1 % v2);
	}
	else {
	  PUSH_LONG(static_cast<s_int64>(0)); // to be done: handle zero case
	}
	pc += 1;
      }
      break;
    case Instr::LSHL:
      {
	JavaDebug::Print("LSHL");
	u_int v2 = JavaInt::FromWord(frame->Pop());
	DECLARE_LONG(v1);
	PUSH_LONG(v1 << (v2 % 64));
	pc += 1;
      }
      break;
    case Instr::LSHR:
      {
	JavaDebug::Print("LSHR");
	u_int v2 = JavaInt::FromWord(frame->Pop());
	DECLARE_LONG(v1);
	PUSH_LONG(v1 / (static_cast<u_int64>(1) << (v2 % 64)));
	pc += 1;
      }
      break;
    case Instr::LSUB:
      {
	JavaDebug::Print("LSUB");
	DECLARE_LONG(v2);
	DECLARE_LONG(v1);
	PUSH_LONG(v1 - v2);
	pc += 1;
      }
      break;
    case Instr::LUSHR:
      {
	JavaDebug::Print("LUSHR");
	u_int v2 = JavaInt::FromWord(frame->Pop());
	DECLARE_LONG(v1);
	u_int64 u_v1 = v1;
	PUSH_LONG(u_v1 / (static_cast<u_int64>(1) << (v2 % 64)));
	pc += 1;
      }
      break;
    case Instr::LXOR:
      {
	JavaDebug::Print("LXOR");
	DECLARE_LONG(v2);
	DECLARE_LONG(v1);
	PUSH_LONG(v1 ^ v2);
	pc += 1;
      }
      break;
    case Instr::MONITORENTER:
      {
	Object *object = Object::FromWord(frame->Pop());
	if (object != INVALID_POINTER) {
	  Lock *lock     = object->GetLock();
	  Future *future = lock->Acquire();
	  if (future != INVALID_POINTER) {
	    frame->Push(object->ToWord());
	    REQUEST(future->ToWord());
	  }
	}
	else {
	  RAISE_VM_EXCEPTION(NullPointerException, "MONITORENTER");
	}
	pc += 1;
      }
      break;
    case Instr::MONITOREXIT:
      {
	Object *object = Object::FromWord(frame->Pop());
	if (object != INVALID_POINTER) {
	  Lock *lock = object->GetLock();
	  // This is safe for verified code; otherwise
	  // IllegalMonitorStateException might be raised
	  lock->Release();
	}
	else {
	  RAISE_VM_EXCEPTION(NullPointerException, "MONITOREXIT");
	}
	pc += 1;
      }
      break;
    case Instr::MULTIANEWARRAY:
      {
	//--** to be done
	JavaDebug::Print("MULTIANEWARRAY");
	Error("not implemented");
	/*
	word wType = GET_POOL_VALUE(GET_POOL_INDEX());
	Type *type = Type::FromWord(wType);
	if (type == INVALID_POINTER)
	  REQUEST(wType);
	u_int nDims = code[pc + 3];
	for (u_int i = nDims; i--;) {
	  s_int count = JavaInt::FromWord(frame->Pop());
	  if (count < 0) {
	    RAISE_VM_EXCEPTION(NegativeArraySizeException, "multianewarray");
	  }
	  ObjectArray *arr = ObjectArray::New(type, count);
	  type = static_cast<Type *>(ArrayType::New(type));
	}
	frame->Push(type->ToWord());
	pc += 4;
	*/
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
	    Class *aClass = static_cast<Class *>(type);
	    Lock *lock = aClass->GetLock();
	    Future *future = lock->Acquire();
	    if (future != INVALID_POINTER)
	      REQUEST(future->ToWord());
	    if (!aClass->IsInitialized()) {
	      frame->SetPC(pc);
	      return aClass->RunInitializer();
	    }
	    Object *object = Object::New(aClass);
	    Assert(object != INVALID_POINTER);
	    frame->Push(object->ToWord());
	    lock->Release();
	  }
	  break;
	case JavaLabel::PrimitiveType:
	case JavaLabel::ArrayType:
	  {
	    RAISE_VM_EXCEPTION(InstantiationError, "new");
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
	s_int count = JavaInt::FromWord(frame->Pop());
	if (count >= 0) {
	  BaseArray *baseArray;
	  switch (code[pc + 1]) {
	  case 4:
	    baseArray = BaseArray::New(PrimitiveType::Boolean, count);
	    break;
	  case 5:
	    baseArray = BaseArray::New(PrimitiveType::Char, count);
	    break;
	  case 6:
	    baseArray = BaseArray::New(PrimitiveType::Float, count);
	    break;
	  case 7:
	    baseArray = BaseArray::New(PrimitiveType::Double, count);
	    break;
	  case 8:
	    baseArray = BaseArray::New(PrimitiveType::Byte, count);
	    break;
	  case 9:
	    baseArray = BaseArray::New(PrimitiveType::Short, count);
	    break;
	  case 10:
	    baseArray = BaseArray::New(PrimitiveType::Int, count);
	    break;
	  case 11:
	    baseArray = BaseArray::New(PrimitiveType::Long, count);
	    break;
	  default:
	    Error("invalid base type");
	  }
	  frame->Push(baseArray->ToWord());
	}
	else {
	  RAISE_VM_EXCEPTION(NegativeArraySizeException, "newarray");
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
	if (fieldRef->GetNumberOfRequiredSlots() == 2) {
	  DROP_SLOT();
	} else {
	  Assert(fieldRef->GetNumberOfRequiredSlots() == 1);
	}
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
	Class *aClass = fieldRef->GetClass();
	Assert(aClass != INVALID_POINTER);
	Lock *lock = aClass->GetLock();
	Future *future = lock->Acquire();
	if (future != INVALID_POINTER)
	  REQUEST(future->ToWord());
	if (!aClass->IsInitialized()) {
	  frame->SetPC(pc);
	  return aClass->RunInitializer();
	}
	if (fieldRef->GetNumberOfRequiredSlots() == 2) {
	  DROP_SLOT();
	} else {
	  Assert(fieldRef->GetNumberOfRequiredSlots() == 1);
	}
	aClass->PutStaticField(fieldRef->GetIndex(), frame->Pop());
	lock->Release();
	pc += 3;
      }
      break;
    case Instr::RET:
      {
	JavaDebug::Print("RET");
	u_int index = GET_BYTE_INDEX();
	pc = ReturnAddress::FromWord(frame->GetEnv(index));
      }
      break;
    case Instr::RETURN:
      {
	JavaDebug::Print("RETURN");
	Scheduler::nArgs = 0;
	Scheduler::PopFrame(frame->GetSize());
	CHECK_PREEMPT();
      }
      break;
    case Instr::SIPUSH:
      {
	JavaDebug::Print("SIPUSH");
	s_int32 value = static_cast<s_int16>(GET_POOL_INDEX());
	frame->Push(JavaInt::ToWord(value));
	pc += 3;
      }
      break;
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
	s_int basePC = pc;
	pc = pc + 4 - (pc & 3);
	s_int defaultOffset = GET_WIDE_INDEX();
	pc += 4;
	s_int lowValue = GET_WIDE_INDEX();
	pc += 4;
	s_int highValue = GET_WIDE_INDEX();
	s_int value = JavaInt::FromWord(frame->Pop());
	if ((value >= lowValue) && (value <= highValue)) {
	  value -= lowValue;
	  pc += value * 4;
	  pc = basePC + static_cast<s_int>(GET_WIDE_INDEX());
	}
	else
	  pc = basePC + defaultOffset; 
      }
      break;
    case Instr::WIDE:
      {
	JavaDebug::Print("WIDE");
	switch (static_cast<Instr::Opcode>(code[++pc])) {
	case Instr::ILOAD:
	case Instr::FLOAD:
	case Instr::ALOAD:
	  {
	    frame->Push(frame->GetEnv(GET_POOL_INDEX()));
	    pc += 2;
	  }
	  break;
	case Instr::LLOAD:
	case Instr::DLOAD:
	  {
	    frame->Push(frame->GetEnv(GET_POOL_INDEX()));
	    FILL_SLOT();
	    pc += 2;
	  }
	  break;
	case Instr::ISTORE:
	case Instr::FSTORE:
	case Instr::ASTORE:
	  {
	    frame->SetEnv(GET_POOL_INDEX(), frame->Pop());
	    pc += 2;
	  }
	case Instr::LSTORE:
	case Instr::DSTORE:
	  break;
	  {
	    DROP_SLOT();
	    frame->SetEnv(GET_POOL_INDEX(), frame->Pop());
	    pc += 2;
	  }
	  break;
	case Instr::IINC:
	  {
	    u_int index = GET_POOL_INDEX();
	    s_int16 inc = ((code[pc + 3] << 8) | code[pc + 4]);
	    s_int value = JavaInt::FromWord(frame->GetEnv(index));
	    value += inc;
	    frame->SetEnv(index, JavaInt::ToWord(value));
	    pc += 4;
	  }
	  break;
	case Instr::RET:
	  {
	    u_int index = GET_POOL_INDEX();
	    pc = ReturnAddress::FromWord(frame->GetEnv(index));
	  }
	  break;
	default:
	  Error("invalid `wide' opcode");
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

Interpreter::Result ByteCodeInterpreter::Handle(word) {
  StackFrame *sFrame = Scheduler::GetFrame();
  Assert(sFrame->GetWorker() == this);
  ByteCodeFrame *frame = static_cast<ByteCodeFrame *>(sFrame);
  s_int pc             = frame->GetPC();
  Table *table         = frame->GetExceptionTable();
  u_int count          = table->GetCount();
  Object *object       = Object::FromWord(Scheduler::currentData);
  Assert(object != INVALID_POINTER);
  for (u_int i = 0; i < count; i++) {
    ExceptionTableEntry *entry =
      ExceptionTableEntry::FromWordDirect(table->Get(i));
    s_int startPC = entry->GetStartPC();
    s_int endPC   = entry->GetEndPC();
    // Exception handler is within range
    if ((startPC <= pc) && (pc < endPC)) {
      // Check exception type
      word wType = entry->GetCatchType();
      if (wType == null) {
	frame->SetPC(entry->GetHandlerPC());
	return Worker::CONTINUE;
      }
      Type *type = Type::FromWord(wType);
      Assert(type != INVALID_POINTER); //--** to be done: should request
      if (IsInstanceOf(object->ToWord(), type)) {
	frame->SetPC(entry->GetHandlerPC());
	return Worker::CONTINUE;
      }
    }
  }
  word wFrame = frame->Clone();
  Scheduler::PopFrame(frame->GetSize());
  Scheduler::currentBacktrace->Enqueue(wFrame);
  return Worker::RAISE;
}

u_int ByteCodeInterpreter::GetInArity(ConcreteCode *concreteCode) {
  Assert(concreteCode->GetInterpreter() == ByteCodeInterpreter::self);
  JavaByteCode *byteCode = static_cast<JavaByteCode *>(concreteCode);
  u_int arity = byteCode->GetMethodInfo()->GetNumberOfArguments();
  return arity == 1? Scheduler::ONE_ARG: arity;
}

const char *ByteCodeInterpreter::Identify() {
  return "ByteCodeInterpreter";
}

void ByteCodeInterpreter::DumpFrame(StackFrame *sFrame) {
  ByteCodeFrame *frame = static_cast<ByteCodeFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  MethodInfo *info     = frame->GetMethodInfo();
  std::fprintf(stderr, "%s#%s%s\n", info->GetClassName()->ExportC(),
	       info->GetName()->ExportC(), info->GetDescriptor()->ExportC());
}

void ByteCodeInterpreter::FillStackTraceElement(word wFrame, 
						Object *stackTraceElement) {
  // to be done: Hack Alert
  u_int size = Store::DirectWordToBlock(wFrame)->GetSize();
  StackFrame *sFrame = Scheduler::PushFrame(size);
  StackFrame::New(sFrame, size, wFrame);
  ByteCodeFrame *frame = static_cast<ByteCodeFrame *>(sFrame);
  Assert(sFrame->GetWorker() == ByteCodeInterpreter::self);
  JavaString *className = frame->GetMethodInfo()->GetClassName();
  JavaString *name = frame->GetMethodInfo()->GetName();
  stackTraceElement->InitInstanceField
    (StackTraceElement::DECLARING_CLASS_INDEX, className->ToWord());
  stackTraceElement->InitInstanceField
    (StackTraceElement::METHOD_NAME_INDEX, name->ToWord());
  stackTraceElement->InitInstanceField
    (StackTraceElement::FILE_NAME_INDEX, null);
  stackTraceElement->InitInstanceField
    (StackTraceElement::LINE_NUMBER_INDEX, JavaInt::ToWord(-1));
}
