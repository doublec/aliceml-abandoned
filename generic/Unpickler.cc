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
#pragma implementation "emulator/Unpickler.hh"
#endif

#include <cstdio>
#include <cstdlib>
#include "adt/HashTable.hh"
#include "emulator/Interpreter.hh"
#include "emulator/Scheduler.hh"
#include "emulator/TaskStack.hh"
#include "emulator/Unpickler.hh"
#include "emulator/Closure.hh"
#include "emulator/Tuple.hh"
#include "emulator/Transients.hh"
#include "emulator/PrimitiveTable.hh"

// pickle    ::= int | chunk | block | tuple | closure | transform
// int       ::= POSINT <uint> | NEGINT <uint>
// chunk     ::= CHUNK size <byte>*size
// size      ::= <uint>
// block     ::= BLOCK label size field*size
// tuple     ::= TUPLE size field*size
// closure   ::= CLOSURE size field*size
// label     ::= <uint>
// field     ::= pickle | reference
// reference ::= REF id
// id        ::= <uint>
// transform ::= TRANSFORM (chunk|reference) field

// Very convienent Macro
#define CONTINUE(args)           \
  Scheduler::currentArgs = args; \
  return Interpreter::CONTINUE;

//
// Streaming Classes
//
class InputStreamBase {
protected:
  int hd, tl, rd, eob;
  u_char *buffer;
public:
  // InputStreamBase Constructor
  InputStreamBase() : hd(0), tl(0), rd(0), eob(0) {
    buffer = INVALID_POINTER;
  }
  // InputStreamBase Functions
  u_int IsEOB() {
    if (eob) {
      eob = 0;
      return 1;
    }
    else {
      return 0;
    }
  }
  u_char GetByte() {
    if (rd == tl) {
      eob = 1;
      return (u_char) 0;
    }
    else {
       return buffer[rd++];
    }
  }
  u_char *GetBytes(u_int n) {
    // This has to be revisited: TOO NAIVE
    u_char *byteBuffer = (u_char *) malloc(sizeof(u_char) * n);
    u_int i = 0;
    if (n > 0) {
      while (i < n) {
	byteBuffer[i++] = GetByte();
	if (eob) {
	  free(byteBuffer);
	  return INVALID_POINTER;
	}
      }
    }
    return byteBuffer;
  }
  u_int GetUInt() {
    return DoGetUInt(0, 1);
  }
  u_int DoGetUInt(u_int x, u_int n) {
    u_char b = GetByte();
    if (eob) {
      return 0;
    }
    else if (b >= 0x80) {
      return DoGetUInt((x + (b - 0x80) * n), (n * 0x80));
    }
    else {
      return (x + b * n);
    }
  }
  void Commit() {
    hd = rd;
  }
  void AppendToBuffer(u_char *src, int size) {
    // This has to be revisited: TOO NAIVE
    // Fresh Buffer
    if (tl == 0) {
      buffer = (u_char *) malloc(sizeof(u_char) * size);
      memcpy(buffer, src, size);
      tl = size;
    }
    // Enlarge Buffer
    else {
      int newTl = size + tl;
      buffer = (u_char *) realloc(buffer, sizeof(u_char) * newTl);
      memcpy(buffer + tl, src, size);
      tl = newTl;
    }
  }
  // Buffer Handling
  virtual void Close() = 0;
  virtual Interpreter::Result FillBuffer(word args, TaskStack *taskStack) = 0;
  // Store Interface
  word ToWord() {
    return Store::UnmanagedPointerToWord((void *) this);
  }
  static InputStreamBase *FromWord(word x) {
    void *p = Store::WordToUnmanagedPointer(x);
    Assert(p != INVALID_POINTER);
    return (InputStreamBase *) p;
  }
};

// FileInputStream
class FileInputStream : public InputStreamBase {
private:
  static const u_int BUFFER_SIZE = 8192;
  FILE *file;
  u_int exception;
public:
  // FileInputStream Constructor
  FileInputStream(char *filename) : InputStreamBase() {
    file = fopen(filename, "r");
    if (file == INVALID_POINTER) {
      exception = 1;
    }
  }
  // FileInputStream Functions
  u_int GotException() {
    return exception;
  }
  virtual void Close() {
    fclose(file);
  }
  virtual Interpreter::Result FillBuffer(word args, TaskStack *taskStack) {
    buffer = (u_char *) malloc(sizeof(u_char) * BUFFER_SIZE);
    // This is blocking
    AppendToBuffer(buffer,
		   (u_int) fread(buffer, sizeof(u_char), BUFFER_SIZE, file));
    taskStack->PopFrame();
    CONTINUE(args);
  }
};

// StringInputStream
class StringInputStream : public InputStreamBase {
private:
  Chunk *string;
public:
  // StringInputStream Constructor
  StringInputStream(Chunk *s) : InputStreamBase() {
    string = s; // to be checked
  }
  // StringInputStream Functions
  virtual void Close() {}
  virtual Interpreter::Result FillBuffer(word args, TaskStack *taskStack) {
    taskStack->PopFrame();
    if (string == INVALID_POINTER) {
      Scheduler::currentData = Store::IntToWord(0); // to be done
      return Interpreter::RAISE;
    }
    else {
      AppendToBuffer((u_char *) string->GetBase(), string->GetSize());
      string = INVALID_POINTER;
      CONTINUE(args);
    }
  }
};

// Pickle Arguments
class PickleArgs : private Block {
private:
  static const u_int STREAM_POS = 0;
  static const u_int ENV_POS    = 1;
  static const u_int COUNT_POS  = 2;
  static const u_int SIZE       = 3;
public:
  using Block::ToWord;
  // PickleArgs Accessors
  InputStreamBase *GetStream() {
    return InputStreamBase::FromWord(GetArg(STREAM_POS));
  }
  word GetEnv() {
    return GetArg(ENV_POS);
  }
  int GetCount() {
    return Store::DirectWordToInt(GetArg(COUNT_POS));
  }
  // PickleArgs Constructor
  static PickleArgs *New(InputStreamBase *is, word env, int count) {
    Block *p = Store::AllocBlock(TUPARGS_LABEL, SIZE);
    p->InitArg(STREAM_POS, is->ToWord());
    p->InitArg(ENV_POS, env);
    p->InitArg(COUNT_POS, count);
    return (PickleArgs *) p;
  }
  // PickleArgs Untagging
  static PickleArgs *FromWord(word args) {
    Block *p = Store::DirectWordToBlock(args);
    Assert(p != INVALID_POINTER && p->GetLabel() == TUPARGS_LABEL);
    return (PickleArgs *) p;
  }
};

// InputInterpreter
class InputInterpreter : public Interpreter {
private:
  static InputInterpreter *self;
public:
  // InputInterpreter Constructor
  InputInterpreter() : Interpreter() {}
  // InputInterpreter Static Constructor
  static void Init() {
    self = new InputInterpreter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual const char *ToString(word args, TaskStack *taskStack);
};

//
// InputInterpreter Functions
//
InputInterpreter *InputInterpreter::self;

void InputInterpreter::PushFrame(TaskStack *taskStack) {
  taskStack->PushFrame(StackFrame::New(INPUT_FRAME, self, 0)->ToWord());
}

Interpreter::Result InputInterpreter::Run(word args, TaskStack *taskStack) {
  PickleArgs *pargs   = PickleArgs::FromWord(args);
  InputStreamBase *is = pargs->GetStream();
  return is->FillBuffer(args, taskStack);
}

const char *InputInterpreter::Identify() {
  return "InputInterpreter";
}

const char *InputInterpreter::ToString(word args, TaskStack *taskStack) {
  return "InputInterpreter::ToString";
}

// TransformInterpreter Frame
class TransformFrame : private StackFrame {
private:
  static const u_int TRANSIENT_POS = 0;
  static const u_int TUPLE_POS     = 1;
  static const u_int SIZE          = 2;
public:
  using Block::ToWord;
  // TransformFrame Accessors
  Transient *GetTransient() {
    return Store::WordToTransient(StackFrame::GetArg(TRANSIENT_POS));
  }
  Tuple *GetTuple() {
    return Tuple::FromWord(StackFrame::GetArg(TUPLE_POS));
  }
  // TransformFrame Constructor
  static TransformFrame *New(Interpreter *interpreter,
			     Transient *transient, Tuple *tuple) {
    StackFrame *frame = StackFrame::New(TRANSFORM_FRAME, interpreter, SIZE);
    frame->ReplaceArg(TRANSIENT_POS, transient->ToWord());
    frame->ReplaceArg(TUPLE_POS, tuple->ToWord());
    return (TransformFrame *) frame;
  }
  // TransformFrame Untagging
  static TransformFrame *FromWord(word frame) {
    Block *p = Store::DirectWordToBlock(frame);
    Assert(p != INVALID_POINTER &&
	   p->GetLabel() == (BlockLabel) TRANSFORM_FRAME);
    return (TransformFrame *) frame;
  }
};

// TransformInterpreter
class TransformInterpreter : public Interpreter {
private:
  static TransformInterpreter *self;
public:
  // TransformInterpreter Constructor
  TransformInterpreter() : Interpreter() {}
  // TransformInterpreter Static Constructor
  static void Init() {
    self = new TransformInterpreter();
  }
  // Frame Handling
  static void TransformInterpreter::PushFrame(TaskStack *taskStack,
					      Transient *transient,
					      Tuple *tuple);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual const char *ToString(word args, TaskStack *taskStack);
};

// ApplyTransform Function
static inline
word ApplyTransform(Chunk *f, word x) {
  char *fs = f->GetBase();
  u_int len = f->GetSize();
  if ((len == sizeof("Alice.primitive.value") - 1) &&
      !strncmp(fs, "Alice.primitive.value", len)) {
    Block *xp = Store::WordToBlock(x);
    return PrimitiveTable::LookupValue(Chunk::FromWord(xp->GetArg(1)));
  }
  else if ((len == sizeof("Alice.primitive.function") - 1) &&
	   !strncmp(fs, "Alice.primitive.function", len)) {
    Block *xp = Store::WordToBlock(x);
    return PrimitiveTable::LookupFunction(Chunk::FromWord(xp->GetArg(1)));
  } else if ((len == sizeof("Alice.function") - 1) && 
	     !strncmp(fs, "Alice.function", len)) {
    // x->AssertWidth(6);
    // to be done here
    return Store::IntToWord(0);
  }
  Assert(0);
  return Store::IntToWord(0);
}

//
// TransformInterpreter Functions
//
TransformInterpreter *TransformInterpreter::self;

void TransformInterpreter::PushFrame(TaskStack *taskStack,
				     Transient *transient,
				     Tuple *tuple) {
  TransformFrame *frame = TransformFrame::New(self, transient, tuple);
  taskStack->PushFrame(frame->ToWord());
}

const char *TransformInterpreter::Identify() {
  return "TransformInterpreter";
}

Interpreter::Result TransformInterpreter::Run(word args, TaskStack *taskStack) {
  TransformFrame *frame = TransformFrame::FromWord(taskStack->GetFrame());
  Transient *transient = frame->GetTransient();
  Tuple *tuple         = frame->GetTuple();
  Chunk *f             = Chunk::FromWord(tuple->Sel(0));
  word x               = tuple->Sel(1);
  transient->Become(REF_LABEL, ApplyTransform(f, x));
  taskStack->PopFrame(); // Discard Frame
  CONTINUE(args);
}

const char *TransformInterpreter::ToString(word args, TaskStack *taskStack) {
  return "TransformInterpreter::ToString";
}

// UnpickleInterpreter Frame
class UnpickleFrame : private StackFrame {
private:
  static const u_int BLOCK_POS   = 0;
  static const u_int INDEX_POS   = 1;
  static const u_int NUMELEM_POS = 2;
  static const u_int SIZE        = 3;
public:
  using Block::ToWord;
  // UnpickleFrame Accessors
  word GetBlock() {
    return StackFrame::GetArg(BLOCK_POS);
  }
  int GetIndex() {
    return Store::WordToInt(StackFrame::GetArg(INDEX_POS));
  }
  int GetNumElems() {
    return Store::WordToInt(StackFrame::GetArg(NUMELEM_POS));
  }
  // UnpickleFrame Constructor
  static UnpickleFrame *New(Interpreter *interpreter,
			    word x, int i, int n) {
    StackFrame *frame = StackFrame::New(UNPICKLE_FRAME, interpreter, SIZE);
    frame->ReplaceArg(BLOCK_POS, x);
    frame->ReplaceArg(INDEX_POS, Store::IntToWord(i));
    frame->ReplaceArg(NUMELEM_POS, Store::IntToWord(n));
    return (UnpickleFrame *) frame;
  }
  // UnpickleFrame Untagging
  static UnpickleFrame *FromWord(word frame) {
    Block *p = Store::DirectWordToBlock(frame);
    Assert(p != INVALID_POINTER &&
	   p->GetLabel() == (BlockLabel) UNPICKLE_FRAME);
    return (UnpickleFrame *) p;
  }
};

// UnpickleInterpreter
class UnpickleInterpreter : public Interpreter {
private:
  static UnpickleInterpreter *self;
public:
// Pickle Tags
  class Tag {
  public:
    enum PickleTags {
      POSINT,
      NEGINT,
      CHUNK,
      BLOCK,
      TUPLE,
      CLOSURE,
      REF,
      TRANSFORM
    };
  };
  // UnpickleInterpreter Constructor
  UnpickleInterpreter() : Interpreter() {}
  // UnpickleInterpreter Static Constructor
  static void Init() {
    self = new UnpickleInterpreter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack, word block, int i, int n);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack);
  // Handle to be done
  // Debugging
  virtual const char *Identify();
  virtual const char *ToString(word args, TaskStack *taskStack);
};

//
// UnpickleInterpreter Functions
//
UnpickleInterpreter *UnpickleInterpreter::self;

void UnpickleInterpreter::PushFrame(TaskStack *taskStack,
				    word block, int i, int n) {
  taskStack->PushFrame(UnpickleFrame::New(self, block, i, n)->ToWord());
}

// Unpickle Helpers
static inline
void PushUnpickleFrame(TaskStack *taskStack, word block, int i, int n) {
  if (i != n) {
    UnpickleInterpreter::PushFrame(taskStack, block, i, n);
  }
}

static inline
void Set(word block, int i, word y) {
  Store::DirectWordToBlock(block)->ReplaceArg(i, y);
}

static inline
void AddToEnv(word env, int count, word value) {
  HashTable *table = HashTable::FromWordDirect(env);
  table->InsertItem(Store::IntToWord(count), value);
}

static inline
word SelFromEnv(word env, int count) {
  HashTable *table = HashTable::FromWordDirect(env);
  return table->GetItem(Store::IntToWord(count));
}

// End of Buffer requires rereading;
// therefore we reinstall the old taskstack
// to be done: more efficient solution
#define CHECK_EOB()                         \
  if (is->IsEOB()) {                        \
    taskStack->PushFrame(frame->ToWord());  \
    InputInterpreter::PushFrame(taskStack); \
    Scheduler::currentArgs = args;          \
    return Interpreter::CONTINUE;           \
  }

// Core Unpickle Function
Interpreter::Result UnpickleInterpreter::Run(word args, TaskStack *taskStack) {
  UnpickleFrame *frame = UnpickleFrame::FromWord(taskStack->GetFrame());
  word x = frame->GetBlock();
  int i  = frame->GetIndex();
  int n  = frame->GetNumElems();
  // It is save to do this since we have the frame argument
  taskStack->PopFrame();
  if (i == n) {
    // We are finished!
    CONTINUE(args);
  }
  else {
    PickleArgs *pargs   = PickleArgs::FromWord(args);
    InputStreamBase *is = pargs->GetStream();
    word env            = pargs->GetEnv();
    int count           = pargs->GetCount();
    u_char tag          = is->GetByte();
    CHECK_EOB();
    switch ((Tag::PickleTags) tag) {
    case Tag::POSINT:
      {
	u_int y = is->GetUInt(); CHECK_EOB();
	Set(x, i, Store::IntToWord(y)); // to be checked
	is->Commit();
	PushUnpickleFrame(taskStack, x, (i + 1), n);
	CONTINUE(args);
      }
      break;
    case Tag::NEGINT:
      {
	u_int y = is->GetUInt(); CHECK_EOB();
	Set(x, i, Store::IntToWord((0 - (int) y))); // to be checked
	is->Commit();
	PushUnpickleFrame(taskStack, x, (i + 1), n);
	CONTINUE(args);
      }
      break;
    case Tag::CHUNK:
      {
	u_int size    = is->GetUInt(); CHECK_EOB();
	u_char *bytes = is->GetBytes(size); CHECK_EOB();
	Chunk *y      = Store::AllocChunk(size);
	memcpy(y->GetBase(), bytes, size);
	Set(x, i, y->ToWord());
	AddToEnv(env, count, y->ToWord());
	is->Commit();
	PushUnpickleFrame(taskStack, x, (i + 1), n);
	CONTINUE(PickleArgs::New(is, env, count + 1)->ToWord());
      }
      break;
    case Tag::BLOCK:
      {
	u_int label = is->GetUInt(); CHECK_EOB();
	u_int size  = is->GetUInt(); CHECK_EOB();
	word   y    = Store::AllocBlock((BlockLabel) label, size)->ToWord();
	Set(x, i, y);
	AddToEnv(env, count, y);
	is->Commit();
	PushUnpickleFrame(taskStack, x, i + 1, n);
	UnpickleInterpreter::PushFrame(taskStack, y, 0, size);
	CONTINUE(PickleArgs::New(is, env, count + 1)->ToWord());
      }
      break;
    case Tag::TUPLE:
      {
	u_int size = is->GetUInt(); CHECK_EOB();
	word y     = Tuple::New(size)->ToWord(); // to be checked
	Set(x, i, y);
	AddToEnv(env, count, y);
	is->Commit();
	PushUnpickleFrame(taskStack, x, i + 1, n);
	UnpickleInterpreter::PushFrame(taskStack, y, 0, size);
	CONTINUE(PickleArgs::New(is, env, count + 1)->ToWord());
      }
      break;
    case Tag::CLOSURE:
      {
	u_int size = is->GetUInt(); CHECK_EOB();
	word cc    = Store::IntToWord(0); // shall be concrete code
	word y     = Closure::New(cc, size)->ToWord(); // to be checked
	Set(x, i, y);
	AddToEnv(env, count, y);
	is->Commit();
	PushUnpickleFrame(taskStack, x, i + 1, n);
	UnpickleInterpreter::PushFrame(taskStack, y, 0, size);
	CONTINUE(PickleArgs::New(is, env, count + 1)->ToWord());
      }
      break;
    case Tag::TRANSFORM:
      {
	Transient *y = (Transient *) Future::New(); // to be checked
	word yw = y->ToWord();
	Set(x, i, yw);
	AddToEnv(env, count, yw);
	is->Commit();
	Tuple *y2 = Tuple::New(2); // to be checked
	PushUnpickleFrame(taskStack, x, i + 1, n);
	UnpickleInterpreter::PushFrame(taskStack, y2->ToWord(), 0, 2);
	TransformInterpreter::PushFrame(taskStack, y, y2);
	CONTINUE(PickleArgs::New(is, env, count + 1)->ToWord());
      }
      break;
    case Tag::REF:
      {
	u_int index = is->GetUInt(); CHECK_EOB();
	Set(x, i, SelFromEnv(env, index));
	is->Commit();
	PushUnpickleFrame(taskStack, x, i + 1, n);
	CONTINUE(args);
      }
    default:
      Assert(0);
      CONTINUE(args);
    }
  }
}

const char *UnpickleInterpreter::Identify() {
  return "UnpickleInterpreter";
}

const char *UnpickleInterpreter::ToString(word args, TaskStack *taskStack) {
  return "UnpickleInterpreter::ToString";
}

// PickleUnpackInterpeter Frame
class PickleUnpackFrame : private StackFrame {
private:
  static const u_int TUPLE_POS = 0;
  static const u_int SIZE      = 1;
public:
  using Block::ToWord;
  // PickleUnpackFrame Accessors
  Tuple *GetTuple() {
    return Tuple::FromWord(StackFrame::GetArg(TUPLE_POS));
  }
  // PickleUnpackFrame Constructor
  static PickleUnpackFrame *New(Interpreter *interpreter, Tuple *x) {
    StackFrame *frame = StackFrame::New(PICKLE_UNPACK_FRAME, interpreter, SIZE);
    frame->ReplaceArg(TUPLE_POS, x->ToWord());
    return (PickleUnpackFrame *) frame;
  }
  // PickleUnpackFrame Untagging
  static PickleUnpackFrame *FromWord(word frame) {
    Block *p = Store::DirectWordToBlock(frame);
    Assert(p != INVALID_POINTER &&
	   p->GetLabel() == (BlockLabel) PICKLE_UNPACK_FRAME);
    return (PickleUnpackFrame *) p;
  }
};

// PickleUnpackInterpeter
class PickleUnpackInterpeter : public Interpreter {
private:
  static PickleUnpackInterpeter *self;
public:
  // PickleUnpackInterpeter Constructor
  PickleUnpackInterpeter() : Interpreter() {}
  // PickleUnpackInterpeter Static Constructor
  static void Init() {
    self = new PickleUnpackInterpeter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack, Tuple *x);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack);
  // Handle to be done
  // Debugging
  virtual const char *Identify();
  virtual const char *ToString(word args, TaskStack *taskStack);
};

//
// PickleUnpackInterpeter Functions
//
PickleUnpackInterpeter *PickleUnpackInterpeter::self;

void PickleUnpackInterpeter::PushFrame(TaskStack *taskStack, Tuple *x) {
  taskStack->PushFrame(PickleUnpackFrame::New(self, x)->ToWord());
}

Interpreter::Result
PickleUnpackInterpeter::Run(word args, TaskStack *taskStack) {
  PickleUnpackFrame *frame = PickleUnpackFrame::FromWord(taskStack->GetFrame());
  Tuple *x                 = frame->GetTuple();
  taskStack->PopFrame();
  CONTINUE(Interpreter::OneArg(x->Sel(0)));
}

const char *PickleUnpackInterpeter::Identify() {
  return "PickleUnpackInterpeter";
}

const char *PickleUnpackInterpeter::ToString(word args, TaskStack *taskStack) {
  return "PickleUnpackInterpeter::ToString";
}

// PickleLoadInterpreter Frame
class PickleLoadFrame : private StackFrame {
private:
  static const u_int TUPLE_POS = 0;
  static const u_int SIZE      = 1;
public:
  using Block::ToWord;
  // PickleLoadFrame Accessors
  Tuple *GetTuple() {
    return Tuple::FromWord(StackFrame::GetArg(TUPLE_POS));
  }
  // PickleLoadFrame Constructor
  static PickleLoadFrame *New(Interpreter *interpreter, Tuple *x) {
    StackFrame *frame = StackFrame::New(PICKLE_LOAD_FRAME, interpreter, SIZE);
    frame->ReplaceArg(TUPLE_POS, x->ToWord());
    return (PickleLoadFrame *) frame;
  }
  // PickleLoadFrame Untagging
  static PickleLoadFrame *FromWord(word frame) {
    Block *p = Store::DirectWordToBlock(frame);
    Assert(p != INVALID_POINTER &&
	   p->GetLabel() == (BlockLabel) PICKLE_LOAD_FRAME);
    return (PickleLoadFrame *) p;
  }
};

// PickleLoadInterpreter
class PickleLoadInterpreter : public Interpreter {
private:
  static PickleLoadInterpreter *self;
public:
  // PickleLoadInterpreter Constructor
  PickleLoadInterpreter() : Interpreter() {}
  // PickleLoadInterpreter Static Constructor
  static void Init() {
    self = new PickleLoadInterpreter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack, Tuple *x);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual const char *ToString(word args, TaskStack *taskStack);
};

//
// PickleLoadInterpreter Functions
//
PickleLoadInterpreter *PickleLoadInterpreter::self;

void PickleLoadInterpreter::PushFrame(TaskStack *taskStack, Tuple *x) {
  taskStack->PushFrame(PickleLoadFrame::New(self, x)->ToWord());
}

Interpreter::Result
PickleLoadInterpreter::Run(word args, TaskStack *taskStack) {
  PickleLoadFrame *frame = PickleLoadFrame::FromWord(taskStack->GetFrame());
  PickleArgs *pargs      = PickleArgs::FromWord(args);
  InputStreamBase *is    = pargs->GetStream();
  Tuple *x               = frame->GetTuple();
  is->Close();
  taskStack->PopFrame();
  CONTINUE(Interpreter::OneArg(x->Sel(0)));
}

const char *PickleLoadInterpreter::Identify() {
  return "PickleLoadInterpreter";
}

const char *PickleLoadInterpreter::ToString(word args, TaskStack *taskStack) {
  return "PickleLoadInterpreter::ToString";
}

//
// Unpickler Functions
//
static const u_int INITIAL_TABLE_SIZE = 16; // to be checked

Interpreter::Result Unpickler::Unpack(Chunk *s, TaskStack *taskStack) {
  Tuple *x = Tuple::New(1);
  InputStreamBase *is = (InputStreamBase *) new StringInputStream(s);
  HashTable *env      = HashTable::New(HashTable::INT_KEY, INITIAL_TABLE_SIZE);
  taskStack->PopFrame();
  PickleUnpackInterpeter::PushFrame(taskStack, x);
  UnpickleInterpreter::PushFrame(taskStack, x->ToWord(), 0, 1);
  CONTINUE(PickleArgs::New(is, env->ToWord(), 0)->ToWord());
}

Interpreter::Result Unpickler::Load(char *filename, TaskStack *taskStack) {
  Tuple *x            = Tuple::New(1);
  FileInputStream *is = new FileInputStream(filename);
  taskStack->PopFrame();
  if (is->GotException()) {
    delete is;
    Scheduler::currentData = Store::IntToWord(0); // to be done
    return Interpreter::RAISE;
  }
  HashTable *env = HashTable::New(HashTable::INT_KEY, INITIAL_TABLE_SIZE);
  PickleLoadInterpreter::PushFrame(taskStack, x);
  UnpickleInterpreter::PushFrame(taskStack, x->ToWord(), 0, 1);
  CONTINUE(PickleArgs::New((InputStreamBase *) is, env->ToWord(), 0)->ToWord());
}

void Unpickler::Init() {
  // Setup internal Interpreters
  InputInterpreter::Init();
  TransformInterpreter::Init();
  UnpickleInterpreter::Init();
  PickleUnpackInterpeter::Init();
  PickleLoadInterpreter::Init();
}
