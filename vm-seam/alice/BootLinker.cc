//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "emulator/BootLinker.hh"
#endif

#include <cstdio>
#include "emulator/BootLinker.hh"

#include "emulator/RootSet.hh"
#include "emulator/Interpreter.hh"
#include "emulator/Scheduler.hh"
#include "emulator/Unpickler.hh"
#include "emulator/Alice.hh"
#include "emulator/Properties.hh"

// Very convienent Macro
#define CONTINUE(args)           \
  Scheduler::currentArgs = args; \
  return Interpreter::CONTINUE;

static u_int ParentDir(char *s, u_int offset) {
  while (offset && (s[--offset] != '/'));
  return offset;
}

// Internal Resolve
static Chunk *ResolveUrl(Chunk *base, Chunk *rel) {
  u_int bSize  = base->GetSize();
  u_int rSize  = rel->GetSize();
  u_int offset = bSize;
  char *bPtr   = base->GetBase();
  char *rPtr   = rel->GetBase();
  while (true) {
    offset = ParentDir(bPtr, offset);
    if ((rSize < 3) || memcmp(rPtr, "../", 3)) {
      break;
    }
    else {
      rPtr += 3;
      rSize -= 3;
    }
  }
  if (offset == 0) {
    Chunk *path = Store::AllocChunk(rSize);
    memcpy(path->GetBase(), rPtr, rSize);
    return path;
  }
  else {
    Chunk *path = Store::AllocChunk(offset + 1 + rSize);
    char *pPtr  = path->GetBase();
    memcpy(pPtr, bPtr, offset);
    pPtr[offset] = '/';
    memcpy(pPtr + offset + 1, rPtr, rSize);
    return path;
  }
}

//
// Interpreter Classes
//
class ApplyInterpreter : public Interpreter {
private:
  static ApplyInterpreter *self;
public:
  // ApplyInterpreter Constructor
  ApplyInterpreter() : Interpreter() {}
  // ApplyInterpreter Static Constructor
  static void Init() {
    self = new ApplyInterpreter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack,
			word closure, word imports, Chunk *key);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

class EnterInterpreter : public Interpreter {
private:
  static EnterInterpreter *self;
public:
  // EnterInterpreter Constructor
  EnterInterpreter() : Interpreter() {}
  // EnterInterpreter Static Constructor
  static void Init() {
    self = new EnterInterpreter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack, Chunk *key, word sign);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

class LinkInterpreter : public Interpreter {
private:
  static LinkInterpreter *self;
public:
  // LinkInterpreter Constructor
  LinkInterpreter() : Interpreter() {}
  // LinkInterpreter Static Constructor
  static void Init() {
    self = new LinkInterpreter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack, Chunk *key);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

class LoadInterpreter : public Interpreter {
private:
  static LoadInterpreter *self;
public:
  // LoadInterpreter Constructor
  LoadInterpreter() : Interpreter() {}
  // LoadInterpreter Static Constructor
  static void Init() {
    self = new LoadInterpreter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack, Chunk *key);
  // Execution
  virtual Result Run(word args, TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// Interpreter Frames
//
class ApplyFrame : private StackFrame {
private:
  static const u_int CLOSURE_POS = 0;
  static const u_int IMPORTS_POS = 1;
  static const u_int KEY_POS     = 2;
  static const u_int SIZE        = 3;
public:
  using Block::ToWord;
  // ApplyFrame Accessors
  word GetClosure() {
    return StackFrame::GetArg(CLOSURE_POS);
  }
  word GetImports() {
    return StackFrame::GetArg(IMPORTS_POS);
  }
  Chunk *GetKey() {
    return Store::WordToChunk(StackFrame::GetArg(KEY_POS));
  }
  // ApplyFrame Constructor
  static ApplyFrame *New(Interpreter *interpreter,
			 word closure, word imports, Chunk *key) {
    StackFrame *frame = StackFrame::New(APPLY_FRAME, interpreter, SIZE);
    frame->InitArg(CLOSURE_POS, closure);
    frame->InitArg(IMPORTS_POS, imports);
    frame->InitArg(KEY_POS, key->ToWord());
    return (ApplyFrame *) frame;
  }
  // ApplyFrame Untagging
  static ApplyFrame *FromWord(word frame) {
    Block *p = Store::DirectWordToBlock(frame);
    Assert(p == INVALID_POINTER ||
	   p->GetLabel() == (BlockLabel) APPLY_FRAME);
    return (ApplyFrame *) p;
  }
};

class EnterFrame : private StackFrame {
private:
  static const u_int KEY_POS  = 0;
  static const u_int SIGN_POS = 1;
  static const u_int SIZE     = 1;
public:
  using Block::ToWord;
  // EnterFrame Accessors
  Chunk *GetKey() {
    return Store::WordToChunk(StackFrame::GetArg(KEY_POS));
  }
  word GetSign() {
    return StackFrame::GetArg(SIGN_POS);
  }
  // EnterFrame Constructor
  static EnterFrame *New(Interpreter *interpreter, Chunk *key, word sign) {
    StackFrame *frame = StackFrame::New(ENTER_FRAME, interpreter, SIZE);
    frame->InitArg(KEY_POS, key->ToWord());
    frame->InitArg(SIGN_POS, sign);
    return (EnterFrame *) frame;
  }
  // EnterFrame Untagging
  static EnterFrame *FromWord(word frame) {
    Block *p = Store::DirectWordToBlock(frame);
    Assert(p == INVALID_POINTER ||
	   p->GetLabel() == (BlockLabel) ENTER_FRAME);
    return (EnterFrame *) p;
  }
};

class LinkFrame : private StackFrame {
private:
  static const u_int KEY_POS = 0;
  static const u_int SIZE    = 1;
public:
  using Block::ToWord;
  // LinkFrame Accessors
  Chunk *GetKey() {
    return Store::WordToChunk(StackFrame::GetArg(KEY_POS));
  }
  // LinkFrame Constructor
  static LinkFrame *New(Interpreter *interpreter, Chunk *key) {
    StackFrame *frame = StackFrame::New(LINK_FRAME, interpreter, SIZE);
    frame->InitArg(KEY_POS, key->ToWord());
    return (LinkFrame *) frame;
  }
  // LinkFrame Untagging
  static LinkFrame *FromWord(word frame) {
    Block *p = Store::DirectWordToBlock(frame);
    Assert(p == INVALID_POINTER ||
	   p->GetLabel() == (BlockLabel) LINK_FRAME);
    return (LinkFrame *) p;
  }
};

class LoadFrame : private StackFrame {
private:
  static const u_int KEY_POS = 0;
  static const u_int SIZE    = 1;
public:
  using Block::ToWord;
  // LoadFrame Accessors
  Chunk *GetString() {
    return Store::WordToChunk(StackFrame::GetArg(KEY_POS));
  }
  // LoadFrame Constructor
  static LoadFrame *New(Interpreter *interpreter, Chunk *key) {
    StackFrame *frame = StackFrame::New(LOAD_FRAME, interpreter, SIZE);
    frame->InitArg(KEY_POS, key->ToWord());
    return (LoadFrame *) frame;
  }
  // LoadFrame Untagging
  static LoadFrame *FromWord(word frame) {
    Block *p = Store::DirectWordToBlock(frame);
    Assert(p == INVALID_POINTER ||
	   p->GetLabel() == (BlockLabel) LOAD_FRAME);
    return (LoadFrame *) p;
  }
};

//
// Interpreter Implementations
//
// ApplyInterpreter
ApplyInterpreter *ApplyInterpreter::self;

void ApplyInterpreter::PushFrame(TaskStack *taskStack,
				 word closure, word imports, Chunk *key) {
  taskStack->PushFrame(ApplyFrame::New(self, closure, imports, key)->ToWord());
}

Interpreter::Result ApplyInterpreter::Run(word, TaskStack *taskStack) {
  ApplyFrame *frame = ApplyFrame::FromWord(taskStack->GetFrame());
  word bodyclosure  = frame->GetClosure();
  Vector *imports   = Vector::FromWord(frame->GetImports());
  Chunk *key        = frame->GetKey();
  taskStack->PopFrame();
  BootLinker::Trace("applying", key);
  u_int n         = imports->GetLength();
  Vector *strs    = Vector::New(n);
  // Order significant here?
  for (u_int i = 0; i < n; i++) {
    // imports = (string * sign) vector
    Tuple *t = Tuple::FromWord(imports->Sub(i));
    Assert(t != INVALID_POINTER);
    t->AssertWidth(2);
    Chunk *key2 = ResolveUrl(key, Store::WordToChunk(t->Sel(0)));
    Component *entry = BootLinker::LookupComponent(key2);
    Assert(entry != INVALID_POINTER);
    strs->Init(i, entry->GetStr());
  }
  Scheduler::currentArgs = Interpreter::OneArg(strs->ToWord());
  return taskStack->PushCall(bodyclosure);
}

const char *ApplyInterpreter::Identify() {
  return "ApplyInterpreter";
}

void ApplyInterpreter::DumpFrame(word frameWord) {
  ApplyFrame *frame = ApplyFrame::FromWord(frameWord);
  Assert(frame != INVALID_POINTER);
  Chunk *key = frame->GetKey();
  fprintf(stderr, "Apply %.*s\n", (int) key->GetSize(), key->GetBase());
}

// EnterInterpreter
EnterInterpreter *EnterInterpreter::self;

void EnterInterpreter::PushFrame(TaskStack *taskStack, Chunk *key, word sign) {
  taskStack->PushFrame(EnterFrame::New(self, key, sign)->ToWord());
}

Interpreter::Result EnterInterpreter::Run(word args, TaskStack *taskStack) {
  EnterFrame *frame = EnterFrame::FromWord(taskStack->GetFrame());
  Chunk *key       = frame->GetKey();
  word sign         = frame->GetSign();
  taskStack->PopFrame();
  BootLinker::Trace("entering", key);
  word str = Interpreter::Construct(args);
  BootLinker::EnterComponent(key, sign, str);
  CONTINUE(Interpreter::OneArg(str));
}

const char *EnterInterpreter::Identify() {
  return "EnterInterpreter";
}

void EnterInterpreter::DumpFrame(word frameWord) {
  EnterFrame *frame = EnterFrame::FromWord(frameWord);
  Assert(frame != INVALID_POINTER);
  Chunk *key = frame->GetKey();
  fprintf(stderr, "Enter %.*s\n", (int) key->GetSize(), key->GetBase());
}

// LinkInterpreter
LinkInterpreter *LinkInterpreter::self;

void LinkInterpreter::PushFrame(TaskStack *taskStack, Chunk *key) {
  taskStack->PushFrame(LinkFrame::New(self, key)->ToWord());
}

typedef enum {
  EVALUATED, UNEVALUATED
} COMPONENT;

Interpreter::Result LinkInterpreter::Run(word args, TaskStack *taskStack) {
  LinkFrame *frame = LinkFrame::FromWord(taskStack->GetFrame());
  Chunk *key      = frame->GetKey();
  BootLinker::Trace("linking", key);
  taskStack->PopFrame();
  args = Interpreter::Construct(args);
  TagVal *targs = TagVal::FromWord(args);
  Assert(targs != INVALID_POINTER);
  switch ((COMPONENT) targs->GetTag()) {
  case EVALUATED:
    {
      targs->AssertWidth(2);
      word sign = targs->Sel(0);
      word x    = targs->Sel(1);
      EnterInterpreter::PushFrame(taskStack, key, sign);
      CONTINUE(Interpreter::OneArg(x));
    }
    break;
  case UNEVALUATED:
    {
      targs->AssertWidth(3);
      word bodyclosure = targs->Sel(0);
      word imports     = targs->Sel(1);
      word sign        = targs->Sel(2);
      // Add EnterFrame
      EnterInterpreter::PushFrame(taskStack, key, sign);
      // Add ApplyFrame
      ApplyInterpreter::PushFrame(taskStack, bodyclosure, imports, key);
      // Add the Load Frames of Imports: string * sign vector
      Vector *iv = Vector::FromWord(imports);
      Assert(iv != INVALID_POINTER);
      for (u_int i = iv->GetLength(); i--;) {
	Tuple *t = Tuple::FromWord(iv->Sub(i));
	Assert(t != INVALID_POINTER);
	t->AssertWidth(2);
	Chunk *rel  = Store::WordToChunk(t->Sel(0));
	Chunk *key2 = ResolveUrl(key, rel);
	if (BootLinker::LookupComponent(key2) == INVALID_POINTER) {
	  LoadInterpreter::PushFrame(taskStack, key2);
	}
      }
      CONTINUE(Interpreter::EmptyArg());
    }
    break;
  default:
    Assert(0);
    CONTINUE(args);
  }
}

const char *LinkInterpreter::Identify() {
  return "LinkInterpreter";
}

void LinkInterpreter::DumpFrame(word frameWord) {
  LinkFrame *frame = LinkFrame::FromWord(frameWord);
  Assert(frame != INVALID_POINTER);
  Chunk *key = frame->GetKey();
  fprintf(stderr, "Link %.*s\n", (int) key->GetSize(), key->GetBase());
}

// LoadInterpreter
LoadInterpreter *LoadInterpreter::self;

void LoadInterpreter::PushFrame(TaskStack *taskStack, Chunk *key) {
  taskStack->PushFrame(LoadFrame::New(self, key)->ToWord());
}

Interpreter::Result LoadInterpreter::Run(word, TaskStack *taskStack) {
  LoadFrame *frame = LoadFrame::FromWord(taskStack->GetFrame());
  Chunk *key      = frame->GetString();
  taskStack->PopFrame();
  if (BootLinker::LookupComponent(key) != INVALID_POINTER) {
    CONTINUE(Interpreter::EmptyArg());
  }
  BootLinker::Trace("loading", key);
  LinkInterpreter::PushFrame(taskStack, key);
  taskStack->PushFrame(frame->ToWord());
  return Unpickler::Load(BootLinker::MakeFileName(key), taskStack);
}

const char *LoadInterpreter::Identify() {
  return "LoadInterpreter";
}

void LoadInterpreter::DumpFrame(word frameWord) {
  LoadFrame *frame = LoadFrame::FromWord(frameWord);
  Assert(frame != INVALID_POINTER);
  Chunk *key = frame->GetString();
  fprintf(stderr, "Load %.*s\n", (int) key->GetSize(), key->GetBase());
}

//
// BootLinker Functions
//
static const u_int INITIAL_TABLE_SIZE = 16; // to be checked
static const u_int INITIAL_QUEUE_SIZE = 16; // to be checked

word BootLinker::componentTable;
word BootLinker::keyQueue;
u_int BootLinker::numberOfEntries;
u_int BootLinker::traceFlag;

void BootLinker::Init(prim_table *builtins) {
  componentTable = HashTable::New(HashTable::BLOCK_KEY,
				  INITIAL_TABLE_SIZE)->ToWord();
  RootSet::Add(componentTable);
  keyQueue = Queue::New(INITIAL_QUEUE_SIZE)->ToWord();
  RootSet::Add(keyQueue);
  numberOfEntries = 0;
  // Initialize Interpreters
  ApplyInterpreter::Init();
  EnterInterpreter::Init();
  LinkInterpreter::Init();
  LoadInterpreter::Init();
  // Import built-in native components
  while (builtins->name != NULL) {
    word (*f)(void) = builtins->init;
    EnterComponent((Chunk *) String::New(builtins->name),
		   Store::IntToWord(0), // NONE
		   f());
    builtins++;
  }
}

void BootLinker::Print(Chunk *c) {
  fprintf(stderr, "%.*s\n", (int) c->GetSize(), c->GetBase());
}

void BootLinker::Trace(const char *prefix, Chunk *key) {
  if (traceFlag) {
    fprintf(stderr, "[boot-linker] %s %.*s\n",
	    prefix, (int) key->GetSize(), key->GetBase());
  }
}

void BootLinker::EnterComponent(Chunk *key, word sign, word str) {
  Assert(!GetComponentTable()->IsMember(key->ToWord()));
  GetComponentTable()->InsertItem(key->ToWord(),
				  Component::New(sign, str)->ToWord());
  GetKeyQueue()->Enqueue(key->ToWord());
  numberOfEntries++;
}

Component *BootLinker::LookupComponent(Chunk *key) {
  HashTable *componentTable = GetComponentTable();
  word keyWord = key->ToWord();
  if (componentTable->IsMember(keyWord)) {
    return Component::FromWord(componentTable->GetItem(keyWord));
  } else {
    return INVALID_POINTER;
  }
}

Chunk *BootLinker::MakeFileName(Chunk *key) {
  // Hack to ensure NUL-terminated strings
  Chunk *aliceHome = Store::DirectWordToChunk(Properties::aliceHome);
  u_int hSize      = aliceHome->GetSize();
  u_int kSize      = key->GetSize();
  Chunk *path      = Store::AllocChunk(hSize + kSize + 5);
  char *base       = path->GetBase();
  memcpy(base, aliceHome->GetBase(), hSize);
  memcpy(base + hSize, key->GetBase(), kSize);
  memcpy(base + hSize + kSize, ".stc", 4);
  base[hSize + kSize + 4] = '\0';
  return path;
}

word BootLinker::Link(Chunk *url) {
  traceFlag = getenv("ALICE_TRACE_BOOT_LINKER") != NULL;
  TaskStack *taskStack = TaskStack::New();
  LoadInterpreter::PushFrame(taskStack, url);
  Scheduler::NewThread(Store::IntToWord(0), Interpreter::EmptyArg(), taskStack);
  word urlWord = url->ToWord();
  RootSet::Add(urlWord);
  Scheduler::Run();
  RootSet::Remove(urlWord);
  Component *component = LookupComponent(url);
  if (component == INVALID_POINTER) {
    return Store::IntToWord(0);
  }
  else {
    return component->GetStr();
  }
}
