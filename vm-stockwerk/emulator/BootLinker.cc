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
#pragma implementation "emulator/BootLinker.hh"
#endif

#include <cstdio>
#include "emulator/BootLinker.hh"

#include "emulator/RootSet.hh"
#include "emulator/Interpreter.hh"
#include "emulator/Scheduler.hh"
#include "emulator/Unpickler.hh"
#include "emulator/Alice.hh"

// Very convienent Macro
#define CONTINUE(args)           \
  Scheduler::currentArgs = args; \
  return Interpreter::CONTINUE;

// ModuleEntry
class ModuleEntry : private Block {
private:
  static const u_int ENTRY_LABEL = MIN_DATA_LABEL;
  static const u_int SIGN_POS    = 0;
  static const u_int MODULE_POS  = 1;
  static const u_int SIZE        = 2;
public:
  using Block::ToWord;
  // ModuleEntry Accessors
  word GetSign() {
    return GetArg(SIGN_POS);
  }
  word GetModule() {
    return GetArg(MODULE_POS);
  }
  // ModuleEntry Constructor
  static ModuleEntry *New(word sign, word module) {
    Block *p = Store::AllocBlock((BlockLabel) ENTRY_LABEL, SIZE);
    p->InitArg(SIGN_POS, sign);
    p->InitArg(MODULE_POS, module);
    return (ModuleEntry *) p;
  }
  // ModuleEntry Untagging
  static ModuleEntry *FromWord(word entry) {
    Block *p = Store::DirectWordToBlock(entry);
    Assert(p != INVALID_POINTER && p->GetLabel() == (BlockLabel) ENTRY_LABEL);
    return (ModuleEntry *) p;
  }
};

// Internal Resolve
static Chunk *ResolveUrl(Chunk *base, Chunk *rel) {
  u_int rSize  = rel->GetSize();
  char *bptr   = base->GetBase();
  char *bmax   = strrchr(bptr, '/');
  if (bmax == NULL) {
    Chunk *path = Store::AllocChunk(rSize);
    char *pptr  = path->GetBase();
    memcpy(pptr, rel->GetBase(), rSize);
    return path;
  }
  else {
    u_int n     = (bmax - bptr);
    Chunk *path = Store::AllocChunk(n + 1 + rSize);
    char *pptr  = path->GetBase();
    memcpy(pptr, bptr, n);
    pptr[n] = '/';
    memcpy(pptr + n + 1, rel->GetBase(), rSize);
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
  virtual const char *ToString(word args, TaskStack *taskStack);
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
  virtual const char *ToString(word args, TaskStack *taskStack);
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
  virtual const char *ToString(word args, TaskStack *taskStack);
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
  virtual const char *ToString(word args, TaskStack *taskStack);
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

Interpreter::Result ApplyInterpreter::Run(word args, TaskStack *taskStack) {
  ApplyFrame *frame = ApplyFrame::FromWord(taskStack->GetFrame());
  word bodyclosure  = frame->GetClosure();
  Vector *imports   = Vector::FromWord(frame->GetImports());
  Chunk *key        = frame->GetKey();
  taskStack->PopFrame();
  BootLinker::Trace("[boot-linker] applying", key);
  u_int n         = imports->GetLength();
  Vector *modules = Vector::New(n);
  // Order significant here?
  for (u_int i = 0; i < n; i++) {
    // imports = (string * sign) vector
    Tuple *t = Tuple::FromWord(imports->Sub(i));
    Assert(t != INVALID_POINTER);
    t->AssertWidth(2);
    Chunk *key2 = ResolveUrl(key, Store::WordToChunk(t->Sel(0)));
    ModuleEntry *entry =
      ModuleEntry::FromWord(BootLinker::GetModuleTable()->
			    GetItem(key2->ToWord()));
    modules->Init(i, entry->GetModule());
  }
  //  taskStack->PushCall(bodyclosure);
  CONTINUE(Interpreter::OneArg(modules->ToWord()));
}

const char *ApplyInterpreter::Identify() {
  return "ApplyInterpreter";
}

const char *ApplyInterpreter::ToString(word args, TaskStack *taskStack) {
  return "ApplyInterpreter::ToString";
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
  BootLinker::Trace("[boot-linker] entering", key);
  word module = Interpreter::Construct(args);
  BootLinker::GetModuleTable()->
    InsertItem(key->ToWord(),
	       ModuleEntry::New(sign, module)->ToWord());
  CONTINUE(Interpreter::OneArg(module));
}

const char *EnterInterpreter::Identify() {
  return "EnterInterpreter";
}

const char *EnterInterpreter::ToString(word args, TaskStack *taskStack) {
  return "EnterInterpreter::ToString";
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
  BootLinker::Trace("[boot-linker] linking", key);
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
	if (!BootLinker::GetModuleTable()->IsMember(key2->ToWord())) {
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
const char *LinkInterpreter::ToString(word args, TaskStack *taskStack) {
  return "LinkInterpreter::ToString";
}

// LoadInterpreter
LoadInterpreter *LoadInterpreter::self;

void LoadInterpreter::PushFrame(TaskStack *taskStack, Chunk *key) {
  taskStack->PushFrame(LoadFrame::New(self, key)->ToWord());
}

Interpreter::Result LoadInterpreter::Run(word args, TaskStack *taskStack) {
  LoadFrame *frame = LoadFrame::FromWord(taskStack->GetFrame());
  Chunk *key      = frame->GetString();
  taskStack->PopFrame();
  BootLinker::Trace("[boot-linker] loading", key);
  LinkInterpreter::PushFrame(taskStack, key);
  taskStack->PushFrame(frame->ToWord());
  return Unpickler::Load(BootLinker::MakeFileName(key)->GetBase(), taskStack);
}

const char *LoadInterpreter::Identify() {
  return "LoadInterpreter";
}

const char *LoadInterpreter::ToString(word args, TaskStack *taskStack) {
  return "LoadInterpreter::ToString";
}

//
// BootLinker Functions
//
static const u_int INITIAL_TABLE_SIZE = 16; // to be checked

word BootLinker::moduleTable;
u_int BootLinker::traceFlag;
char *BootLinker::aliceHome;

void BootLinker::Init(char *home, prim_table *builtins) {
  moduleTable = HashTable::New(HashTable::BLOCK_KEY,
			       INITIAL_TABLE_SIZE)->ToWord();
  RootSet::Add(moduleTable);
  aliceHome = home;
  // Initialize Interpreters
  ApplyInterpreter::Init();
  EnterInterpreter::Init();
  LinkInterpreter::Init();
  LoadInterpreter::Init();
  // Import builtin native Modules
  while (builtins->name != NULL) {
    fprintf(stderr, "BootLinker::Init: found builtins\n");
    GetModuleTable()->
      InsertItem(String::New(builtins->name)->ToWord(), builtins->module);
    builtins++;
  }
}

void BootLinker::Print(Chunk *c) {
  fprintf(stderr, "'%.*s'\n", (int) c->GetSize(), c->GetBase());
}

void BootLinker::Trace(const char *prefix, Chunk *key) {
  if (traceFlag) {
    fprintf(stderr, "%s '%.*s'\n", prefix, (int) key->GetSize(), key->GetBase());
  }
}

Chunk *BootLinker::MakeFileName(Chunk *key) {
  // Hack to ensure 0x00 terminated strings
  u_int hSize  = strlen(aliceHome);
  u_int kSize  = key->GetSize();
  Chunk *path  = Store::AllocChunk(hSize + kSize + 5);
  char *base   = path->GetBase();
  strcpy(base, aliceHome);
  strncpy(base + hSize, key->GetBase(), kSize);
  base[hSize + kSize] = (char) 0x00;
  strcat(base, ".stc");
  base[hSize + kSize + 4] = (char) 0x00;
  Print(path);
  return path;
}

word BootLinker::Link(Chunk *url) {
  TaskStack *taskStack = TaskStack::New();
  LoadInterpreter::PushFrame(taskStack, url);
  Scheduler::NewThread(Store::IntToWord(0), Interpreter::EmptyArg(), taskStack);
  Scheduler::Run();
  word entry = GetModuleTable()->GetItem(url->ToWord());
  return ModuleEntry::FromWord(entry)->GetModule();
}
