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

enum ComponentTag {
  EVALUATED, UNEVALUATED
};

// Tracing
static bool traceFlag;

static void Trace(const char *prefix, Chunk *key) {
  if (traceFlag) {
    std::fprintf(stderr, "[boot-linker] %s %.*s\n", prefix,
		 (int) key->GetSize(), key->GetBase());
  }
}

// File name handling: Resolving, Localizing
static u_int ParentDir(char *s, u_int offset) {
  while (offset && (s[--offset] != '/'));
  return offset;
}

static Chunk *Resolve(Chunk *base, Chunk *rel) {
  u_int bSize  = base->GetSize();
  u_int rSize  = rel->GetSize();
  u_int offset = bSize;
  char *bPtr   = base->GetBase();
  char *rPtr   = rel->GetBase();
  while (true) {
    offset = ParentDir(bPtr, offset);
    if ((rSize < 3) || std::memcmp(rPtr, "../", 3)) {
      break;
    }
    else {
      rPtr += 3;
      rSize -= 3;
    }
  }
  if (offset == 0) {
    Chunk *path = Store::AllocChunk(rSize);
    std::memcpy(path->GetBase(), rPtr, rSize);
    return path;
  }
  else {
    Chunk *path = Store::AllocChunk(offset + 1 + rSize);
    char *pPtr  = path->GetBase();
    std::memcpy(pPtr, bPtr, offset);
    pPtr[offset] = '/';
    std::memcpy(pPtr + offset + 1, rPtr, rSize);
    return path;
  }
}

static Chunk *Localize(Chunk *key) {
  //--** Hack to ensure NUL-terminated strings
  Chunk *aliceHome = Store::DirectWordToChunk(Properties::aliceHome);
  u_int hSize      = aliceHome->GetSize();
  u_int kSize      = key->GetSize();
  Chunk *path      = Store::AllocChunk(hSize + kSize + 5);
  char *base       = path->GetBase();
  std::memcpy(base, aliceHome->GetBase(), hSize);
  std::memcpy(base + hSize, key->GetBase(), kSize);
  std::memcpy(base + hSize + kSize, ".stc", 4);
  base[hSize + kSize + 4] = '\0';
  return path;
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
  static void PushFrame(TaskStack *taskStack, Chunk *key,
			word closure, Vector *imports);
  // Execution
  virtual Result Run(TaskStack *taskStack);
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
  virtual Result Run(TaskStack *taskStack);
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
  virtual Result Run(TaskStack *taskStack);
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
  virtual Result Run(TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// Interpreter Frames
//
class ApplyFrame : private StackFrame {
private:
  static const u_int KEY_POS     = 0;
  static const u_int CLOSURE_POS = 1;
  static const u_int IMPORTS_POS = 2;
  static const u_int SIZE        = 3;
public:
  using Block::ToWord;
  // ApplyFrame Accessors
  Chunk *GetKey() {
    return Store::WordToChunk(StackFrame::GetArg(KEY_POS));
  }
  word GetClosure() {
    return StackFrame::GetArg(CLOSURE_POS);
  }
  Vector *GetImports() {
    return Vector::FromWordDirect(StackFrame::GetArg(IMPORTS_POS));
  }
  // ApplyFrame Constructor
  static ApplyFrame *New(Interpreter *interpreter, Chunk *key,
			 word closure, Vector *imports) {
    StackFrame *frame = StackFrame::New(APPLY_FRAME, interpreter, SIZE);
    frame->InitArg(KEY_POS, key->ToWord());
    frame->InitArg(CLOSURE_POS, closure);
    frame->InitArg(IMPORTS_POS, imports->ToWord());
    return static_cast<ApplyFrame *>(frame);
  }
  // ApplyFrame Untagging
  static ApplyFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == APPLY_FRAME);
    return static_cast<ApplyFrame *>(p);
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
    return static_cast<EnterFrame *>(frame);
  }
  // EnterFrame Untagging
  static EnterFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == ENTER_FRAME);
    return static_cast<EnterFrame *>(p);
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
    return static_cast<LinkFrame *>(frame);
  }
  // LinkFrame Untagging
  static LinkFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == LINK_FRAME);
    return static_cast<LinkFrame *>(p);
  }
};

class LoadFrame : private StackFrame {
private:
  static const u_int KEY_POS = 0;
  static const u_int SIZE    = 1;
public:
  using Block::ToWord;
  // LoadFrame Accessors
  Chunk *GetKey() {
    return Store::WordToChunk(StackFrame::GetArg(KEY_POS));
  }
  // LoadFrame Constructor
  static LoadFrame *New(Interpreter *interpreter, Chunk *key) {
    StackFrame *frame = StackFrame::New(LOAD_FRAME, interpreter, SIZE);
    frame->InitArg(KEY_POS, key->ToWord());
    return static_cast<LoadFrame *>(frame);
  }
  // LoadFrame Untagging
  static LoadFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == LOAD_FRAME);
    return static_cast<LoadFrame *>(p);
  }
};

//
// Interpreter Implementations
//
// ApplyInterpreter
ApplyInterpreter *ApplyInterpreter::self;

void ApplyInterpreter::PushFrame(TaskStack *taskStack, Chunk *key,
				 word closure, Vector *imports) {
  taskStack->PushFrame(ApplyFrame::New(self, key, closure, imports)->ToWord());
}

Interpreter::Result ApplyInterpreter::Run(TaskStack *taskStack) {
  ApplyFrame *frame = ApplyFrame::FromWordDirect(taskStack->GetFrame());
  Chunk *key        = frame->GetKey();
  word closure      = frame->GetClosure();
  Vector *imports   = frame->GetImports(); // (string * sign) vector
  taskStack->PopFrame();
  Trace("applying", key);
  u_int n      = imports->GetLength();
  Vector *strs = Vector::New(n);
  for (u_int i = 0; i < n; i++) {
    Tuple *t = Tuple::FromWord(imports->Sub(i));
    Assert(t != INVALID_POINTER);
    t->AssertWidth(2);
    Chunk *key2 = Resolve(key, Store::WordToChunk(t->Sel(0)));
    Component *entry = BootLinker::LookupComponent(key2);
    Assert(entry != INVALID_POINTER);
    strs->Init(i, entry->GetStr());
  }
  Scheduler::nArgs = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = strs->ToWord();
  return taskStack->PushCall(closure);
}

const char *ApplyInterpreter::Identify() {
  return "ApplyInterpreter";
}

void ApplyInterpreter::DumpFrame(word frameWord) {
  ApplyFrame *frame = ApplyFrame::FromWordDirect(frameWord);
  Chunk *key = frame->GetKey();
  std::fprintf(stderr, "Apply %.*s\n", (int) key->GetSize(), key->GetBase());
}

// EnterInterpreter
EnterInterpreter *EnterInterpreter::self;

void EnterInterpreter::PushFrame(TaskStack *taskStack, Chunk *key, word sign) {
  taskStack->PushFrame(EnterFrame::New(self, key, sign)->ToWord());
}

Interpreter::Result EnterInterpreter::Run(TaskStack *taskStack) {
  EnterFrame *frame = EnterFrame::FromWordDirect(taskStack->GetFrame());
  Chunk *key        = frame->GetKey();
  word sign         = frame->GetSign();
  taskStack->PopFrame();
  Trace("entering", key);
  Interpreter::Construct();
  BootLinker::EnterComponent(key, sign, Scheduler::currentArgs[0]);
  return Interpreter::CONTINUE;
}

const char *EnterInterpreter::Identify() {
  return "EnterInterpreter";
}

void EnterInterpreter::DumpFrame(word frameWord) {
  EnterFrame *frame = EnterFrame::FromWordDirect(frameWord);
  Chunk *key = frame->GetKey();
  std::fprintf(stderr, "Enter %.*s\n", (int) key->GetSize(), key->GetBase());
}

// LinkInterpreter
LinkInterpreter *LinkInterpreter::self;

void LinkInterpreter::PushFrame(TaskStack *taskStack, Chunk *key) {
  taskStack->PushFrame(LinkFrame::New(self, key)->ToWord());
}

Interpreter::Result LinkInterpreter::Run(TaskStack *taskStack) {
  LinkFrame *frame = LinkFrame::FromWordDirect(taskStack->GetFrame());
  Chunk *key       = frame->GetKey();
  Trace("linking", key);
  taskStack->PopFrame();
  Interpreter::Construct();
  TagVal *tagVal = TagVal::FromWord(Scheduler::currentArgs[0]);
  Assert(tagVal != INVALID_POINTER);
  switch (static_cast<ComponentTag>(tagVal->GetTag())) {
  case EVALUATED:
    {
      tagVal->AssertWidth(2);
      word sign = tagVal->Sel(0);
      word str  = tagVal->Sel(1);
      EnterInterpreter::PushFrame(taskStack, key, sign);
      Scheduler::nArgs = 0;
      Scheduler::currentArgs[0] = str;
      return Interpreter::CONTINUE;
    }
    break;
  case UNEVALUATED:
    {
      tagVal->AssertWidth(3);
      word closure    = tagVal->Sel(0);
      Vector *imports = Vector::FromWord(tagVal->Sel(1));
      word sign       = tagVal->Sel(2);
      Assert(imports != INVALID_POINTER);
      // Push EnterFrame
      EnterInterpreter::PushFrame(taskStack, key, sign);
      // Push ApplyFrame
      ApplyInterpreter::PushFrame(taskStack, key, closure, imports);
      // Push LoadFrames for imports: string * sign vector
      for (u_int i = imports->GetLength(); i--;) {
	Tuple *t = Tuple::FromWord(imports->Sub(i));
	Assert(t != INVALID_POINTER);
	t->AssertWidth(2);
	Chunk *rel  = Store::WordToChunk(t->Sel(0));
	Chunk *key2 = Resolve(key, rel);
	if (BootLinker::LookupComponent(key2) == INVALID_POINTER) {
	  LoadInterpreter::PushFrame(taskStack, key2);
	}
      }
      Scheduler::nArgs = 0;
      return Interpreter::CONTINUE;
    }
    break;
  default:
    Error("Boot Linker: invalid component tag");
  }
}

const char *LinkInterpreter::Identify() {
  return "LinkInterpreter";
}

void LinkInterpreter::DumpFrame(word frameWord) {
  LinkFrame *frame = LinkFrame::FromWordDirect(frameWord);
  Chunk *key = frame->GetKey();
  std::fprintf(stderr, "Link %.*s\n", (int) key->GetSize(), key->GetBase());
}

// LoadInterpreter
LoadInterpreter *LoadInterpreter::self;

void LoadInterpreter::PushFrame(TaskStack *taskStack, Chunk *key) {
  taskStack->PushFrame(LoadFrame::New(self, key)->ToWord());
}

Interpreter::Result LoadInterpreter::Run(TaskStack *taskStack) {
  LoadFrame *frame = LoadFrame::FromWordDirect(taskStack->GetFrame());
  Chunk *key       = frame->GetKey();
  taskStack->PopFrame();
  if (BootLinker::LookupComponent(key) != INVALID_POINTER) {
    Scheduler::nArgs = 0;
    return Interpreter::CONTINUE;
  }
  Trace("loading", key);
  LinkInterpreter::PushFrame(taskStack, key);
  taskStack->PushFrame(frame->ToWord());
  return Unpickler::Load(Localize(key), taskStack);
}

const char *LoadInterpreter::Identify() {
  return "LoadInterpreter";
}

void LoadInterpreter::DumpFrame(word frameWord) {
  LoadFrame *frame = LoadFrame::FromWordDirect(frameWord);
  Chunk *key = frame->GetKey();
  std::fprintf(stderr, "Load %.*s\n", (int) key->GetSize(), key->GetBase());
}

//
// BootLinker Functions
//
static const u_int INITIAL_TABLE_SIZE = 16; // to be checked
static const u_int INITIAL_QUEUE_SIZE = 16; // to be checked

word BootLinker::componentTable;
word BootLinker::keyQueue;
u_int BootLinker::numberOfEntries;

void BootLinker::Init(NativeComponent *nativeComponents) {
  RootSet::Add(componentTable);
  RootSet::Add(keyQueue);
  componentTable =
    HashTable::New(HashTable::BLOCK_KEY, INITIAL_TABLE_SIZE)->ToWord();
  keyQueue = Queue::New(INITIAL_QUEUE_SIZE)->ToWord();
  numberOfEntries = 0;
  // Initialize Interpreters
  ApplyInterpreter::Init();
  EnterInterpreter::Init();
  LinkInterpreter::Init();
  LoadInterpreter::Init();
  // Enter built-in native components
  while (nativeComponents->name != NULL) {
    word (*init)(void) = nativeComponents->init;
    EnterComponent(static_cast<Chunk *>(String::New(nativeComponents->name)),
		   Store::IntToWord(0), // NONE
		   init());
    nativeComponents++;
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
    return Component::FromWordDirect(componentTable->GetItem(keyWord));
  } else {
    return INVALID_POINTER;
  }
}

static word urlWord;

word BootLinker::Link(Chunk *url) {
  traceFlag = getenv("ALICE_TRACE_BOOT_LINKER") != NULL;
  TaskStack *taskStack = TaskStack::New();
  LoadInterpreter::PushFrame(taskStack, url);
  Scheduler::NewThread(0, Store::IntToWord(0), taskStack);
  urlWord = url->ToWord();
  RootSet::Add(urlWord);
  Scheduler::Run();
  RootSet::Remove(urlWord);
  Component *component = LookupComponent(Store::WordToChunk(urlWord));
  if (component == INVALID_POINTER) {
    return Store::IntToWord(0);
  }
  else {
    return component->GetStr();
  }
}
