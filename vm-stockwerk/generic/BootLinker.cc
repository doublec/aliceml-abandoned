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
#pragma implementation "generic/BootLinker.hh"
#endif

#include <cstdio>
#include "generic/BootLinker.hh"
#include "generic/RootSet.hh"
#include "generic/Interpreter.hh"
#include "generic/StackFrame.hh"
#include "generic/Scheduler.hh"
#include "generic/Unpickler.hh"
#include "generic/Properties.hh"
#include "alice/Data.hh" //--** avoid Alice dependencies

enum ComponentTag {
  EVALUATED, UNEVALUATED
};

// Tracing
static bool traceFlag;

static void Trace(const char *prefix, String *key) {
  if (traceFlag) {
    std::fprintf(stderr, "[boot-linker] %s %.*s\n", prefix,
		 (int) key->GetSize(), key->GetValue());
  }
}

// File name handling: Resolving, Localizing
static u_int ParentDir(u_char *s, u_int offset) {
  while (offset && (s[--offset] != '/'));
  return offset;
}

static String *Resolve(String *base, String *rel) {
  u_int bSize  = base->GetSize();
  u_int rSize  = rel->GetSize();
  u_int offset = bSize;
  u_char *bPtr = base->GetValue();
  u_char *rPtr = rel->GetValue();
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
    return String::New(reinterpret_cast<char *>(rPtr), rSize);
  }
  else {
    String *path = String::New(offset + 1 + rSize);
    u_char *pPtr = path->GetValue();
    std::memcpy(pPtr, bPtr, offset);
    pPtr[offset] = '/';
    std::memcpy(pPtr + offset + 1, rPtr, rSize);
    return path;
  }
}

static String *Localize(String *key) {
  String *aliceHome = String::FromWordDirect(Properties::aliceHome);
  u_int hSize       = aliceHome->GetSize();
  u_int kSize       = key->GetSize();
  String *path      = String::New(hSize + kSize + 4);
  u_char *p         = path->GetValue();
  std::memcpy(p, aliceHome->GetValue(), hSize);
  std::memcpy(p + hSize, key->GetValue(), kSize);
  std::memcpy(p + hSize + kSize, ".stc", 4);
  return path;
}

//
// Interpreter Classes
//
class ApplyInterpreter: public Interpreter {
private:
  static ApplyInterpreter *self;
public:
  // ApplyInterpreter Constructor
  ApplyInterpreter(): Interpreter() {}
  // ApplyInterpreter Static Constructor
  static void Init() {
    self = new ApplyInterpreter();
  }
  // Frame Handling
  static void PushFrame(String *key, word closure, Vector *imports);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

class EnterInterpreter: public Interpreter {
private:
  static EnterInterpreter *self;
public:
  // EnterInterpreter Constructor
  EnterInterpreter(): Interpreter() {}
  // EnterInterpreter Static Constructor
  static void Init() {
    self = new EnterInterpreter();
  }
  // Frame Handling
  static void PushFrame(String *key, word sign);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

class LinkInterpreter: public Interpreter {
private:
  static LinkInterpreter *self;
public:
  // LinkInterpreter Constructor
  LinkInterpreter(): Interpreter() {}
  // LinkInterpreter Static Constructor
  static void Init() {
    self = new LinkInterpreter();
  }
  // Frame Handling
  static void PushFrame(String *key);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

class LoadInterpreter: public Interpreter {
private:
  static LoadInterpreter *self;
public:
  // LoadInterpreter Constructor
  LoadInterpreter(): Interpreter() {}
  // LoadInterpreter Static Constructor
  static void Init() {
    self = new LoadInterpreter();
  }
  // Frame Handling
  static void PushFrame(String *key);
  static void PushFrame(Thread *thread, String *key);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// Interpreter Frames
//
class ApplyFrame: private StackFrame {
private:
  enum { KEY_POS, CLOSURE_POS, IMPORTS_POS, SIZE };
public:
  using Block::ToWord;
  // ApplyFrame Accessors
  String *GetKey() {
    return String::FromWordDirect(StackFrame::GetArg(KEY_POS));
  }
  word GetClosure() {
    return StackFrame::GetArg(CLOSURE_POS);
  }
  Vector *GetImports() {
    return Vector::FromWordDirect(StackFrame::GetArg(IMPORTS_POS));
  }
  // ApplyFrame Constructor
  static ApplyFrame *New(Interpreter *interpreter, String *key,
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

class EnterFrame: private StackFrame {
private:
  enum { KEY_POS, SIGN_POS, SIZE };
public:
  using Block::ToWord;
  // EnterFrame Accessors
  String *GetKey() {
    return String::FromWordDirect(StackFrame::GetArg(KEY_POS));
  }
  word GetSign() {
    return StackFrame::GetArg(SIGN_POS);
  }
  // EnterFrame Constructor
  static EnterFrame *New(Interpreter *interpreter, String *key, word sign) {
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

class LinkFrame: private StackFrame {
private:
  enum { KEY_POS, SIZE };
public:
  using Block::ToWord;
  // LinkFrame Accessors
  String *GetKey() {
    return String::FromWordDirect(StackFrame::GetArg(KEY_POS));
  }
  // LinkFrame Constructor
  static LinkFrame *New(Interpreter *interpreter, String *key) {
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

class LoadFrame: private StackFrame {
private:
  enum { KEY_POS, SIZE };
public:
  using Block::ToWord;
  // LoadFrame Accessors
  String *GetKey() {
    return String::FromWordDirect(StackFrame::GetArg(KEY_POS));
  }
  // LoadFrame Constructor
  static LoadFrame *New(Interpreter *interpreter, String *key) {
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

void ApplyInterpreter::PushFrame(String *key, word closure, Vector *imports) {
  Scheduler::PushFrame(ApplyFrame::New(self, key, closure, imports)->ToWord());
}

Interpreter::Result ApplyInterpreter::Run() {
  ApplyFrame *frame = ApplyFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  String *key = frame->GetKey();
  word closure = frame->GetClosure();
  Vector *imports = frame->GetImports(); // (string * sign) vector
  Trace("applying", key);
  u_int n = imports->GetLength();
  Vector *strs = Vector::New(n);
  for (u_int i = 0; i < n; i++) {
    Tuple *t = Tuple::FromWord(imports->Sub(i));
    Assert(t != INVALID_POINTER);
    t->AssertWidth(2);
    String *key2 = Resolve(key, String::FromWordDirect(t->Sel(0)));
    Component *entry = BootLinker::LookupComponent(key2);
    Assert(entry != INVALID_POINTER);
    strs->Init(i, entry->GetStr());
  }
  Scheduler::nArgs = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = strs->ToWord();
  return Scheduler::PushCall(closure);
}

const char *ApplyInterpreter::Identify() {
  return "ApplyInterpreter";
}

void ApplyInterpreter::DumpFrame(word frameWord) {
  ApplyFrame *frame = ApplyFrame::FromWordDirect(frameWord);
  String *key = frame->GetKey();
  std::fprintf(stderr, "Apply %.*s\n", (int) key->GetSize(), key->GetValue());
}

// EnterInterpreter
EnterInterpreter *EnterInterpreter::self;

void EnterInterpreter::PushFrame(String *key, word sign) {
  Scheduler::PushFrame(EnterFrame::New(self, key, sign)->ToWord());
}

Interpreter::Result EnterInterpreter::Run() {
  EnterFrame *frame = EnterFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  String *key = frame->GetKey();
  word sign = frame->GetSign();
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
  String *key = frame->GetKey();
  std::fprintf(stderr, "Enter %.*s\n", (int) key->GetSize(), key->GetValue());
}

// LinkInterpreter
LinkInterpreter *LinkInterpreter::self;

void LinkInterpreter::PushFrame(String *key) {
  Scheduler::PushFrame(LinkFrame::New(self, key)->ToWord());
}

Interpreter::Result LinkInterpreter::Run() {
  LinkFrame *frame = LinkFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  String *key = frame->GetKey();
  Trace("linking", key);
  Interpreter::Construct();
  TagVal *tagVal = TagVal::FromWord(Scheduler::currentArgs[0]);
  Assert(tagVal != INVALID_POINTER);
  switch (static_cast<ComponentTag>(tagVal->GetTag())) {
  case EVALUATED:
    {
      tagVal->AssertWidth(2);
      word sign = tagVal->Sel(0);
      word str = tagVal->Sel(1);
      EnterInterpreter::PushFrame(key, sign);
      Scheduler::nArgs = 0;
      Scheduler::currentArgs[0] = str;
      return Interpreter::CONTINUE;
    }
    break;
  case UNEVALUATED:
    {
      tagVal->AssertWidth(3);
      word closure = tagVal->Sel(0);
      Vector *imports = Vector::FromWord(tagVal->Sel(1));
      word sign = tagVal->Sel(2);
      Assert(imports != INVALID_POINTER);
      EnterInterpreter::PushFrame(key, sign);
      ApplyInterpreter::PushFrame(key, closure, imports);
      // Push LoadFrames for imports: string * sign vector
      for (u_int i = imports->GetLength(); i--;) {
	Tuple *t = Tuple::FromWord(imports->Sub(i));
	Assert(t != INVALID_POINTER);
	t->AssertWidth(2);
	String *rel = String::FromWordDirect(t->Sel(0));
	String *key2 = Resolve(key, rel);
	if (BootLinker::LookupComponent(key2) == INVALID_POINTER)
	  LoadInterpreter::PushFrame(key2);
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
  String *key = frame->GetKey();
  std::fprintf(stderr, "Link %.*s\n", (int) key->GetSize(), key->GetValue());
}

// LoadInterpreter
LoadInterpreter *LoadInterpreter::self;

void LoadInterpreter::PushFrame(String *key) {
  Scheduler::PushFrame(LoadFrame::New(self, key)->ToWord());
}

void LoadInterpreter::PushFrame(Thread *thread, String *key) {
  thread->PushFrame(LoadFrame::New(self, key)->ToWord());
}

Interpreter::Result LoadInterpreter::Run() {
  LoadFrame *frame = LoadFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  String *key = frame->GetKey();
  if (BootLinker::LookupComponent(key) != INVALID_POINTER) {
    Scheduler::nArgs = 0;
    return Interpreter::CONTINUE;
  }
  Trace("loading", key);
  LinkInterpreter::PushFrame(key);
  Scheduler::PushFrame(frame->ToWord()); //--** what is this good for?
  return Unpickler::Load(Localize(key));
}

const char *LoadInterpreter::Identify() {
  return "LoadInterpreter";
}

void LoadInterpreter::DumpFrame(word frameWord) {
  LoadFrame *frame = LoadFrame::FromWordDirect(frameWord);
  String *key = frame->GetKey();
  std::fprintf(stderr, "Load %.*s\n", (int) key->GetSize(), key->GetValue());
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
    String *key = String::New(nativeComponents->name);
    word sign = Store::IntToWord(0); // 0 = NONE
    EnterComponent(key, sign, init());
    nativeComponents++;
  }
}

void BootLinker::EnterComponent(String *key, word sign, word str) {
  Assert(!GetComponentTable()->IsMember(key->ToWord()));
  GetComponentTable()->InsertItem(key->ToWord(),
				  Component::New(sign, str)->ToWord());
  GetKeyQueue()->Enqueue(key->ToWord());
  numberOfEntries++;
}

Component *BootLinker::LookupComponent(String *key) {
  HashTable *componentTable = GetComponentTable();
  word keyWord = key->ToWord();
  if (componentTable->IsMember(keyWord)) {
    return Component::FromWordDirect(componentTable->GetItem(keyWord));
  } else {
    return INVALID_POINTER;
  }
}

static word urlWord;

word BootLinker::Link(String *url) {
  traceFlag = getenv("ALICE_TRACE_BOOT_LINKER") != NULL;
  Thread *thread = Scheduler::NewThread(0, Store::IntToWord(0));
  LoadInterpreter::PushFrame(thread, url);
  urlWord = url->ToWord();
  RootSet::Add(urlWord);
  Scheduler::Run();
  RootSet::Remove(urlWord);
  Component *component = LookupComponent(String::FromWordDirect(urlWord));
  if (component == INVALID_POINTER) {
    return Store::IntToWord(0);
  }
  else {
    return component->GetStr();
  }
}
