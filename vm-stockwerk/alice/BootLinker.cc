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
#include "generic/Tuple.hh"
#include "generic/Worker.hh"
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
// Worker Classes
//
class ApplyWorker: public Worker {
private:
  static ApplyWorker *self;
public:
  // ApplyWorker Constructor
  ApplyWorker(): Worker() {}
  // ApplyWorker Static Constructor
  static void Init() {
    self = new ApplyWorker();
  }
  // Frame Handling
  static void PushFrame(String *key, word closure, Vector *imports);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

class EnterWorker: public Worker {
private:
  static EnterWorker *self;
public:
  // EnterWorker Constructor
  EnterWorker(): Worker() {}
  // EnterWorker Static Constructor
  static void Init() {
    self = new EnterWorker();
  }
  // Frame Handling
  static void PushFrame(String *key, word sign);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

class LinkWorker: public Worker {
private:
  static LinkWorker *self;
public:
  // LinkWorker Constructor
  LinkWorker(): Worker() {}
  // LinkWorker Static Constructor
  static void Init() {
    self = new LinkWorker();
  }
  // Frame Handling
  static void PushFrame(String *key);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

class LoadWorker: public Worker {
private:
  static LoadWorker *self;
public:
  // LoadWorker Constructor
  LoadWorker(): Worker() {}
  // LoadWorker Static Constructor
  static void Init() {
    self = new LoadWorker();
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

class BootWorker: public Worker {
private:
  static BootWorker *self;
public:
  // BootWorker Constructor
  BootWorker(): Worker() {}
  // BootWorker Static Constructor
  static void Init() {
    self = new BootWorker();
  }
  // Frame Handling
  static void PushFrame(Thread *thread, String *key);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

//
// Worker Frames
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
  static ApplyFrame *New(Worker *worker, String *key,
			 word closure, Vector *imports) {
    StackFrame *frame = StackFrame::New(APPLY_FRAME, worker, SIZE);
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
  static EnterFrame *New(Worker *worker, String *key, word sign) {
    StackFrame *frame = StackFrame::New(ENTER_FRAME, worker, SIZE);
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
  static LinkFrame *New(Worker *worker, String *key) {
    StackFrame *frame = StackFrame::New(LINK_FRAME, worker, SIZE);
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
  static LoadFrame *New(Worker *worker, String *key) {
    StackFrame *frame = StackFrame::New(LOAD_FRAME, worker, SIZE);
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

class BootFrame: private StackFrame {
private:
  enum { KEY_POS, SIZE };
public:
  using Block::ToWord;
  // BootFrame Accessors
  String *GetKey() {
    return String::FromWordDirect(StackFrame::GetArg(KEY_POS));
  }
  // BootFrame Constructor
  static BootFrame *New(Worker *worker, String *key) {
    StackFrame *frame = StackFrame::New(BOOT_FRAME, worker, SIZE);
    frame->InitArg(KEY_POS, key->ToWord());
    return static_cast<BootFrame *>(frame);
  }
  // BootFrame Untagging
  static BootFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == BOOT_FRAME);
    return static_cast<BootFrame *>(p);
  }
};

//
// Worker Implementations
//
// ApplyWorker
ApplyWorker *ApplyWorker::self;

void ApplyWorker::PushFrame(String *key, word closure, Vector *imports) {
  Scheduler::PushFrame(ApplyFrame::New(self, key, closure, imports)->ToWord());
}

Worker::Result ApplyWorker::Run() {
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

const char *ApplyWorker::Identify() {
  return "ApplyWorker";
}

void ApplyWorker::DumpFrame(word wFrame) {
  ApplyFrame *frame = ApplyFrame::FromWordDirect(wFrame);
  String *key = frame->GetKey();
  std::fprintf(stderr, "Apply %.*s\n", (int) key->GetSize(), key->GetValue());
}

// EnterWorker
EnterWorker *EnterWorker::self;

void EnterWorker::PushFrame(String *key, word sign) {
  Scheduler::PushFrame(EnterFrame::New(self, key, sign)->ToWord());
}

Worker::Result EnterWorker::Run() {
  EnterFrame *frame = EnterFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  String *key = frame->GetKey();
  word sign = frame->GetSign();
  Trace("entering", key);
  Worker::Construct();
  BootLinker::EnterComponent(key, sign, Scheduler::currentArgs[0]);
  return Worker::CONTINUE;
}

const char *EnterWorker::Identify() {
  return "EnterWorker";
}

void EnterWorker::DumpFrame(word wFrame) {
  EnterFrame *frame = EnterFrame::FromWordDirect(wFrame);
  String *key = frame->GetKey();
  std::fprintf(stderr, "Enter %.*s\n", (int) key->GetSize(), key->GetValue());
}

// LinkWorker
LinkWorker *LinkWorker::self;

void LinkWorker::PushFrame(String *key) {
  Scheduler::PushFrame(LinkFrame::New(self, key)->ToWord());
}

Worker::Result LinkWorker::Run() {
  LinkFrame *frame = LinkFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  String *key = frame->GetKey();
  Trace("linking", key);
  Worker::Construct();
  TagVal *tagVal = TagVal::FromWord(Scheduler::currentArgs[0]);
  Assert(tagVal != INVALID_POINTER);
  switch (static_cast<ComponentTag>(tagVal->GetTag())) {
  case EVALUATED:
    {
      tagVal->AssertWidth(2);
      word sign = tagVal->Sel(0);
      word str = tagVal->Sel(1);
      EnterWorker::PushFrame(key, sign);
      Scheduler::nArgs = 0;
      Scheduler::currentArgs[0] = str;
      return Worker::CONTINUE;
    }
    break;
  case UNEVALUATED:
    {
      tagVal->AssertWidth(3);
      word closure = tagVal->Sel(0);
      Vector *imports = Vector::FromWord(tagVal->Sel(1));
      word sign = tagVal->Sel(2);
      Assert(imports != INVALID_POINTER);
      EnterWorker::PushFrame(key, sign);
      ApplyWorker::PushFrame(key, closure, imports);
      // Push LoadFrames for imports: string * sign vector
      for (u_int i = imports->GetLength(); i--;) {
	Tuple *t = Tuple::FromWord(imports->Sub(i));
	Assert(t != INVALID_POINTER);
	t->AssertWidth(2);
	String *rel = String::FromWordDirect(t->Sel(0));
	String *key2 = Resolve(key, rel);
	if (BootLinker::LookupComponent(key2) == INVALID_POINTER)
	  LoadWorker::PushFrame(key2);
      }
      Scheduler::nArgs = 0;
      return Worker::CONTINUE;
    }
    break;
  default:
    Error("Boot Linker: invalid component tag");
  }
}

const char *LinkWorker::Identify() {
  return "LinkWorker";
}

void LinkWorker::DumpFrame(word wFrame) {
  LinkFrame *frame = LinkFrame::FromWordDirect(wFrame);
  String *key = frame->GetKey();
  std::fprintf(stderr, "Link %.*s\n", (int) key->GetSize(), key->GetValue());
}

// LoadWorker
LoadWorker *LoadWorker::self;

void LoadWorker::PushFrame(String *key) {
  Scheduler::PushFrame(LoadFrame::New(self, key)->ToWord());
}

void LoadWorker::PushFrame(Thread *thread, String *key) {
  thread->PushFrame(LoadFrame::New(self, key)->ToWord());
}

Worker::Result LoadWorker::Run() {
  LoadFrame *frame = LoadFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  String *key = frame->GetKey();
  if (BootLinker::LookupComponent(key) != INVALID_POINTER) {
    Scheduler::nArgs = 0;
    return Worker::CONTINUE;
  }
  Trace("loading", key);
  LinkWorker::PushFrame(key);
  Scheduler::PushFrame(frame->ToWord()); //--** what is this good for?
  return Unpickler::Load(Localize(key));
}

const char *LoadWorker::Identify() {
  return "LoadWorker";
}

void LoadWorker::DumpFrame(word wFrame) {
  LoadFrame *frame = LoadFrame::FromWordDirect(wFrame);
  String *key = frame->GetKey();
  std::fprintf(stderr, "Load %.*s\n", (int) key->GetSize(), key->GetValue());
}

// BootWorker
BootWorker *BootWorker::self;

void BootWorker::PushFrame(Thread *thread, String *key) {
  thread->PushFrame(BootFrame::New(self, key)->ToWord());
}

Worker::Result BootWorker::Run() {
  BootFrame *frame = BootFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  String *key = frame->GetKey();
  Component *component = BootLinker::LookupComponent(key);
  Assert(component != INVALID_POINTER);
  Record *record = Record::FromWord(component->GetStr());
  Assert(record != INVALID_POINTER);
  word boot = record->PolySel(UniqueString::New(String::New("boot")));
  Scheduler::nArgs = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = Properties::rootUrl;
  return Scheduler::PushCall(boot);
}

const char *BootWorker::Identify() {
  return "BootWorker";
}

void BootWorker::DumpFrame(word wFrame) {
  BootFrame *frame = BootFrame::FromWordDirect(wFrame);
  String *key = frame->GetKey();
  std::fprintf(stderr, "Boot %.*s\n", (int) key->GetSize(), key->GetValue());
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
  // Initialize Workers
  ApplyWorker::Init();
  EnterWorker::Init();
  LinkWorker::Init();
  LoadWorker::Init();
  BootWorker::Init();
  // Enter built-in native components
  while (nativeComponents->name != NULL) {
    word (*init)() = nativeComponents->init;
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

void BootLinker::Link(String *url) {
  traceFlag = getenv("ALICE_TRACE_BOOT_LINKER") != NULL;
  Thread *thread = Scheduler::NewThread(0, Store::IntToWord(0));
  BootWorker::PushFrame(thread, url);
  LoadWorker::PushFrame(thread, url);
}
