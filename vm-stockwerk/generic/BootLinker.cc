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
#include "alice/Types.hh" //--** avoid Alice dependencies

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
  virtual u_int GetFrameSize(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *sFrame);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
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
  virtual u_int GetFrameSize(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *sFrame);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
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
  virtual u_int GetFrameSize(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *sFrame);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
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
  virtual u_int GetFrameSize(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *sFrame);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
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
  virtual u_int GetFrameSize(StackFrame *sFrame);
  // Execution
  virtual Result Run(StackFrame *sFrame);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

//
// Worker Frames
//
class ApplyFrame: private StackFrame {
private:
  enum { KEY_POS, CLOSURE_POS, IMPORTS_POS, SIZE };
public:
  // ApplyFrame Accessors
  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
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
    NEW_STACK_FRAME(frame, worker, SIZE);
    frame->InitArg(KEY_POS, key->ToWord());
    frame->InitArg(CLOSURE_POS, closure);
    frame->InitArg(IMPORTS_POS, imports->ToWord());
    return static_cast<ApplyFrame *>(frame);
  }
};

class EnterFrame: private StackFrame {
private:
  enum { KEY_POS, SIGN_POS, SIZE };
public:
  // EnterFrame Accessors
  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  String *GetKey() {
    return String::FromWordDirect(StackFrame::GetArg(KEY_POS));
  }
  word GetSign() {
    return StackFrame::GetArg(SIGN_POS);
  }
  // EnterFrame Constructor
  static EnterFrame *New(Worker *worker, String *key, word sign) {
    NEW_STACK_FRAME(frame, worker, SIZE);
    frame->InitArg(KEY_POS, key->ToWord());
    frame->InitArg(SIGN_POS, sign);
    return static_cast<EnterFrame *>(frame);
  }
};

class LinkFrame: private StackFrame {
private:
  enum { KEY_POS, SIZE };
public:
  // LinkFrame Accessors
  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  String *GetKey() {
    return String::FromWordDirect(StackFrame::GetArg(KEY_POS));
  }
  // LinkFrame Constructor
  static LinkFrame *New(Worker *worker, String *key) {
    NEW_STACK_FRAME(frame, worker, SIZE);
    frame->InitArg(KEY_POS, key->ToWord());
    return static_cast<LinkFrame *>(frame);
  }
};

class LoadFrame: private StackFrame {
private:
  enum { KEY_POS, SIZE };
public:
  // LoadFrame Accessors
  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  String *GetKey() {
    return String::FromWordDirect(StackFrame::GetArg(KEY_POS));
  }
  // LoadFrame Constructor
  static LoadFrame *New(Worker *worker, String *key) {
    NEW_STACK_FRAME(frame, worker, SIZE);
    frame->InitArg(KEY_POS, key->ToWord());
    return static_cast<LoadFrame *>(frame);
  }
  static LoadFrame *New(Thread *thread, Worker *worker, String *key) {
    NEW_THREAD_STACK_FRAME(frame, thread, worker, SIZE);
    frame->InitArg(KEY_POS, key->ToWord());
    return static_cast<LoadFrame *>(frame);
  }
};

class BootFrame: private StackFrame {
private:
  enum { KEY_POS, SIZE };
public:
  // BootFrame Accessors
  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  String *GetKey() {
    return String::FromWordDirect(StackFrame::GetArg(KEY_POS));
  }
  // BootFrame Constructor
  static BootFrame *New(Thread *thread, Worker *worker, String *key) {
    NEW_THREAD_STACK_FRAME(frame, thread, worker, SIZE);
    frame->InitArg(KEY_POS, key->ToWord());
    return static_cast<BootFrame *>(frame);
  }
};

//
// Worker Implementations
//
// ApplyWorker
ApplyWorker *ApplyWorker::self;

void ApplyWorker::PushFrame(String *key, word closure, Vector *imports) {
  ApplyFrame::New(self, key, closure, imports);
}

u_int ApplyWorker::GetFrameSize(StackFrame *sFrame) {
  ApplyFrame *frame = static_cast<ApplyFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result ApplyWorker::Run(StackFrame *sFrame) {
  ApplyFrame *frame = static_cast<ApplyFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  String *key = frame->GetKey();
  word closure = frame->GetClosure();
  Vector *imports = frame->GetImports(); // (string * sign) vector
  Scheduler::PopFrame(frame->GetSize());
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

void ApplyWorker::DumpFrame(StackFrame *sFrame) {
  ApplyFrame *frame = static_cast<ApplyFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  String *key = frame->GetKey();
  std::fprintf(stderr, "Apply %.*s\n", (int) key->GetSize(), key->GetValue());
}

// EnterWorker
EnterWorker *EnterWorker::self;

void EnterWorker::PushFrame(String *key, word sign) {
  EnterFrame::New(self, key, sign);
}

u_int EnterWorker::GetFrameSize(StackFrame *sFrame) {
  EnterFrame *frame = static_cast<EnterFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result EnterWorker::Run(StackFrame *sFrame) {
  EnterFrame *frame = static_cast<EnterFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  String *key = frame->GetKey();
  word sign = frame->GetSign();
  Scheduler::PopFrame(frame->GetSize());
  Trace("entering", key);
  Worker::Construct();
  BootLinker::EnterComponent(key, sign, Scheduler::currentArgs[0]);
  return Worker::CONTINUE;
}

const char *EnterWorker::Identify() {
  return "EnterWorker";
}

void EnterWorker::DumpFrame(StackFrame *sFrame) {
  EnterFrame *frame = static_cast<EnterFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  String *key = frame->GetKey();
  std::fprintf(stderr, "Enter %.*s\n", (int) key->GetSize(), key->GetValue());
}

// LinkWorker
LinkWorker *LinkWorker::self;

void LinkWorker::PushFrame(String *key) {
  LinkFrame::New(self, key);
}

u_int LinkWorker::GetFrameSize(StackFrame *sFrame) {
  LinkFrame *frame = static_cast<LinkFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result LinkWorker::Run(StackFrame *sFrame) {
  LinkFrame *frame = static_cast<LinkFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  String *key = frame->GetKey();
  Scheduler::PopFrame(frame->GetSize());
  Trace("linking", key);
  Worker::Construct();
  TagVal *tagVal = TagVal::FromWord(Scheduler::currentArgs[0]);
  Assert(tagVal != INVALID_POINTER);
  switch (tagVal->GetTag()) {
  case Types::EVALUATED:
    {
      tagVal->AssertWidth(2);
      word sign = tagVal->Sel(Types::inf1);
      word str = tagVal->Sel(Types::mod);
      EnterWorker::PushFrame(key, sign);
      Scheduler::nArgs = 0;
      Scheduler::currentArgs[0] = str;
      return Worker::CONTINUE;
    }
    break;
  case Types::UNEVALUATED:
    {
      tagVal->AssertWidth(3);
      word closure = tagVal->Sel(Types::body);
      Vector *imports = Vector::FromWord(tagVal->Sel(Types::imports));
      word sign = tagVal->Sel(Types::inf2);
      Assert(imports != INVALID_POINTER);
      EnterWorker::PushFrame(key, sign);
      ApplyWorker::PushFrame(key, closure, imports);
      // Push LoadFrames for imports: string * sign vector
      for (u_int i = imports->GetLength(); i--; ) {
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

void LinkWorker::DumpFrame(StackFrame *sFrame) {
  LinkFrame *frame = static_cast<LinkFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  String *key = frame->GetKey();
  std::fprintf(stderr, "Link %.*s\n", (int) key->GetSize(), key->GetValue());
}

// LoadWorker
LoadWorker *LoadWorker::self;

void LoadWorker::PushFrame(String *key) {
  LoadFrame::New(self, key);
}

void LoadWorker::PushFrame(Thread *thread, String *key) {
  LoadFrame::New(thread, self, key);
}

u_int LoadWorker::GetFrameSize(StackFrame *sFrame) {
  LoadFrame *frame = static_cast<LoadFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result LoadWorker::Run(StackFrame *sFrame) {
  LoadFrame *frame = static_cast<LoadFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  String *key = frame->GetKey();
  Scheduler::PopFrame(frame->GetSize());
  if (BootLinker::LookupComponent(key) != INVALID_POINTER) {
    Scheduler::nArgs = 0;
    return Worker::CONTINUE;
  }
  Trace("loading", key);
  LinkWorker::PushFrame(key);
  LoadWorker::PushFrame(key); //--** what is this good for?
  return Unpickler::Load(Localize(key));
}

const char *LoadWorker::Identify() {
  return "LoadWorker";
}

void LoadWorker::DumpFrame(StackFrame *sFrame) {
  LoadFrame *frame = static_cast<LoadFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  String *key = frame->GetKey();
  std::fprintf(stderr, "Load %.*s\n", (int) key->GetSize(), key->GetValue());
}

// BootWorker
BootWorker *BootWorker::self;

void BootWorker::PushFrame(Thread *thread, String *key) {
  BootFrame::New(thread, self, key);
}

u_int BootWorker::GetFrameSize(StackFrame *sFrame) {
  BootFrame *frame = static_cast<BootFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result BootWorker::Run(StackFrame *sFrame) {
  BootFrame *frame = static_cast<BootFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  String *key = frame->GetKey();
  Scheduler::PopFrame(frame->GetSize());
  Component *component = BootLinker::LookupComponent(key);
  Assert(component != INVALID_POINTER);
  Record *record = Record::FromWord(component->GetStr());
  Assert(record != INVALID_POINTER);
  word boot = record->PolySel(UniqueString::New(String::New("boot")));
  Scheduler::nArgs = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = Properties::rootUrl;
  // t2;
  return Scheduler::PushCall(boot);
}

const char *BootWorker::Identify() {
  return "BootWorker";
}

void BootWorker::DumpFrame(StackFrame *sFrame) {
  BootFrame *frame = static_cast<BootFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
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
  componentTable = ChunkMap::New(INITIAL_TABLE_SIZE)->ToWord();
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
    word sign = Store::IntToWord(Types::NONE);
    EnterComponent(key, sign, init());
    nativeComponents++;
  }
}

void BootLinker::EnterComponent(String *key, word sign, word str) {
  Assert(!GetComponentTable()->IsMember(key->ToWord()));
  GetComponentTable()->Put(key->ToWord(),
			   Component::New(sign, str)->ToWord());
  GetKeyQueue()->Enqueue(key->ToWord());
  numberOfEntries++;
}

Component *BootLinker::LookupComponent(String *key) {
  ChunkMap *componentTable = GetComponentTable();
  word keyWord = key->ToWord();
  if (componentTable->IsMember(keyWord)) {
    return Component::FromWordDirect(componentTable->Get(keyWord));
  } else {
    return INVALID_POINTER;
  }
}

void BootLinker::Link(String *url) {
  traceFlag = std::getenv("ALICE_TRACE_BOOT_LINKER") != NULL;
  Thread *thread = Scheduler::NewThread(0, Store::IntToWord(0));
  BootWorker::PushFrame(thread, url);
  LoadWorker::PushFrame(thread, url);
}
