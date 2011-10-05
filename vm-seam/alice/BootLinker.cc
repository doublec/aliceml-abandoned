//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "alice/BootLinker.hh"
#endif

#include <cstdio>
#include "alice/Data.hh"
#include "alice/Types.hh"
#include "alice/AliceLanguageLayer.hh"
#include "alice/BootLinker.hh"


namespace {

  // Tracing
  bool traceFlag;

  void Trace(const char *prefix, String *key) {
    if (traceFlag) {
      std::fprintf(stderr, "[boot-linker] %s %.*s\n", prefix,
		  static_cast<int>(key->GetSize()), key->GetValue());
    }
  }

  // File name handling: Resolving, Localizing
  u_int ParentDir(u_char *s, u_int offset) {
    while (offset && (s[--offset] != '/'));
    return offset;
  }

  String *Resolve(String *base, String *rel) {
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

  String *Localize(String *key) {
    String *aliceHome = String::FromWordDirect(AliceLanguageLayer::aliceHome);
    u_int hSize       = aliceHome->GetSize();
    u_int kSize       = key->GetSize();
    String *path      = String::New(hSize + kSize + 4);
    u_char *p         = path->GetValue();
    std::memcpy(p, aliceHome->GetValue(), hSize);
    std::memcpy(p + hSize, key->GetValue(), kSize);
    std::memcpy(p + hSize + kSize, ".alc", 4);
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
    virtual void DumpFrame(StackFrame *sFrame, std::ostream& out);
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
    virtual void DumpFrame(StackFrame *sFrame, std::ostream& out);
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
    virtual void DumpFrame(StackFrame *sFrame, std::ostream& out);
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
    virtual void DumpFrame(StackFrame *sFrame, std::ostream& out);
  };

  class StartWorker: public Worker {
  private:
    static StartWorker *self;
  public:
    // StartWorker Constructor
    StartWorker(): Worker() {}
    // StartWorker Static Constructor
    static void Init() {
      self = new StartWorker();
    }
    // Frame Handling
    static void PushFrame(String *key);
    static void PushFrame(Thread *thread, String *key);
    virtual u_int GetFrameSize(StackFrame *sFrame);
    // Execution
    virtual Result Run(StackFrame *sFrame);
    // Debugging
    virtual const char *Identify();
    virtual void DumpFrame(StackFrame *sFrame, std::ostream& out);
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


  /**
  * Links the boot component and invokes the boot function.
  *
  * This is done in a slightly convoluted way, because straight-up
  * link-ing it requires a type-check on the boot-component, which
  * fails during bootstrapping.
  */
  class StartFrame: private StackFrame {
  private:
    enum { KEY_POS, ACTION_POS, BOOT_URL_POS, SIZE };
  public:
    enum Action {
      MAKE_URL,
      LOAD_BOOT_COMPONENT,
      ENTER_BOOT_COMPONENT,
      LOOKUP_BOOT_COMPONENT,
      INVOKE_BOOT_FUNCTION
    };

    u_int GetSize() {
      return StackFrame::GetSize() + SIZE;
    }
    
    String *GetKey() {
      return String::FromWordDirect(StackFrame::GetArg(KEY_POS));
    }
    
    Action GetAction() {
      return static_cast<Action>(Store::WordToInt(GetArg(ACTION_POS)));
    }
    
    void NextAction() {
      Assert(GetAction() != INVOKE_BOOT_FUNCTION);
      ReplaceArg(ACTION_POS, Store::IntToWord(static_cast<Action>(GetAction() + 1)));
    }
    
    word GetBootUrl() {
      return GetArg(BOOT_URL_POS);
    }
    
    void SetBootUrl(word url) {
      ReplaceArg(BOOT_URL_POS, url);
    }
    
    static StartFrame *New(Worker *worker, String *key) {
      NEW_STACK_FRAME(frame, worker, SIZE);
      frame->InitArg(KEY_POS, key->ToWord());
      frame->InitArg(ACTION_POS, Store::IntToWord(MAKE_URL));
      frame->InitArg(BOOT_URL_POS, Store::IntToWord(0));
      return static_cast<StartFrame *>(frame);
    }
    
    static StartFrame *New(Thread *thread, Worker *worker, String *key) {
      NEW_THREAD_STACK_FRAME(frame, thread, worker, SIZE);
      frame->InitArg(KEY_POS, key->ToWord());
      frame->InitArg(ACTION_POS, Store::IntToWord(MAKE_URL));
      frame->InitArg(BOOT_URL_POS, Store::IntToWord(0));
      return static_cast<StartFrame *>(frame);
    }
    
    static const char* StringOfAction(Action action){
      switch(action){
	case MAKE_URL              : return "MAKE_URL";
	case LOAD_BOOT_COMPONENT   : return "LOAD_BOOT_COMPONENT";
	case ENTER_BOOT_COMPONENT  : return "ENTER_BOOT_COMPONENT";
	case LOOKUP_BOOT_COMPONENT : return "LOOKUP_BOOT_COMPONENT";
	case INVOKE_BOOT_FUNCTION  : return "INVOKE_BOOT_FUNCTION";
      }
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
    ApplyFrame *frame = reinterpret_cast<ApplyFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);
    return frame->GetSize();
  }

  Worker::Result ApplyWorker::Run(StackFrame *sFrame) {
    ApplyFrame *frame = reinterpret_cast<ApplyFrame *>(sFrame);
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
    Scheduler::SetNArgs(1);
    Scheduler::SetCurrentArg(0, strs->ToWord());
    return Scheduler::PushCall(closure);
  }

  const char *ApplyWorker::Identify() {
    return "ApplyWorker";
  }

  void ApplyWorker::DumpFrame(StackFrame *sFrame, std::ostream& out) {
    ApplyFrame *frame = reinterpret_cast<ApplyFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);
    out << "[BootLinker::Apply] " << frame->GetKey() << std::endl;
  }

  // EnterWorker
  EnterWorker *EnterWorker::self;

  void EnterWorker::PushFrame(String *key, word sign) {
    EnterFrame::New(self, key, sign);
  }

  u_int EnterWorker::GetFrameSize(StackFrame *sFrame) {
    EnterFrame *frame = reinterpret_cast<EnterFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);
    return frame->GetSize();
  }

  Worker::Result EnterWorker::Run(StackFrame *sFrame) {
    EnterFrame *frame = reinterpret_cast<EnterFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);
    String *key = frame->GetKey();
    word sign = frame->GetSign();
    Scheduler::PopFrame(frame->GetSize());
    Trace("entering", key);
    Assert(Scheduler::GetNArgs() == 1);
    BootLinker::EnterComponent(key, sign, Scheduler::GetCurrentArg(0));
    return Worker::CONTINUE;
  }

  const char *EnterWorker::Identify() {
    return "EnterWorker";
  }

  void EnterWorker::DumpFrame(StackFrame *sFrame, std::ostream& out) {
    EnterFrame *frame = reinterpret_cast<EnterFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);
    out << "[BootLinker::Enter] " << frame->GetKey() << std::endl;
  }

  // LinkWorker
  LinkWorker *LinkWorker::self;

  void LinkWorker::PushFrame(String *key) {
    LinkFrame::New(self, key);
  }

  u_int LinkWorker::GetFrameSize(StackFrame *sFrame) {
    LinkFrame *frame = reinterpret_cast<LinkFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);
    return frame->GetSize();
  }

  Worker::Result LinkWorker::Run(StackFrame *sFrame) {
    LinkFrame *frame = reinterpret_cast<LinkFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);
    String *key = frame->GetKey();
    Scheduler::PopFrame(frame->GetSize());
    Trace("linking", key);
    Assert(Scheduler::GetNArgs() == 1);
    TagVal *tagVal = TagVal::FromWord(Scheduler::GetCurrentArg(0));
    Assert(tagVal != INVALID_POINTER);
    switch (tagVal->GetTag()) {
    case Types::EVALUATED:
      {
	tagVal->AssertWidth(2);
	word sign = tagVal->Sel(Types::inf1);
	word str = tagVal->Sel(Types::mod);
	EnterWorker::PushFrame(key, sign);
	Scheduler::SetNArgs(0);
	Scheduler::SetCurrentArg(0, str);
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
	Scheduler::SetNArgs(0);
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

  void LinkWorker::DumpFrame(StackFrame *sFrame, std::ostream& out) {
    LinkFrame *frame = reinterpret_cast<LinkFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);
    out << "[BootLinker::Link] " << frame->GetKey() << std::endl;
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
    LoadFrame *frame = reinterpret_cast<LoadFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);
    return frame->GetSize();
  }

  Worker::Result LoadWorker::Run(StackFrame *sFrame) {
    LoadFrame *frame = reinterpret_cast<LoadFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);
    String *key = frame->GetKey();
    Scheduler::PopFrame(frame->GetSize());
    Component *component = BootLinker::LookupComponent(key);
    if (component != INVALID_POINTER) {
      Scheduler::SetNArgs(1);
      Scheduler::SetCurrentArg(0, component->GetStr());
      return Worker::CONTINUE;
    }
    LinkWorker::PushFrame(key);
    LoadWorker::PushFrame(key); // popped by Unpickler::Load
    Trace("loading", key);
    return Unpickler::Load(Localize(key));
  }

  const char *LoadWorker::Identify() {
    return "LoadWorker";
  }

  void LoadWorker::DumpFrame(StackFrame *sFrame, std::ostream& out) {
    LoadFrame *frame = reinterpret_cast<LoadFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);
    out << "[BootLinker::Load] " << frame->GetKey() << std::endl;
  }

  // StartWorker
  StartWorker *StartWorker::self;

  void StartWorker::PushFrame(String *key) {
    StartFrame::New(self, key);
  }

  void StartWorker::PushFrame(Thread *thread, String *key) {
    StartFrame::New(thread, self, key);
  }

  u_int StartWorker::GetFrameSize(StackFrame *sFrame) {
    StartFrame *frame = reinterpret_cast<StartFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);
    return frame->GetSize();
  }


  /**
  * Assumes that lib/system/ComponentManager has already been entered.
  */
  Worker::Result CallPrimalComponentManager(const char* name){
    Record *cmWrap = Record::FromWord(
      BootLinker::LookupComponent("lib/system/ComponentManager")->GetStr());
    Record *cm = Record::FromWord(cmWrap->PolySel("ComponentManager$"));
    word closure = cm->PolySel(name);
    return Scheduler::PushCall(closure);
  }


  Worker::Result StartWorker::Run(StackFrame *sFrame) {
    StartFrame *frame = reinterpret_cast<StartFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);
    Construct();
    
    word appUrl = frame->GetKey()->ToWord();
    Trace(StartFrame::StringOfAction(frame->GetAction()), String::FromWord(appUrl));
    
    switch(frame->GetAction()){
      
      // turn boot-component url *string* into a *url* value
      case StartFrame::MAKE_URL: {
	// note that the (ComponentManager component) argument is ignored
	// assumption: Url component will have been entered during linking of ComponentManager
	Record *urlWrap =
	  Record::FromWord(BootLinker::LookupComponent("lib/system/Url")->GetStr());
	word urlFromString = Record::FromWord(urlWrap->PolySel("Url$"))->PolySel("fromString");
	
	Scheduler::SetNArgs(1);
	Scheduler::SetCurrentArg(0, String::New("x-alice:/lib/system/Boot")->ToWord());
	frame->NextAction();
	return Scheduler::PushCall(urlFromString);
      }
      
      // obtain an unevaluated boot-component
      case StartFrame::LOAD_BOOT_COMPONENT: {
	frame->SetBootUrl(Scheduler::GetCurrentArg(0));
	frame->NextAction();
	// dont need to set argument - the boot-component url is already in the argument register
	return CallPrimalComponentManager("load");
      }
      
      // enter the boot-component in the primal-component-manager
      case StartFrame::ENTER_BOOT_COMPONENT: {
	word bootCp = Scheduler::GetCurrentArg(0);
	Scheduler::SetNArgs(2);
	Scheduler::SetCurrentArg(0, frame->GetBootUrl());
	Scheduler::SetCurrentArg(1, bootCp);
	frame->NextAction();
	return CallPrimalComponentManager("enter");
      }
      
      // lookup the (evaluated) boot-component)
      case StartFrame::LOOKUP_BOOT_COMPONENT: {
	Scheduler::SetNArgs(1);
	Scheduler::SetCurrentArg(0, frame->GetBootUrl());
	frame->NextAction();
	return CallPrimalComponentManager("lookup");
      }
      
      // invoke the boot function
      case StartFrame::INVOKE_BOOT_FUNCTION: {
	// assume that no transients are involved
	TagVal *someBootCp = TagVal::FromWord(Scheduler::GetCurrentArg(0));
	TagVal *bootCp = TagVal::FromWord(someBootCp->Sel(0));
	Record *mod = Record::FromWord(bootCp->Sel(1));
	word boot = mod->PolySel("boot");
	
	Scheduler::PopFrame(frame->GetSize());
	Scheduler::SetNArgs(1);
	Scheduler::SetCurrentArg(0, appUrl);
	return Scheduler::PushCall(boot);
      }
    }
  }

  const char *StartWorker::Identify() {
    return "StartWorker";
  }

  void StartWorker::DumpFrame(StackFrame *sFrame, std::ostream& out) {
    StartFrame *frame = reinterpret_cast<StartFrame *>(sFrame);
    Assert(sFrame->GetWorker() == this);
    out << "[BootLinker::Start] " << frame->GetKey() << std::endl;
  }
  
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
  traceFlag = std::getenv("ALICE_TRACE_BOOT_LINKER") != NULL;
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
  StartWorker::Init();
  // Enter built-in native components
  while (nativeComponents->name != NULL) {
    word (*init)() = nativeComponents->init;
    String *key = String::New(nativeComponents->name);
    word sign = Store::IntToWord(Types::NONE);
    EnterComponent(key, sign, init());
    nativeComponents++;
  }
  // HACK ALERT: Enter fake ComponentManager (under URL that will be
  // derived by boot linker for resp. x-alice import in lib/system/Component)
  String *key = String::New("lib/system/x-alice:/lib/system/ComponentManager");
  word sign = Store::IntToWord(Types::NONE);
  EnterComponent(key, sign, Store::IntToWord(0));
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


Component *BootLinker::LookupComponent(const char* key) {
  return LookupComponent(String::New(key));
}


void BootLinker::Link(String *url) {
  Trace("init-link", url);
  StartWorker::PushFrame(url);
  LoadWorker::PushFrame(String::New("lib/system/ComponentManager"));
}

void BootLinker::Link(Thread *thread, String *url) {
  Trace("init-link", url);
  StartWorker::PushFrame(thread, url);
  LoadWorker::PushFrame(thread, String::New("lib/system/ComponentManager"));
}
