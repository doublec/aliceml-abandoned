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
#pragma implementation "generic/Profiler.hh"
#endif

#if PROFILE
#include "generic/Profiler.hh"
#include "generic/RootSet.hh"
#include "generic/Worker.hh"
#include "generic/StackFrame.hh"
#include "generic/String.hh"
#include "generic/ConcreteCode.hh"
#include "alice/Data.hh"

class ProfileEntry : private Tuple {
protected:
  enum {
    NAME_POS, NB_CALLS_POS, NB_HEAP_POS, NB_CLOSURES_POS, NB_INSTANCES_POS,
    NB_INSTRS_POS, SIZE
  };

  void Modify(u_int index, u_int value) {
    u_int v = Store::DirectWordToInt(Sel(index));
    Init(index, Store::IntToWord(v + value));
  }
public:
  using Tuple::ToWord;
  // ProfileEntry Accessors
  void AddHeap(u_int value) {
    Modify(NB_HEAP_POS, value);
  }
  void IncCalls() {
    Modify(NB_CALLS_POS, 1);
  }
  void IncClosures() {
    Modify(NB_CLOSURES_POS, 1);
  }
  void IncInstances() {
    Modify(NB_INSTANCES_POS, 1);
  }
  void IncInstrs() {
    Modify(NB_INSTRS_POS, 1);
  }
  // ProfileEntry Concstructor
  static ProfileEntry *New(String *name) {
    Tuple *entry = Tuple::New(SIZE);
    entry->Init(NAME_POS, name->ToWord());
    entry->Init(NB_CALLS_POS, Store::IntToWord(0));
    entry->Init(NB_HEAP_POS, Store::IntToWord(0));
    entry->Init(NB_CLOSURES_POS, Store::IntToWord(0));
    entry->Init(NB_INSTANCES_POS, Store::IntToWord(0));
    entry->Init(NB_INSTRS_POS, Store::IntToWord(0));
    return (ProfileEntry *) entry;
  }
  // ProfileEntry untagging
  static ProfileEntry *FromWordDirect(word x) {
    Tuple *entry = Tuple::FromWordDirect(x);
    entry->AssertWidth(SIZE);
    return (ProfileEntry *) entry;
  }
};

//
// Profiler Methods
//
word Profiler::table;
u_int Profiler::heapUsage;

void Profiler::Init() {
  table = BlockHashTable::New(256)->ToWord(); // to be done
  RootSet::Add(table);
}

ProfileEntry *Profiler::GetEntry(StackFrame *frame) {
  Worker *worker = frame->GetWorker();
  word key = worker->GetProfileKey(frame);
  BlockHashTable *t = BlockHashTable::FromWordDirect(table);
  if (t->IsMember(key))
    return ProfileEntry::FromWordDirect(t->GetItem(key));
  else {
    String *name = worker->GetProfileName(frame);
    ProfileEntry *entry = ProfileEntry::New(name);
    t->InsertItem(key, entry->ToWord());
    return entry;
  }
}

ProfileEntry *Profiler::GetEntry(ConcreteCode *concreteCode) {
  Worker *worker = concreteCode->GetWorker();
  word key = worker->GetProfileKey(concreteCode);
  BlockHashTable *t = BlockHashTable::FromWordDirect(table);
  if (t->IsMember(key))
    return ProfileEntry::FromWordDirect(t->GetItem(key));
  else {
    String *name = worker->GetProfileName(concreteCode);
    ProfileEntry *entry = ProfileEntry::New(name);
    t->InsertItem(key, entry->ToWord());
    return entry;
  }
}

ProfileEntry *Profiler::GetEntry(TagVal *template_) {
  BlockHashTable *t = BlockHashTable::FromWordDirect(table);
  word key          = template_->ToWord();
  if (t->IsMember(key))
    return ProfileEntry::FromWordDirect(t->GetItem(key));
  else {
    Tuple *coord = Tuple::FromWordDirect(template_->Sel(0));
    String *name = String::FromWordDirect(coord->Sel(0));
    char buf[1024]; // to be done
    std::sprintf(buf, "Alice template %.*s, line %d, column %d",
		 (int) name->GetSize(), name->GetValue(),
		 Store::DirectWordToInt(coord->Sel(1)),
		 Store::DirectWordToInt(coord->Sel(2)));
    ProfileEntry *entry = ProfileEntry::New(String::New(buf));
    t->InsertItem(key, entry->ToWord());
    return entry;
  }
}

u_int Profiler::GetHeapTotal() {
  u_int heapTotal = 0;
  Store::curChunk->SetTop(Store::chunkTop);
  for (u_int i = STORE_GENERATION_NUM - 1; i--;)
    heapTotal += Store::GetMemUsage(Store::roots[i]);
  return heapTotal;
}

void Profiler::SampleHeap() {
  heapUsage = GetHeapTotal();
}

void Profiler::AddHeap(StackFrame *frame) {
  u_int heapTotal     = GetHeapTotal();
  ProfileEntry *entry = GetEntry(frame);
  entry->AddHeap(heapTotal - heapUsage);
}

void Profiler::IncCalls(StackFrame *frame) {
  ProfileEntry *entry = GetEntry(frame);
  entry->IncCalls();
}

void Profiler::IncClosures(word cCode) {
  ConcreteCode *concreteCode = ConcreteCode::FromWord(cCode);
  if (concreteCode != INVALID_POINTER)
    GetEntry(concreteCode)->IncClosures();
}

void Profiler::IncInstances(TagVal *template_) {
  ProfileEntry *entry = GetEntry(template_);
  entry->IncInstances();
}

void Profiler::IncInstrs(word cCode) {
  ConcreteCode *concreteCode = ConcreteCode::FromWord(cCode);
  if (concreteCode != INVALID_POINTER)
    GetEntry(concreteCode)->IncInstrs();
}

static FILE *logFile;

static void PrintInfo(word /*key*/, word value) {
  Tuple *entry      = Tuple::FromWordDirect(value);
  String *name      = String::FromWordDirect(entry->Sel(0));
  u_int calls       = Store::DirectWordToInt(entry->Sel(1));
  u_int heap        = Store::DirectWordToInt(entry->Sel(2));
  u_int closures    = Store::DirectWordToInt(entry->Sel(3));
  u_int specialized = Store::DirectWordToInt(entry->Sel(4));
  u_int instrs      = Store::DirectWordToInt(entry->Sel(5));

  char *s = name->ExportC();
  for (char *t = s; *t; t++)
    if (*t == ',') *t = ';';
  std::fprintf(logFile, "%d, %d, %d, %d, %d, %.2f, %s\n",
	       calls, closures, heap, specialized, instrs,
	       calls? static_cast<float>(heap) / calls: 0.0,
	       s);
}

void Profiler::DumpInfo() {
  BlockHashTable *t = BlockHashTable::FromWordDirect(table);
  if ((logFile = std::fopen("profile_log.txt", "w")) == NULL)
    Error("Profiler:DumpInfo: unable to open log file");
  t->Apply((item_apply) PrintInfo);
  std::fclose(logFile);
}

#endif
