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
#include "store/Map.hh"
#include "generic/Profiler.hh"
#include "generic/RootSet.hh"
#include "generic/Worker.hh"
#include "generic/StackFrame.hh"
#include "generic/String.hh"
#include "generic/ConcreteCode.hh"
#include "generic/Tuple.hh"

class ProfileEntry : private Tuple {
protected:
  enum {
    NAME_POS, NB_CALLS_POS, NB_HEAP_POS, NB_CLOSURES_POS,
    NB_INSTRS_POS, NB_RUNS_POS, SIZE
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
  void IncInstrs() {
    Modify(NB_INSTRS_POS, 1);
  }
  void IncRuns() {
    Modify(NB_RUNS_POS, 1);
  }
  // ProfileEntry Concstructor
  static ProfileEntry *New(String *name) {
    Tuple *entry = Tuple::New(SIZE);
    entry->Init(NAME_POS, name->ToWord());
    entry->Init(NB_CALLS_POS, Store::IntToWord(0));
    entry->Init(NB_HEAP_POS, Store::IntToWord(0));
    entry->Init(NB_CLOSURES_POS, Store::IntToWord(0));
    entry->Init(NB_INSTRS_POS, Store::IntToWord(0));
    entry->Init(NB_RUNS_POS, Store::IntToWord(0));
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
word Profiler::sampleKey;
String *Profiler::sampleName;

void Profiler::Init() {
  table = Map::New(256)->ToWord(); // to be done
  RootSet::Add(table);
}

ProfileEntry *Profiler::GetEntry(word key, String *name) {
  Map *t = Map::FromWordDirect(table);
  if (t->IsMember(key))
    return ProfileEntry::FromWordDirect(t->Get(key));
  else {
    ProfileEntry *entry = ProfileEntry::New(name);
    t->Put(key, entry->ToWord());
    return entry;
  }
}

ProfileEntry *Profiler::GetEntry(StackFrame *frame) {
  Worker *worker = frame->GetWorker();
  word key = worker->GetProfileKey(frame);
  Map *t = Map::FromWordDirect(table);
  if (t->IsMember(key))
    return ProfileEntry::FromWordDirect(t->Get(key));
  else {
    String *name = worker->GetProfileName(frame);
    ProfileEntry *entry = ProfileEntry::New(name);
    t->Put(key, entry->ToWord());
    return entry;
  }
}

ProfileEntry *Profiler::GetEntry(ConcreteCode *concreteCode) {
  Interpreter *interpreter = concreteCode->GetInterpreter();
  word key = interpreter->GetProfileKey(concreteCode);
  Map *t = Map::FromWordDirect(table);
  if (t->IsMember(key))
    return ProfileEntry::FromWordDirect(t->Get(key));
  else {
    String *name = interpreter->GetProfileName(concreteCode);
    ProfileEntry *entry = ProfileEntry::New(name);
    t->Put(key, entry->ToWord());
    return entry;
  }
}

u_int Profiler::GetHeapTotal() {
  u_int heapTotal = 0;
  for (u_int i = STORE_GENERATION_NUM - 1; i--;)
    heapTotal += Store::roots[i].GetExactSize();
  return heapTotal;
}

void Profiler::SampleHeap(StackFrame *frame) {
  Worker *worker = frame->GetWorker();
  sampleKey = worker->GetProfileKey(frame);
  Map *t = Map::FromWordDirect(table);
  if (!t->IsMember(sampleKey))
    sampleName = worker->GetProfileName(frame);
  heapUsage = GetHeapTotal();
}

void Profiler::AddHeap() {
  u_int heapTotal     = GetHeapTotal();
  ProfileEntry *entry = GetEntry(sampleKey, sampleName);
  entry->AddHeap(heapTotal - heapUsage);
  entry->IncRuns();
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

void Profiler::IncInstrs(word cCode) {
  ConcreteCode *concreteCode = ConcreteCode::FromWord(cCode);
  if (concreteCode != INVALID_POINTER)
    GetEntry(concreteCode)->IncInstrs();
}

static FILE *logFile;

static void PrintInfo(word /*key*/, word value) {
  Tuple *entry   = Tuple::FromWordDirect(value);
  String *name   = String::FromWordDirect(entry->Sel(0));
  u_int calls    = Store::DirectWordToInt(entry->Sel(1));
  u_int heap     = Store::DirectWordToInt(entry->Sel(2));
  u_int closures = Store::DirectWordToInt(entry->Sel(3));
  u_int instrs   = Store::DirectWordToInt(entry->Sel(4));
  u_int runs     = Store::DirectWordToInt(entry->Sel(5));

  char *s = name->ExportC();
  for (char *t = s; *t; t++)
    if (*t == ',') *t = ';';
  std::fprintf(logFile, "%d, %d, %d, %d, %d, %.2f, %s\n",
	       runs, calls, closures, heap, instrs,
	       calls? static_cast<float>(heap) / calls: 0.0,
	       s);
}

void Profiler::DumpInfo() {
  Map *t = Map::FromWordDirect(table);
  if ((logFile = std::fopen("profile_log.txt", "w")) == NULL)
    Error("Profiler:DumpInfo: unable to open log file");
  t->Apply((item_apply) PrintInfo);
  std::fclose(logFile);
}

#endif
