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
#pragma implementation "alice/NativeCodeJitter.hh"
#pragma implementation "alice/NativeConcreteCode.hh"
#pragma implementation "generic/JitterGenericData.hh"
#pragma implementation "alice/JitterAliceData.hh"
#pragma implementation "alice/JitterImmediateEnv.hh"
#pragma implementation "alice/LivenessInformation.hh"
#endif

#include <cstdio>
#include "adt/IntMap.hh"
#include "adt/ChunkMap.hh"
#include "generic/Scheduler.hh"
#include "generic/Backtrace.hh"
#include "generic/Closure.hh"
#include "generic/ConcreteCode.hh"
#include "generic/Transients.hh"
#include "generic/Transform.hh"
#include "generic/Primitive.hh"
#include "generic/RootSet.hh"
#include "alice/PrimitiveTable.hh"
#include "alice/Data.hh"
#include "alice/AbstractCode.hh"
#include "alice/LazySelInterpreter.hh"
#include "alice/NativeCodeInterpreter.hh"
#include "alice/NativeCodeJitter.hh"
#include "alice/LivenessInformation.hh"

#include "generic/JitterGenericData.hh"
#include "alice/JitterAliceData.hh"
#include "alice/JitterImmediateEnv.hh"
#include "alice/AliceLanguageLayer.hh"
#include "alice/Types.hh"

#if PROFILE
#include "generic/Profiler.hh"
#endif

static inline u_int GetArity(TagVal *args) {
  switch (AbstractCode::GetArgs(args)) {
  case AbstractCode::OneArg:
    return Scheduler::ONE_ARG;
  case AbstractCode::TupArgs:
    return Vector::FromWordDirect(args->Sel(0))->GetLength();
  default:
    Error("invalid args tag");
  }
}

// to be used by GetActualIdRefs only
static word actualIdRefVector;

static inline Vector *GetActualIdRefs(TagVal *actualArgs) {
  switch (AbstractCode::GetArgs(actualArgs)) {
  case AbstractCode::OneArg:
    {
      Vector *actualIdRefs = Vector::FromWordDirect(actualIdRefVector);
      actualIdRefs->Init(0, actualArgs->Sel(0));
      return actualIdRefs;
    }
  case AbstractCode::TupArgs:
    return Vector::FromWordDirect(actualArgs->Sel(0));
  default:
    Error("invalid args tag");
  }
}

namespace Outline {
  namespace Backtrace {
    static ::Backtrace *New(::StackFrame *frame) {
      word wFrame = frame->Clone();
      return ::Backtrace::New(wFrame);
    }
  };
  namespace Constructor {
    static ::Constructor *New(String *name) {
      return ::Constructor::New(name);
    }
  };
  namespace Record {
    static word PolySel(::Record *record, UniqueString *label) {
      Assert((::Record::FromWordDirect(record->ToWord()), true));
      return record->PolySel(label);
    }
  };
};

// NativeCodeFrame
// to be done: inherit from real NativeCodeFrame
class NativeCodeFrame : private Generic::StackFrame {
protected:
  enum {
    PC_POS, CODE_POS, CLOSURE_POS, IMMEDIATE_ARGS_POS,
    CONTINUATION_POS, BASE_SIZE
  };
public:
  // Side-Effect: Scratches JIT_R0, JIT_FP
  static void New(u_int This, u_int nLocals) {
    Generic::StackFrame::New(This, BASE_SIZE + nLocals,
			     NativeCodeInterpreter::self);
  }
  static u_int GetFrameSize(u_int nLocals) {
    return (Generic::StackFrame::BASE_SIZE + BASE_SIZE + nLocals);
  }
  static void GetImmediateArgs(u_int Dest, u_int This) {
    Sel(Dest, This, IMMEDIATE_ARGS_POS);
  }
  static void GetSize(u_int Dest, u_int This) {
    GetImmediateArgs(Dest, This);
    Generic::Tuple::Sel(Dest, Dest, 0);
  }
  static void GetPC(u_int Dest, u_int This) {
    Sel(Dest, This, PC_POS);
  }
  static void PutPC(u_int This, u_int Value) {
    Put(This, PC_POS, Value);
  }
  static void GetCode(u_int Dest, u_int This) {
    Sel(Dest, This, CODE_POS);
  }
  static void PutCode(u_int This, u_int Value) {
    Put(This, CODE_POS, Value);
  }
  static void GetClosure(u_int Dest, u_int This) {
    Sel(Dest, This, CLOSURE_POS);
  }
  static void PutClosure(u_int This, u_int Value) {
    Put(This, CLOSURE_POS, Value);
  }
  static void PutImmediateArgs(u_int This, u_int Value) {
    Put(This, IMMEDIATE_ARGS_POS, Value);
  }
  static void GetContinuation(u_int Dest, u_int This) {
    Sel(Dest, This, CONTINUATION_POS);
  }
  static void PutContinuation(u_int This, u_int Value) {
    Put(This, CONTINUATION_POS, Value);
  }
  static void ReplaceClosure(u_int This, u_int Closure) {
    Replace(This, CLOSURE_POS, Closure);
  }
  static void GetEnv(u_int Dest, u_int This, u_int pos) {
    Sel(Dest, This, BASE_SIZE + pos);
  }
  static void PutEnv(u_int This, u_int pos, u_int Value) {
    Put(This, BASE_SIZE + pos, Value);
  }
  static void ReplaceEnv(u_int This, u_int pos, u_int Value) {
    Replace(This, BASE_SIZE + pos, Value);
  }
};

//
// Register Allocation
//
class RegisterNode {
public:
  u_int index;
  u_int reg;
  u_int expiration;
  RegisterNode *next;

  RegisterNode(u_int r) {
    reg = r;
  }
  void Init(u_int i, u_int e) {
    index       = i;
    expiration  = e;
    next        = NULL;
  }
};

#if defined(JIT_STORE_DEBUG)
static u_int max(u_int a, u_int b) {
  return ((a >= b) ? a : b);
}
#endif

class RegisterBank {
protected:
  static RegisterNode *regs[ALICE_REGISTER_NB + 1];
  static u_int regVals[ALICE_REGISTER_NB];
  static u_int top;
public:
  // RegisterBank Static Constructor
  static void Init() {
    regs[1] = new RegisterNode(0);
    regs[2] = new RegisterNode(1);
    regs[3] = new RegisterNode(2);
    // Assign register to indices
    regVals[0] = JIT_R1;
    regVals[1] = JIT_R2;
    regVals[2] = JIT_V0;
    top        = ALICE_REGISTER_NB;
  }
  static bool HaveRegister() {
    return (top != 0);
  }
  static RegisterNode *Alloc() {
    return regs[top--];
  }
  static void Free(RegisterNode *node) {
    regs[++top] = node;
  }
  static u_int IndexToRegister(u_int i) {
    Assert(i < ALICE_REGISTER_NB);
    return regVals[i];
  }
};

RegisterNode *RegisterBank::regs[ALICE_REGISTER_NB + 1];
u_int RegisterBank::regVals[ALICE_REGISTER_NB];
u_int RegisterBank::top;

class MemoryNode : public Tuple {
protected:
  enum { INDEX_POS, END_POS, NEXT_POS, SIZE };

  static u_int index;
public:
  static void Reset() {
    index = ALICE_REGISTER_NB;
  }
  static u_int GetNbSlots() {
    return index;
  }
  u_int GetIndex() {
    return Store::DirectWordToInt(Sel(INDEX_POS));
  }
  u_int GetExpiration() {
    return Store::DirectWordToInt(Sel(END_POS));
  }
  void SetExpiration(u_int expiration) {
    Init(END_POS, Store::IntToWord(expiration));
    Init(NEXT_POS, Store::UnmanagedPointerToWord(INVALID_POINTER));
  }
  MemoryNode *GetNext() {
    return (MemoryNode *) Store::DirectWordToUnmanagedPointer(Sel(NEXT_POS));
  }
  void SetNext(MemoryNode *next) {
    Init(NEXT_POS, Store::UnmanagedPointerToWord(next));
  }
  // MemoryNode Constructor
  static MemoryNode *New() {
    Tuple *node = Tuple::New(SIZE);
    node->Init(INDEX_POS, Store::IntToWord(index++));
    node->Init(NEXT_POS, Store::UnmanagedPointerToWord(INVALID_POINTER));
    return (MemoryNode *) node;
  }
  // MemoryNode Untagging
  static MemoryNode *FromWordDirect(word x) {
    return (MemoryNode *) Tuple::FromWordDirect(x);
  }
};

u_int MemoryNode::index;

class MemorySlots {
protected:
  static MemoryNode *nodes;
public:
  static void Reset() {
    MemoryNode::Reset();
    nodes = INVALID_POINTER;
  }
  static MemoryNode *Alloc() {
    if (nodes != INVALID_POINTER) {
      MemoryNode *node = nodes;
      nodes = nodes->GetNext();
      return node;
    }
    else
      return MemoryNode::New();
  }
  static void Free(MemoryNode *node) {
    node->SetNext(nodes);
    nodes = node;
  }
};

MemoryNode *MemorySlots::nodes;

class ActiveSet {
protected:
  static RegisterNode *regActive;
  static MemoryNode *memActive;
public:
  // ActiveSet Static Constructor
  static void Init() {
    RegisterBank::Init();
    MemorySlots::Reset();
    regActive = NULL;
  }
  static void Reset() {
    while (regActive != NULL) {
      RegisterNode *next = regActive->next;
      RegisterBank::Free(regActive);
      regActive = next;
    }
    MemorySlots::Reset();
    memActive = INVALID_POINTER;
  }
  static void ExpireRegs(u_int i) {
    RegisterNode *node = regActive;
    RegisterNode *prev = NULL;
    while (node != NULL) {
      RegisterNode *next = node->next;
      if (node->expiration < i) {
	RegisterBank::Free(node);
	if (prev != NULL)
	  prev->next = next;
	else
	  regActive = next;
      }
      else
	prev = node;
      node = next;
    }
  }
  static void ExpireSlots(u_int i) {
    MemoryNode *node = memActive;
    MemoryNode *prev = INVALID_POINTER;
    while (node != INVALID_POINTER) {
      MemoryNode *next = node->GetNext();
      if (node->GetExpiration() < i) {
	MemorySlots::Free(node);
	if (prev != INVALID_POINTER)
	  prev->SetNext(next);
	else
	  memActive = next;
      }
      else
	prev = node;
      node = next;
    }
  }
  static u_int Add(u_int index, u_int expiration) {
    RegisterNode *node = RegisterBank::Alloc();
    node->Init(index, expiration);
    if (regActive == NULL)
      regActive = node;
    else {
      RegisterNode *nodes = regActive;
      RegisterNode *prev  = NULL;
      while ((nodes != NULL) && (nodes->expiration >= expiration)) {
	prev  = nodes;
	nodes = nodes->next;
      }
      if (prev == NULL) {
	node->next = regActive;
	regActive  = node;
      }
      else {
	node->next = nodes;
	prev->next = node;
      }
    }
    return node->reg;
  }
  static u_int AddMem(u_int expiration) {
    MemoryNode *node = MemorySlots::Alloc();
    node->SetExpiration(expiration);
    if (memActive == INVALID_POINTER)
      memActive = node;
    else {
      node->SetNext(memActive);
      memActive = node;
    }
    return node->GetIndex();
  }
  static RegisterNode *Spill() {
    return regActive;
  }
  static void Remove(RegisterNode *node) {
    regActive = regActive->next;
    RegisterBank::Free(node);
  }
};

RegisterNode *ActiveSet::regActive;
MemoryNode *ActiveSet::memActive;

class RegisterAllocator {
protected:
  static  word NewReg(u_int reg) {
    return Store::IntToWord(reg);
  }
  static word NewMem(u_int expiration) {
    return Store::IntToWord(ActiveSet::AddMem(expiration));
  }
public:
  static Tuple *Run(u_int nLocals, Vector *liveness) {
    Tuple *assignment = Tuple::New(nLocals);
    u_int size        = liveness->GetLength();
    ActiveSet::Reset();
    for (u_int i = 0; i < size; i += 3) {
      u_int curIndex = Store::DirectWordToInt(liveness->Sub(i));
      u_int curStart = Store::DirectWordToInt(liveness->Sub(i + 1));
      u_int curEnd   = Store::DirectWordToInt(liveness->Sub(i + 2));
      ActiveSet::ExpireRegs(curStart);
      //ActiveSet::ExpireSlots(curStart);
      if (RegisterBank::HaveRegister()) {
	u_int reg = ActiveSet::Add(curIndex, curEnd);
	assignment->Init(curIndex, NewReg(reg));
      }
      else {
	RegisterNode *node = ActiveSet::Spill();
	if (node->expiration > curEnd) {
	  u_int nodeIndex      = node->index;
	  u_int nodeExpiration = node->expiration;
	  ActiveSet::Remove(node);
	  assignment->Init(nodeIndex, NewMem(nodeExpiration));
	  u_int reg = ActiveSet::Add(curIndex, curEnd);
	  assignment->Init(curIndex, NewReg(reg));
	}
	else
	  assignment->Init(curIndex, NewMem(curEnd));
      }
    }
    return assignment;
  }
#if defined(JIT_STORE_DEBUG)
  static void Dump(word wCoord, u_int nLocals, Tuple *assignment) {
    u_int mem   = 0;
    u_int reg   = 0;
    u_int slots = ALICE_REGISTER_NB;
    u_int maxVal = nLocals + ALICE_REGISTER_NB;
    for (u_int i = nLocals; i--;) {
      u_int val = Store::DirectWordToInt(assignment->Sel(i));
      if (val < ALICE_REGISTER_NB) {
	fprintf(stderr, "%d: r %d\n", i, val);
	reg++;
      }
      else if (val < maxVal) {
	slots = max(slots, val);
	mem++;
	fprintf(stderr, "%d: l %d\n", i, val - ALICE_REGISTER_NB);
      }
      else {
	fprintf(stderr, "Dump: index %d is has not been assigned\n", i);
      }
    }
    if (mem > 0)
      slots++;
    if (MemoryNode::GetNbSlots() != slots) {
      fprintf(stderr, "Slot assignment failed: %d != %d\n",
	      MemoryNode::GetNbSlots(), slots);
    }
    Assert(MemoryNode::GetNbSlots() == slots);
    Tuple *coord = Tuple::FromWord(wCoord);
    String *name = String::FromWord(coord->Sel(0));
    u_int line   = Store::WordToInt(coord->Sel(1));
    fprintf(stderr, "%d %d %d %d %s:%d:\n",
    nLocals, reg, mem, MemoryNode::GetNbSlots(), name->ExportC(), line);
  }
#endif
};

// LivenessTable
class LivenessTable : private Chunk {
protected:
  enum { DEAD, ALIVE, KILL };

  void Set(u_int i, char v) {
    Assert(i < this->GetSize());
    char *p = this->GetBase();
    p[i] = v;
  }
  char Get(u_int i) {
    Assert(i < this->GetSize());
    char *p = this->GetBase();
    return p[i];
  }
public:
  using Chunk::GetSize;
  using Chunk::ToWord;
  // LivenessTable Accessors
  void SetAlive(u_int i) {
    Set(i, ALIVE);
  }
  void SetDead(u_int i) {
    Set(i, DEAD);
  }
  void SetKill(u_int i) {
    if (Get(i) == ALIVE)
      Set(i, KILL);
  }
  bool NeedsKill(u_int i) {
    return (Get(i) == KILL);
//      u_int status = Get(i);
//      return ((status == KILL) || (status == DEAD));
  }
  LivenessTable *Clone() {
    u_int size   = this->GetSize();
    Chunk *clone = Store::AllocChunk(size);
    std::memcpy(clone->GetBase(), this->GetBase(), size);
    return (LivenessTable *) clone;
  }
  void Clone(LivenessTable *clone) {
    //--** why break abstraction barriers?
    u_int size = ((::Block *) this)->GetSize();
    memcpy(clone, this,  (size + 1) * sizeof(word));
  }
  // LivenessTable Constructor
  static LivenessTable *New(u_int size) {
    Chunk *table = Store::AllocChunk(size);
    std::memset(table->GetBase(), 0, size);
    return (LivenessTable *) table;
  }
  // LivenessTable Untagging
  static LivenessTable *FromWordDirect(word table) {
    return (LivenessTable *) Store::DirectWordToChunk(table);
  }
};

//
// ImmediateEnv Variables
//
u_int ImmediateEnv::index;
u_int ImmediateEnv::size;
Tuple *ImmediateEnv::values;

//
// NativeCodeJitter Debug Stuff
//
#if defined(JIT_STORE_DEBUG)

#define JIT_LOG_REG(reg) \
  JITStore::LogReg(reg);
#define JIT_LOG_MESG(mesg) \
  JITStore::LogMesg(mesg);

#define JIT_PRINT_PC(instr) \
  JITStore::LogMesg(instr); \
  JITStore::LogReg(JIT_SP);

#else

#define JIT_LOG_REG(reg)
#define JIT_LOG_MESG(mesg)
#define JIT_PRINT_PC(instr)

#endif

//
// NativeCodeJitter Variables
//
jit_insn *NativeCodeJitter::codeBuffer;
u_int NativeCodeJitter::codeBufferSize;
 
word NativeCodeJitter::initialPC;
word NativeCodeJitter::initialNoCCCPC;
word NativeCodeJitter::instructionStartPC;
IntMap *NativeCodeJitter::sharedTable;

LivenessTable *NativeCodeJitter::livenessTable;
LivenessTable *NativeCodeJitter::livenessFreeList;
Tuple *NativeCodeJitter::assignment;
Vector *NativeCodeJitter::globalSubst;

word NativeCodeJitter::inlineTable;
u_int NativeCodeJitter::currentNLocals;
TagVal *NativeCodeJitter::currentArgs;
word NativeCodeJitter::currentConcreteCode;
//
// Environment Accessors
//
u_int NativeCodeJitter::RefToIndex(word ref) {
  u_int n     = Store::WordToInt(ref);
  u_int index = Store::DirectWordToInt(assignment->Sel(n));
  return index;
}

u_int NativeCodeJitter::LocalEnvSel(u_int Dest, u_int Ptr, u_int pos) {
  if (pos >= ALICE_REGISTER_NB) {
    NativeCodeFrame::GetEnv(Dest, Ptr, pos);
    return Dest;
  }
  else {
    return RegisterBank::IndexToRegister(pos);
  }
}

void NativeCodeJitter::LocalEnvPut(u_int Ptr, word pos, u_int Value) {
  u_int index = RefToIndex(pos);
  livenessTable->SetAlive(index);
  if (index >= ALICE_REGISTER_NB) {
    NativeCodeFrame::ReplaceEnv(Ptr, index, Value);
  }
  else {
    jit_movr_p(RegisterBank::IndexToRegister(index), Value);
  }
}

void NativeCodeJitter::KillIdRef(word idRef) {
  TagVal *tagVal = TagVal::FromWordDirect(idRef);
  if (AbstractCode::GetIdRef(tagVal) == AbstractCode::LastUseLocal) {
    u_int index = RefToIndex(tagVal->Sel(0));
    livenessTable->SetKill(index);
  }
}

void NativeCodeJitter::GlobalEnvSel(u_int Dest, u_int Ptr, word pos) {
  Assert(Dest != Ptr);
  Assert(Dest != JIT_FP); // We do not have segment override prefix
  NativeCodeFrame::GetClosure(Dest, Ptr);
  Generic::Closure::Sel(Dest, Dest, Store::WordToInt(pos));
}

void NativeCodeJitter::ImmediateSel(u_int Dest, u_int Ptr, u_int pos) {
  Assert(Dest != Ptr);
  Assert(Dest != JIT_FP); // We do not have segment override prefix
  NativeCodeFrame::GetImmediateArgs(Dest, Ptr);
  JITStore::GetArg(Dest, Dest, pos);
}

TagVal *NativeCodeJitter::LookupSubst(u_int index) {
  return TagVal::FromWordDirect(globalSubst->Sub(index));
}

// LazySelClosure (belongs to alice, of course)
void NativeCodeJitter::LazySelClosureNew(u_int Record, UniqueString *label) {
  jit_pushr_ui(Record); // Save Record
  Generic::ConcreteCode::New(JIT_V1, LazySelInterpreter::self, 0);
  jit_pushr_ui(JIT_V1); // Save ConcreteCode Ptr
  Generic::Closure::New(JIT_V1, LazySelClosure::SIZE);
  jit_popr_ui(JIT_R0); // Restore ConcreteCode Ptr
  Generic::Closure::InitConcreteCode(JIT_V1, JIT_R0);
  jit_popr_ui(JIT_R0); // Restore Record
  Generic::Closure::Put(JIT_V1, LazySelClosure::RECORD_POS, JIT_R0);
  u_int labelIndex = ImmediateEnv::Register(label->ToWord());
  ImmediateSel(JIT_R0, JIT_V2, labelIndex);
  Generic::Closure::Put(JIT_V1, LazySelClosure::LABEL_POS, JIT_R0);
}

#define RETURN() \
  JIT_LOG_REG(JIT_SP); \
  JIT_LOG_MESG("returning to base\n"); \
  jit_ret();

void NativeCodeJitter::ResetRegister() {
  word empty = Store::IntToWord(0);
  jit_movi_p(JIT_R1, empty);
  jit_movi_p(JIT_R2, empty);
  jit_movi_p(JIT_V0, empty);
}

void NativeCodeJitter::SaveRegister() {
  NativeCodeFrame::ReplaceEnv(JIT_V2, 0, JIT_R1);
  NativeCodeFrame::ReplaceEnv(JIT_V2, 1, JIT_R2);
  NativeCodeFrame::ReplaceEnv(JIT_V2, 2, JIT_V0);
}

void NativeCodeJitter::RestoreRegister() {
  NativeCodeFrame::GetEnv(JIT_R1, JIT_V2, 0);
  NativeCodeFrame::GetEnv(JIT_R2, JIT_V2, 1);
  NativeCodeFrame::GetEnv(JIT_V0, JIT_V2, 2);
}

void NativeCodeJitter::PushCall(CallInfo *info) {
#if PROFILE
  if (info->mode != NORMAL_CALL) {
    ImmediateSel(JIT_R0, JIT_V2, info->closure);
    jit_pushr_ui(JIT_R0); // Closure
  }
  JITStore::Call(1, (void *) Scheduler::PushCall);
  RETURN();
#else
  switch (info->mode) {
  case NATIVE_CALL:
    {
      ImmediateSel(JIT_R0, JIT_V2, info->closure);
      jit_pushr_ui(JIT_R0); // Closure
      jit_ldi_p(JIT_R0, &NativeCodeInterpreter::nativeContinuation);
      jit_pushr_ui(JIT_R0);  // Continuation
      JITStore::Call(2, (void *) NativeCodeInterpreter::FastPushCall);
      jit_movr_p(JIT_V2, JIT_R0); // Move to new frame
      CheckPreempt(info->pc);
      ResetRegister();
      NativeCodeFrame::GetCode(JIT_R0, JIT_V2);
      jit_addi_p(JIT_R0, JIT_R0, info->pc);
      jit_jmpr(JIT_R0);
    }
    break;
  case SELF_CALL:
    {
      NativeCodeFrame::New(JIT_V1, currentNLocals);
      ImmediateSel(JIT_R0, JIT_V2, info->closure); // Closure
      NativeCodeFrame::PutClosure(JIT_V1, JIT_R0);
      NativeCodeFrame::GetImmediateArgs(JIT_R0, JIT_V2);
      NativeCodeFrame::PutImmediateArgs(JIT_V1, JIT_R0);
      jit_ldi_p(JIT_R0, &NativeCodeInterpreter::nativeContinuation);
      NativeCodeFrame::PutContinuation(JIT_V1, JIT_R0);
      NativeCodeFrame::GetCode(JIT_R0, JIT_V2);
      NativeCodeFrame::PutCode(JIT_V1, JIT_R0);
      jit_movr_p(JIT_V2, JIT_V1); // Move to new frame
      CheckPreempt(info->pc);
      NativeCodeFrame::GetCode(JIT_R0, JIT_V2);
      jit_addi_p(JIT_R0, JIT_R0, info->pc);
      jit_jmpr(JIT_R0);
    }
    break;
  case NORMAL_CALL:
    {
      // Invariant: Closure is on the stack
      JITStore::Call(1, (void *) Scheduler::PushCall);
      RETURN();
    }
    break;
  default:
    Error("NativeCodeJitter::PushCall: invalid call info");
  }
#endif
}

void NativeCodeJitter::DirectCall(Interpreter *interpreter) {
  JIT_LOG_MESG("DirectCall\n");
  JIT_LOG_MESG(interpreter->Identify());
  jit_movi_p(JIT_R0, interpreter);
  jit_pushr_ui(JIT_R0);
  JITStore::Call(1, (void *) Primitive::Execute);
  RETURN();
}

void NativeCodeJitter::TailCall(CallInfo *info) {
#if PROFILE
  if (info->mode != NORMAL_CALL) {
    ImmediateSel(JIT_R0, JIT_V2, info->closure);
    jit_pushr_ui(JIT_R0);
  }
  Generic::Scheduler::PopFrame();
  JITStore::Call(1, (void *) ::Scheduler::PushCall);
  RETURN();
#else
  switch (info->mode) {
  case NATIVE_CALL:
    {
      ImmediateSel(JIT_R0, JIT_V2, info->closure);
      jit_pushr_ui(JIT_R0); // Closure
      NativeCodeFrame::GetContinuation(JIT_R0, JIT_V2); // Reuse continuation
      jit_pushr_ui(JIT_R0); // Continuation
      u_int size = NativeCodeFrame::GetFrameSize(currentNLocals);
      Generic::Scheduler::PopFrame(size);
      JITStore::Call(2, (void *) NativeCodeInterpreter::FastPushCall);
      jit_movr_p(JIT_V2, JIT_R0); // Move to new Frame
      CheckPreempt(info->pc);
      ResetRegister();
      NativeCodeFrame::GetCode(JIT_R0, JIT_V2);
      jit_addi_p(JIT_R0, JIT_R0, info->pc);
      jit_jmpr(JIT_R0);
    }
    break;
  case SELF_CALL:
    {
      ImmediateSel(JIT_R0, JIT_V2, info->closure);
      NativeCodeFrame::ReplaceClosure(JIT_V2, JIT_R0);
      CheckPreempt(info->pc);
      NativeCodeFrame::GetCode(JIT_R0, JIT_V2);
      jit_addi_p(JIT_R0, JIT_R0, info->pc);
      jit_jmpr(JIT_R0);
    }
    break;
  case NORMAL_CALL:
    {
      // Invariant: Closure is on the stack
      u_int size = NativeCodeFrame::GetFrameSize(currentNLocals);
      Generic::Scheduler::PopFrame(size);
      JITStore::Call(1, (void *) Scheduler::PushCall);
      RETURN();
    }
    break;
  default:
    Error("NativeCodeJitter::TailCall: illegal call info");
  }
#endif
}

void NativeCodeJitter::BranchToOffset(u_int wOffset) {
  Assert(wOffset == JIT_R0);
  JITStore::DirectWordToInt(wOffset, wOffset);
  NativeCodeFrame::GetCode(JIT_FP, JIT_V2);
  jit_addr_ui(wOffset, wOffset, JIT_FP);
  jit_jmpr(wOffset);
}

u_int NativeCodeJitter::GetRelativePC() {
  return ((jit_get_ip().ptr - (char *) codeBuffer) + 2 * sizeof(word));
}

void NativeCodeJitter::SetRelativePC(word pc) {
  jit_movi_p(JIT_R0, pc);
  NativeCodeFrame::PutPC(JIT_V2, JIT_R0);
}

//
// Calling Convention Conversion
//
void NativeCodeJitter::CompileCCC(u_int calleeArity, bool update) {
  switch (calleeArity) {
  case Scheduler::ONE_ARG:
    {
      JIT_LOG_MESG("Worker::Construct\n");
      JITStore::Prepare();
      JITStore::Call(0, (void *) Worker::Construct);
      JITStore::Finish();
      if (update)
	initialNoCCCPC = Store::IntToWord(GetRelativePC());
    }
    break;
  case 0:
    {
      if (update)
	initialNoCCCPC = initialPC;
    }
    break;
  default:
    {
      JIT_LOG_MESG("Deconstruct results\n");
      JITStore::Prepare();
      JITStore::Call(0, (void *) Worker::Deconstruct);
      JITStore::Finish();
      JIT_LOG_REG(JIT_RET);
      jit_insn *no_request = jit_beqi_ui(jit_forward(), JIT_R0, 0);
      jit_movi_ui(JIT_RET, Worker::REQUEST);
      RETURN();
      jit_patch(no_request);
      if (update)
	initialNoCCCPC = Store::IntToWord(GetRelativePC());
    }
    break;
  }
}

void NativeCodeJitter::StoreResults(u_int calleeArity, TagVal *idDefArgs) {
  switch (calleeArity) {
  case Scheduler::ONE_ARG:
    {
      if (idDefArgs != INVALID_POINTER) {
	TagVal *idDef = TagVal::FromWord(idDefArgs->Sel(0));
	if (idDef != INVALID_POINTER) {
	  Generic::Scheduler::GetZeroArg(JIT_R0);
	  LocalEnvPut(JIT_V2, idDef->Sel(0), JIT_R0);
	}
      }
    }
    break;
  case 0:
    // Request unit to be done
    break;
  default:
    {
      Vector *idDefs = Vector::FromWord(idDefArgs->Sel(0));
      Generic::Scheduler::GetCurrentArgs(JIT_V1);
      for (u_int i = calleeArity; i--;) {
	TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
	if (idDef != INVALID_POINTER) {
	  Generic::Scheduler::SelArg(JIT_R0, JIT_V1, i);
	  LocalEnvPut(JIT_V2, idDef->Sel(0), JIT_R0);
	}
      }
    }
    break;
  }
}

//
// Instruction Helper
//
u_int NativeCodeJitter::LoadIdRefKill(u_int Dest, word idRef) {
  TagVal *tagVal = TagVal::FromWord(idRef);
  if (AbstractCode::GetIdRef(tagVal) == AbstractCode::Global)
    tagVal = LookupSubst(Store::DirectWordToInt(tagVal->Sel(0)));
  switch (AbstractCode::GetIdRef(tagVal)) {
  case AbstractCode::Local:
    Dest = LocalEnvSel(Dest, JIT_V2, RefToIndex(tagVal->Sel(0))); break;
  case AbstractCode::LastUseLocal:
    {
      u_int index = RefToIndex(tagVal->Sel(0));
      Dest = LocalEnvSel(Dest, JIT_V2, index);
      livenessTable->SetKill(index);
    }
    break;
  case AbstractCode::Global:
    GlobalEnvSel(Dest, JIT_V2, tagVal->Sel(0)); break;
  case AbstractCode::Immediate:
    {
      word val = tagVal->Sel(0);
      if (PointerOp::IsInt(val)) {
	jit_movi_p(Dest, val);
      }
      else
	ImmediateSel(Dest, JIT_V2, ImmediateEnv::Register(val));
    }
    break;
  default:
    Error("NativeCodeJitter::LoadIdRef: invalid idRef Tag");
  }
  return Dest;
}

void NativeCodeJitter::Await(u_int Ptr, word pc) {
  jit_insn *ref[2];
  JITStore::DerefItem(Ptr, ref);
  jit_patch(ref[0]);
  BlockOnTransient(Ptr, pc);
  jit_patch(ref[1]);
}

u_int NativeCodeJitter::LoadIdRef(u_int Dest, word idRef, word pc) {
  TagVal *tagVal = TagVal::FromWord(idRef);
  if (AbstractCode::GetIdRef(tagVal) == AbstractCode::Global)
    tagVal = LookupSubst(Store::DirectWordToInt(tagVal->Sel(0)));
  switch (AbstractCode::GetIdRef(tagVal)) {
  case AbstractCode::Local:
  case AbstractCode::LastUseLocal:
    Dest = LocalEnvSel(Dest, JIT_V2, RefToIndex(tagVal->Sel(0))); break;
  case AbstractCode::Global:
    GlobalEnvSel(Dest, JIT_V2, tagVal->Sel(0)); break;
  case AbstractCode::Immediate:
    {
      word val = tagVal->Sel(0);
      if (PointerOp::IsInt(val)) {
	jit_movi_p(Dest, val);
	return Dest;
      }
      else
	ImmediateSel(Dest, JIT_V2, ImmediateEnv::Register(val));
    }
    break;
  default:
    Error("NativeCodeJitter::LoadIdRef: invalid idRef Tag");
  }
  if (pc != (word) 0)
    Await(Dest, pc);
  return Dest;
}

u_int NativeCodeJitter::ReloadIdRef(u_int Dest, word idRef) {
  TagVal *tagVal = TagVal::FromWord(idRef);
  if (AbstractCode::GetIdRef(tagVal) == AbstractCode::Global)
    tagVal = LookupSubst(Store::DirectWordToInt(tagVal->Sel(0)));
  switch (AbstractCode::GetIdRef(tagVal)) {
  case AbstractCode::Local:
  case AbstractCode::LastUseLocal:
    Dest = LocalEnvSel(Dest, JIT_V2, RefToIndex(tagVal->Sel(0))); break;
  case AbstractCode::Global:
    GlobalEnvSel(Dest, JIT_V2, tagVal->Sel(0)); break;
  case AbstractCode::Immediate:
    {
      word val = tagVal->Sel(0);
      if (PointerOp::IsInt(val)) {
	jit_movi_p(Dest, val);
	return Dest;
      }
      else
	ImmediateSel(Dest, JIT_V2, ImmediateEnv::Register(val));
    }
    break;
  default:
    Error("NativeCodeJitter::ReloadIdRef: invalid idRef Tag");
  }
  JITStore::SaveDeref(Dest);
  return Dest;
}

void NativeCodeJitter::KillVariables() {
  jit_movi_p(JIT_R0, Store::IntToWord(4711));
  for (u_int i = livenessTable->GetSize(); i--;)
    if (livenessTable->NeedsKill(i))
      NativeCodeFrame::PutEnv(JIT_V2, i, JIT_R0);
//      else if (i < ALICE_REGISTER_NB)
//        NativeCodeFrame::ReplaceEnv(JIT_V2, i,
//  				  RegisterBank::IndexToRegister(i));
  SaveRegister();
}

void NativeCodeJitter::BlockOnTransient(u_int Ptr, word pc) {
  SetRelativePC(pc);
  JITStore::SetTransientTag(Ptr);
  Generic::Scheduler::SetCurrentData(Ptr);
  jit_movi_ui(JIT_R0, 0);
  Generic::Scheduler::PutNArgs(JIT_R0);
  KillVariables();
  jit_movi_ui(JIT_RET, Worker::REQUEST);
  RETURN();
}

void NativeCodeJitter::CheckPreempt(u_int pc) {
  JITStore::LoadStatus(JIT_R0);
  jit_insn *no_preempt = jit_beqi_ui(jit_forward(), JIT_R0, 0);
  SetRelativePC(Store::IntToWord(pc));
  jit_movi_ui(JIT_R0, Worker::PREEMPT);
  RETURN();
  jit_patch(no_preempt);
}

u_int NativeCodeJitter::InlinePrimitive(word wPrimitive, Vector *actualIdRefs) {
  IntMap *inlineMap = IntMap::FromWordDirect(inlineTable);
  word tag          = inlineMap->Get(wPrimitive);
  u_int Result;
  switch (static_cast<INLINED_PRIMITIVE>(Store::DirectWordToInt(tag))) {
  case FUTURE_BYNEED:
    {
      Generic::Byneed::New(JIT_V1);
      u_int Reg = LoadIdRefKill(JIT_R0, actualIdRefs->Sub(0));
      Generic::Byneed::InitClosure(JIT_V1, Reg);
      JITStore::SetTransientTag(JIT_V1);
      Result = JIT_V1;
    }
    break;
  case CHAR_ORD:
    {
      word instrPC = Store::IntToWord(GetRelativePC());
      Result = LoadIdRef(JIT_V1, actualIdRefs->Sub(0), instrPC);
    }
    break;
  case INT_OPPLUS:
    {
      // to be done: check for overflow
      word instrPC = Store::IntToWord(GetRelativePC());
      u_int x1 = LoadIdRef(JIT_V1, actualIdRefs->Sub(0), instrPC);
      jit_movr_p(JIT_FP, x1);
      u_int x2 = LoadIdRef(JIT_V1, actualIdRefs->Sub(1), instrPC);
      jit_addr_p(JIT_FP, JIT_FP, x2);
      jit_subi_p(JIT_FP, JIT_FP, 1);
      Result = JIT_FP;
    }
    break;
  case INT_OPSUB:
    {
      // to be done: check for overflow
      word instrPC = Store::IntToWord(GetRelativePC());
      u_int x1 = LoadIdRef(JIT_V1, actualIdRefs->Sub(0), instrPC);
      jit_movr_p(JIT_FP, x1);
      u_int x2 = LoadIdRef(JIT_V1, actualIdRefs->Sub(1), instrPC);
      jit_subr_p(JIT_FP, JIT_FP, x2);
      jit_addi_p(JIT_FP, JIT_FP, 1);
      Result = JIT_FP;
    }
    break;
  case INT_OPMUL:
    {
      // to be done: check for overflow
      word instrPC = Store::IntToWord(GetRelativePC());
      u_int x1 = LoadIdRef(JIT_V1, actualIdRefs->Sub(0), instrPC);
      jit_movr_p(JIT_FP, x1);
      u_int x2 = LoadIdRef(JIT_V1, actualIdRefs->Sub(1), instrPC);
      jit_movr_p(JIT_R0, JIT_FP);
      if (x2 != JIT_V1) {
	jit_movr_p(JIT_V1, x2);
      }
      JITStore::DirectWordToInt(JIT_R0, JIT_R0);
      JITStore::DirectWordToInt(JIT_V1, JIT_V1);
      jit_mulr_i(JIT_R0, JIT_R0, JIT_V1);
      JITStore::IntToWord(JIT_R0, JIT_R0);
      Result = JIT_R0;
    }
    break;
  case INT_OPLESS:
    {
      word instrPC = Store::IntToWord(GetRelativePC());
      u_int x1 = LoadIdRef(JIT_V1, actualIdRefs->Sub(0), instrPC);
      jit_movr_p(JIT_FP, x1);
      u_int x2 = LoadIdRef(JIT_V1, actualIdRefs->Sub(1), instrPC);
      if (x2 != JIT_V1) {
	jit_movr_p(JIT_V1, x2);
      }
      JITStore::DirectWordToInt(JIT_FP, JIT_FP);
      JITStore::DirectWordToInt(JIT_V1, JIT_V1);
      jit_movi_p(JIT_R0, Store::IntToWord(1));
      jit_insn *smaller = jit_bltr_i(jit_forward(), JIT_FP, JIT_V1);
      jit_movi_p(JIT_R0, Store::IntToWord(0));
      jit_patch(smaller);
      Result = JIT_R0;
    }
    break;
  default:
    Result = JIT_R0;
    Error("CompilePrimitive: illegal inline");
  }
  return Result;
} 

void NativeCodeJitter::CompileContinuation(TagVal *idDefArgsInstrOpt) {
  JIT_LOG_MESG("non-tail call\n");
  Tuple *idDefArgsInstr = Tuple::FromWordDirect(idDefArgsInstrOpt->Sel(0));
  jit_insn *docall      = jit_jmpi(jit_forward());
  word contPC = Store::IntToWord(GetRelativePC());
  TagVal *idDefArgs = TagVal::FromWordDirect(idDefArgsInstr->Sel(0));
  u_int calleeArity = GetArity(idDefArgs);
  CompileCCC(calleeArity);
  StoreResults(calleeArity, idDefArgs);
  CompileBranch(TagVal::FromWordDirect(idDefArgsInstr->Sel(1)));
  jit_patch(docall);
  SetRelativePC(contPC);
}

void NativeCodeJitter::LoadArguments(TagVal *actualArgs) {
  switch (AbstractCode::GetArgs(actualArgs)) {
  case AbstractCode::OneArg:
    {
      jit_movi_ui(JIT_R0, Scheduler::ONE_ARG);
      Generic::Scheduler::PutNArgs(JIT_R0);
      u_int reg = LoadIdRefKill(JIT_R0, actualArgs->Sel(0));
      Generic::Scheduler::PutZeroArg(reg);
    }
    break;
  case AbstractCode::TupArgs:
    {
      Vector *actualIdRefs = Vector::FromWordDirect(actualArgs->Sel(0));
      u_int nArgs          = actualIdRefs->GetLength();
      jit_movi_ui(JIT_R0, nArgs);
      Generic::Scheduler::PutNArgs(JIT_R0);
      Generic::Scheduler::GetCurrentArgs(JIT_V1);
      for (u_int i = nArgs; i--;) {
	u_int reg = LoadIdRefKill(JIT_R0, actualIdRefs->Sub(i));
	Generic::Scheduler::PutArg(JIT_V1, i, reg);
      }
    }
    break;
  }
}

static
inline bool SkipCCC(bool direct, TagVal *actualArgs, TagVal *calleeArgs) {
  return (direct || (AbstractCode::GetArgs(actualArgs) ==
		     AbstractCode::GetArgs(calleeArgs)));
}

// DirectAppVar/AppVar of idRef * idRef args * (idDef args * instr) option
TagVal *NativeCodeJitter::Apply(TagVal *pc, Closure *closure, bool direct) {
  TagVal *actualArgs = TagVal::FromWordDirect(pc->Sel(1));
  CallInfo info;
  info.mode = NORMAL_CALL;
  info.pc   = Store::DirectWordToInt(initialPC);
  if (closure != INVALID_POINTER) {
    word wConcreteCode         = closure->GetConcreteCode();
    ConcreteCode *concreteCode = ConcreteCode::FromWord(wConcreteCode);
    if (wConcreteCode == currentConcreteCode) {
      info.mode    = SELF_CALL;
      info.closure = ImmediateEnv::Register(closure->ToWord());
      if (SkipCCC(direct, actualArgs, currentArgs))
	info.pc = Store::DirectWordToInt(initialNoCCCPC);
      goto no_request;
    } else if (concreteCode != INVALID_POINTER) {
      Interpreter *interpreter = concreteCode->GetInterpreter();
      void *cFunction          = (void *) interpreter->GetCFunction();
      u_int calleeArity        = interpreter->GetInArity(concreteCode);
      u_int actualArity        = GetArity(actualArgs);
      if (interpreter == NativeCodeInterpreter::self) {
	NativeConcreteCode *nativeCode =
	  static_cast<NativeConcreteCode *>(concreteCode);
	info.mode    = NATIVE_CALL;
	info.closure = ImmediateEnv::Register(closure->ToWord());
	if (direct || (calleeArity == actualArity))
	  info.pc = nativeCode->GetSkipCCCPC();
	goto no_request;
      }
      if (cFunction != NULL) {
	IntMap *inlineMap = IntMap::FromWordDirect(inlineTable);
	word wCFunction = Store::UnmanagedPointerToWord(cFunction);
	if ((direct || (calleeArity == actualArity)) &&
	    inlineMap->IsMember(wCFunction)) {
	  Vector *actualIdRefs = GetActualIdRefs(actualArgs);
	  u_int Result         = InlinePrimitive(wCFunction, actualIdRefs);
	  TagVal *idDefArgsInstrOpt = TagVal::FromWord(pc->Sel(2));
	  if (idDefArgsInstrOpt != INVALID_POINTER) {
	    Tuple *idDefArgsInstr =
	      Tuple::FromWordDirect(idDefArgsInstrOpt->Sel(0));
	    TagVal *idDefArgs =
	      TagVal::FromWordDirect(idDefArgsInstr->Sel(0));
	    calleeArity = GetArity(idDefArgs);
	    // to be done: Output CCC
	    switch (calleeArity) {
	    case Scheduler::ONE_ARG:
	      if (idDefArgs != INVALID_POINTER) {
		TagVal *idDef = TagVal::FromWord(idDefArgs->Sel(0));
		if (idDef != INVALID_POINTER) {
		  LocalEnvPut(JIT_V2, idDef->Sel(0), Result);
		}
	      }
	      break;
	    case 0:
	      // to be done: request unit
	      break;
	    default:
	      Error("Apply: unable to inline multi return values\n");
	    }
	    return TagVal::FromWordDirect(idDefArgsInstr->Sel(1));
	  } else {
	    Generic::Primitive::Return1(Result);
	    u_int size = NativeCodeFrame::GetFrameSize(currentNLocals);
	    Generic::Scheduler::PopFrame(size);
	    jit_movi_ui(JIT_R0, Worker::CONTINUE);
	    jit_ret();
	    return INVALID_POINTER;
	  }
	} else {
	  LoadArguments(actualArgs);
	  if (!direct && (calleeArity != actualArity))
	    CompileCCC(calleeArity);
	  TagVal *idDefArgsInstrOpt = TagVal::FromWord(pc->Sel(2));
	  if (idDefArgsInstrOpt != INVALID_POINTER) {
	    CompileContinuation(idDefArgsInstrOpt);
	    KillVariables();
	    DirectCall(interpreter);
	  } else {
	    // Optimize Primitive Tail Call
	    // Invariant: we have enough space on the stack
	    u_int size = NativeCodeFrame::GetFrameSize(currentNLocals);
	    Generic::Scheduler::PopAndPushFrame(JIT_V2, size, 1);
	    Generic::StackFrame::PutWorker(JIT_V2, interpreter);
	    JITStore::Call(0, cFunction);
	    RETURN();
	  }
	  return INVALID_POINTER;
	}
      }
    }
  }
  {
    word instrPC  = Store::IntToWord(GetRelativePC());
    u_int closure = LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
    jit_pushr_ui(closure); // Save Closure
  }
 no_request:
  TagVal *idDefArgsInstrOpt = TagVal::FromWord(pc->Sel(2));
  if (idDefArgsInstrOpt != INVALID_POINTER)
    CompileContinuation(idDefArgsInstrOpt);
  LoadArguments(actualArgs);
  KillIdRef(pc->Sel(0));
  if (idDefArgsInstrOpt != INVALID_POINTER) {
    KillVariables();
    PushCall(&info);
  }
  else
    TailCall(&info);
  return INVALID_POINTER;
}

//
// Instructions
//
// Kill of id vector * instr
TagVal *NativeCodeJitter::InstrKill(TagVal *pc) {
  Vector *kills = Vector::FromWordDirect(pc->Sel(0));
  for (u_int i = kills->GetLength(); i--;) {
    u_int index = RefToIndex(kills->Sub(i));
    livenessTable->SetKill(index);
  }
  return TagVal::FromWordDirect(pc->Sel(1));
}

// PutVar of id * idRef * instr
TagVal *NativeCodeJitter::InstrPutVar(TagVal *pc) {
  JIT_PRINT_PC("PutVar\n");
  u_int Reg = LoadIdRefKill(JIT_R0, pc->Sel(1));
  LocalEnvPut(JIT_V2, pc->Sel(0), Reg);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// PutNew of id * string * instr
TagVal *NativeCodeJitter::InstrPutNew(TagVal *pc) {
  JIT_PRINT_PC("PutNew\n");
  u_int i1 = ImmediateEnv::Register(pc->Sel(1));
  ImmediateSel(JIT_R0, JIT_V2, i1);
  // DirectWordToBlock(JIT_R0) does nothing
  JITStore::Prepare();
  jit_pushr_ui(JIT_R0);
  void *ptr = (void *) Outline::Constructor::New;
  JITStore::Call(1, ptr); // Constructor resides in JIT_RET
  JITStore::Finish();
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_RET);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// PutTag of id * int * idRef vector * instr
TagVal *NativeCodeJitter::InstrPutTag(TagVal *pc) {
  JIT_PRINT_PC("PutTag\n");
  Vector *idRefs = Vector::FromWordDirect(pc->Sel(2));
  u_int nArgs    = idRefs->GetLength();
  JITAlice::TagVal::New(JIT_V1, Store::DirectWordToInt(pc->Sel(1)), nArgs);
  for (u_int i = nArgs; i--;) {
    u_int Reg = LoadIdRefKill(JIT_R0, idRefs->Sub(i));
    JITAlice::TagVal::Put(JIT_V1, i, Reg);
  }
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_V1);
  return TagVal::FromWordDirect(pc->Sel(3));
}

// PutCon of id * idRef * idRef vector * instr
TagVal *NativeCodeJitter::InstrPutCon(TagVal *pc) {
  word instrPC = Store::IntToWord(GetRelativePC());
  JIT_PRINT_PC("PutCon\n");
  u_int constr = LoadIdRef(JIT_V1, pc->Sel(1), instrPC);
  KillIdRef(pc->Sel(1));
  jit_pushr_ui(constr); // Save Constructor
  Vector *idRefs = Vector::FromWordDirect(pc->Sel(2));
  u_int nArgs    = idRefs->GetLength();
  JITAlice::ConVal::New(JIT_V1, nArgs);
  jit_popr_ui(JIT_R0); // Restore Constructor
  JITAlice::ConVal::InitConstr(JIT_V1, JIT_R0);
  for (u_int i = nArgs; i--;) {
    u_int Reg = LoadIdRefKill(JIT_R0, idRefs->Sub(i));
    JITAlice::ConVal::Put(JIT_V1, i, Reg);
  }
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_V1);
  return TagVal::FromWordDirect(pc->Sel(3));
}

// PutRef of id * idRef * instr
TagVal *NativeCodeJitter::InstrPutRef(TagVal *pc) {
  JIT_PRINT_PC("PutRef\n");
  JITAlice::Cell::New(JIT_V1);
  u_int reg = LoadIdRefKill(JIT_R0, pc->Sel(1));
  JITAlice::Cell::Put(JIT_V1, reg);
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_V1);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// PutTup of id * idRef vector * instr
TagVal *NativeCodeJitter::InstrPutTup(TagVal *pc) {
  JIT_PRINT_PC("PutTup\n");
  Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
  u_int nArgs    = idRefs->GetLength();
  if (nArgs == 0) {
    jit_movi_p(JIT_V1, Store::IntToWord(0)); // unit
  }
  else {
    Generic::Tuple::New(JIT_V1, nArgs);
    for (u_int i = nArgs; i--;) {
      u_int Reg = LoadIdRefKill(JIT_R0, idRefs->Sub(i));
      Generic::Tuple::Put(JIT_V1, i, Reg);
    }
  }
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_V1);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// PutPolyRec of id * label vector * idRef vector * instr
TagVal *NativeCodeJitter::InstrPutPolyRec(TagVal *pc) {
  JIT_PRINT_PC("PutPolyRec\n");
  Vector *labels = Vector::FromWordDirect(pc->Sel(1));
  Vector *idRefs = Vector::FromWordDirect(pc->Sel(2));
  Assert(labels->GetLength() == idRefs->GetLength());
  u_int n = labels->GetLength();
  JITAlice::Record::New(JIT_V1, n);
  for (u_int i = n; i--;) {
    UniqueString *label = UniqueString::FromWordDirect(labels->Sub(i));
    u_int labelIndex    = ImmediateEnv::Register(label->ToWord());
    ImmediateSel(JIT_R0, JIT_V2, labelIndex);
    JITAlice::Record::InitLabel(JIT_V1, i, JIT_R0);
    u_int Reg = LoadIdRefKill(JIT_R0, idRefs->Sub(i));
    JITAlice::Record::InitValue(JIT_V1, i, Reg);
  }
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_V1);
  return TagVal::FromWordDirect(pc->Sel(3));
}

// PutVec of id * idRef vector * instr
TagVal *NativeCodeJitter::InstrPutVec(TagVal *pc) {
  JIT_PRINT_PC("PutVec\n");
  Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
  u_int nArgs    = idRefs->GetLength();
  JITAlice::Vector::New(JIT_V1, nArgs);
  for (u_int i = nArgs; i--;) {
    u_int Reg = LoadIdRefKill(JIT_R0, idRefs->Sub(i));
    JITAlice::Vector::Put(JIT_V1, i, Reg);
  }
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_V1);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// Close of id * idRef vector * template * instr
TagVal *NativeCodeJitter::InstrClose(TagVal *pc) {
  JIT_PRINT_PC("Close\n");
  Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
  u_int nGlobals = idRefs->GetLength();
  Generic::Closure::New(JIT_V1, nGlobals);
  // Instantiate the template into an abstract code:
  TagVal *abstractCode =
    TagVal::New(AbstractCode::Function, AbstractCode::functionWidth);
  TagVal *template_ = TagVal::FromWordDirect(pc->Sel(2));
  template_->AssertWidth(AbstractCode::functionWidth);
  abstractCode->Init(0, template_->Sel(0));
  Assert(static_cast<u_int>(Store::DirectWordToInt(template_->Sel(1))) ==
	 nGlobals);
  // Inherit substitution
  Vector *subst = Vector::New(nGlobals);
  for (u_int i = nGlobals; i--; ) {
    TagVal *tagVal = TagVal::FromWord(idRefs->Sub(i));
    if (AbstractCode::GetIdRef(tagVal) == AbstractCode::Global) {
      TagVal *substVal = LookupSubst(Store::DirectWordToInt(tagVal->Sel(0)));
      if (AbstractCode::GetIdRef(substVal) == AbstractCode::Immediate) {
	TagVal *some = TagVal::New(Types::SOME, 1);
	some->Init(0, substVal->Sel(0));
	subst->Init(i, some->ToWord());
      }
      else
	subst->Init(i, Store::IntToWord(Types::NONE));
    }
    else
      subst->Init(i, Store::IntToWord(Types::NONE));
  }
  abstractCode->Init(1, subst->ToWord());
  abstractCode->Init(2, template_->Sel(2));
  abstractCode->Init(3, template_->Sel(3));
  abstractCode->Init(4, template_->Sel(4));
  abstractCode->Init(5, template_->Sel(5));
  // Construct concrete code from abstract code:
  word wConcreteCode =
    AliceLanguageLayer::concreteCodeConstructor(abstractCode);
  u_int i1 = ImmediateEnv::Register(wConcreteCode);
  ImmediateSel(JIT_R0, JIT_V2, i1);
  Generic::Closure::InitConcreteCode(JIT_V1, JIT_R0);
#if PROFILE
  JITStore::Prepare();
  jit_pushr_ui(JIT_R0);
  JITStore::Call(1, (void *) Profiler::IncClosures);
  JITStore::Finish();
#endif
  for (u_int i = nGlobals; i--;) {
    u_int Reg = LoadIdRefKill(JIT_R0, idRefs->Sub(i));
    Generic::Closure::Put(JIT_V1, i, Reg);
  }
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_V1);
  return TagVal::FromWordDirect(pc->Sel(3));
}

// Specialize of id * idRef vector * template * instr
// where   template = Template of coord * int * string vector *
//                    idDef args * instr * liveness
// abstractCode =
//    Function of coord * value option vector * string vector *
//                idDef args * instr * liveness
// Design options: call NativeCodeConctructor directly or use
// AliceLanguageLayer::concreteCodeConstructor
TagVal *NativeCodeJitter::InstrSpecialize(TagVal *pc) {
  JIT_PRINT_PC("Specialize\n");
  // Create specialized abstractCode
  JITAlice::TagVal::New(JIT_V1, AbstractCode::Function,
			AbstractCode::functionWidth);
  jit_pushr_ui(JIT_V0); // Save V0
  TagVal *template_ = TagVal::FromWordDirect(pc->Sel(2));
  template_->AssertWidth(AbstractCode::functionWidth);
  u_int i1 = ImmediateEnv::Register(template_->ToWord()); // Save template_
  ImmediateSel(JIT_V0, JIT_V2, i1); // Load template_
#if PROFILE
  JITStore::Prepare();
  jit_pushr_ui(JIT_V0);
  JITStore::Call(1, (void *) Profiler::IncInstances);
  JITStore::Finish();
#endif
  JITAlice::TagVal::Sel(JIT_R0, JIT_V0, 0);
  JITAlice::TagVal::Put(JIT_V1, 0, JIT_R0);
  // position one will be filled in later
  JITAlice::TagVal::Sel(JIT_R0, JIT_V0, 2);
  JITAlice::TagVal::Put(JIT_V1, 2, JIT_R0);
  JITAlice::TagVal::Sel(JIT_R0, JIT_V0, 3);
  JITAlice::TagVal::Put(JIT_V1, 3, JIT_R0);
  JITAlice::TagVal::Sel(JIT_R0, JIT_V0, 4);
  JITAlice::TagVal::Put(JIT_V1, 4, JIT_R0);
  JITAlice::TagVal::Sel(JIT_R0, JIT_V0, 5);
  JITAlice::TagVal::Put(JIT_V1, 5, JIT_R0);
  jit_popr_ui(JIT_V0); // Restore V0
  jit_pushr_ui(JIT_V1); // Save abstractCode
  // Create Substitution (value option vector)
  Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
  u_int nGlobals = idRefs->GetLength();
  Assert(static_cast<u_int>(Store::DirectWordToInt(template_->Sel(1))) ==
	 nGlobals);
  JITAlice::Vector::New(JIT_V1, nGlobals);
  for (u_int i = nGlobals; i--;) {
    jit_pushr_ui(JIT_V1); // save subst vector
    JITAlice::TagVal::New(JIT_V1, Types::SOME, 1);
    u_int Reg = LoadIdRef(JIT_R0, idRefs->Sub(i), (word) 0);
    JITAlice::TagVal::Put(JIT_V1, 0, Reg);
    jit_movr_p(JIT_R0, JIT_V1); // move TagVal (SOME value) to R0
    jit_popr_ui(JIT_V1); // restore subst vector
    JITAlice::Vector::Put(JIT_V1, i, JIT_R0);
  }
  jit_movr_p(JIT_R0, JIT_V1); // Move subst vector to R0
  jit_popr_ui(JIT_V1); // Restore abstractCode
  JITAlice::TagVal::Put(JIT_V1, 1, JIT_R0); // Store subst vector
  JITStore::Prepare();
  jit_pushr_ui(JIT_V1); // abstractCode
  Generic::Closure::New(JIT_V1, nGlobals);
  JITStore::Call(1, (void *) AliceLanguageLayer::concreteCodeConstructor);
  JITStore::Finish();
  Generic::Closure::InitConcreteCode(JIT_V1, JIT_RET);
  for (u_int i = nGlobals; i--;) {
    u_int Reg = LoadIdRefKill(JIT_R0, idRefs->Sub(i));
    Generic::Closure::Put(JIT_V1, i, Reg);
  }
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_V1);
  return TagVal::FromWordDirect(pc->Sel(3));
}

// AppPrim of value * idRef vector * (idDef * instr) option
TagVal *NativeCodeJitter::InstrAppPrim(TagVal *pc) {
  JIT_PRINT_PC("AppPrim\n");
  Closure *closure = Closure::FromWord(pc->Sel(0));
  ConcreteCode *concreteCode =
    ConcreteCode::FromWord(closure->GetConcreteCode());
  Interpreter *interpreter = concreteCode->GetInterpreter();
#if defined(JIT_STORE_DEBUG)
  JIT_LOG_MESG(interpreter->Identify());
#endif
  IntMap *inlineMap = IntMap::FromWordDirect(inlineTable);
  void *cFunction   = (void *) interpreter->GetCFunction();
  word wCFunction   = Store::UnmanagedPointerToWord(cFunction);
  if (inlineMap->IsMember(wCFunction)) {
    // Inline primitive
    Vector *actualIdRefs  = Vector::FromWordDirect(pc->Sel(1));
    u_int Result          = InlinePrimitive(wCFunction, actualIdRefs);
    TagVal *idDefInstrOpt = TagVal::FromWord(pc->Sel(2));
    if (idDefInstrOpt != INVALID_POINTER) { // SOME (idDef * instr)
      Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
      TagVal *idDef     = TagVal::FromWord(idDefInstr->Sel(0));
      if (idDef != INVALID_POINTER)
	LocalEnvPut(JIT_V2, idDef->Sel(0), Result);
      return TagVal::FromWordDirect(idDefInstr->Sel(1));
    } else {
      Generic::Primitive::Return1(Result);
      u_int size = NativeCodeFrame::GetFrameSize(currentNLocals);
      Generic::Scheduler::PopFrame(size);
      jit_movi_ui(JIT_R0, Worker::CONTINUE);
      RETURN();
    }
  } else {
    // Normal primitive call
    TagVal *idDefInstrOpt = TagVal::FromWord(pc->Sel(2));
    word contPC = Store::IntToWord(0);
    if (idDefInstrOpt != INVALID_POINTER) { // SOME (idDef * instr)
      Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
      jit_insn *docall  = jit_jmpi(jit_forward());
      contPC = Store::IntToWord(GetRelativePC());
      TagVal *idDef = TagVal::FromWord(idDefInstr->Sel(0));
      if (idDef != INVALID_POINTER) {
	Generic::Scheduler::GetZeroArg(JIT_R0);
	LocalEnvPut(JIT_V2, idDef->Sel(0), JIT_R0);
      }
      CompileBranch(TagVal::FromWordDirect(idDefInstr->Sel(1)));
      jit_patch(docall);
      SetRelativePC(contPC);
    }
    // Load Arguments
    Vector *actualIdRefs = Vector::FromWordDirect(pc->Sel(1));
    u_int nArgs          = actualIdRefs->GetLength();
    jit_movi_ui(JIT_R0, ((nArgs == 1) ? Scheduler::ONE_ARG : nArgs));
    Generic::Scheduler::PutNArgs(JIT_R0);
    Generic::Scheduler::GetCurrentArgs(JIT_V1);
    for (u_int i = nArgs; i--;) {
      u_int reg = LoadIdRefKill(JIT_R0, actualIdRefs->Sub(i));
      Generic::Scheduler::PutArg(JIT_V1, i, reg);
    }
#if defined(DEBUG_CHECK)
    u_int arity = interpreter->GetInArity(concreteCode);
    Assert(arity == Scheduler::ONE_ARG && nArgs == 1 ||
	   arity != Scheduler::ONE_ARG && nArgs == arity); arity = arity;
#endif
    if (idDefInstrOpt != INVALID_POINTER) {
      KillVariables();
      DirectCall(interpreter);
    }
    else {
      // Optimize Primitive Tail Call
      // Invariant: we have enough space on the stack
      u_int size = NativeCodeFrame::GetFrameSize(currentNLocals);
      Generic::Scheduler::PopAndPushFrame(JIT_V2, size, 1);
      Generic::StackFrame::PutWorker(JIT_V2, interpreter);
      JITStore::Call(0, cFunction);
      RETURN();
    }
  }
  return INVALID_POINTER;
}

// DirectAppVar/AppVar of idRef * idRef args * (idDef args * instr) option
TagVal *NativeCodeJitter::InstrAppVar(TagVal *pc, bool direct) {
  JIT_PRINT_PC((direct ? "DirectAppVar\n" : "AppVar\n"));
  TagVal *tagVal = TagVal::FromWord(pc->Sel(0));
  if (AbstractCode::GetIdRef(tagVal) == AbstractCode::Global)
    tagVal = LookupSubst(Store::DirectWordToInt(tagVal->Sel(0)));
  word wClosure;
  switch (AbstractCode::GetIdRef(tagVal)) {
  case AbstractCode::Immediate:
    wClosure = tagVal->Sel(0);
    break;
  default:
    wClosure = Store::IntToWord(0);
    break;
  }
  return Apply(pc, Closure::FromWord(wClosure), direct);
}

// GetRef of id * idRef * instr
TagVal *NativeCodeJitter::InstrGetRef(TagVal *pc) {
  JIT_PRINT_PC("GetRef\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  u_int Cell   = LoadIdRef(JIT_V1, pc->Sel(1), instrPC);
  KillIdRef(pc->Sel(1));
  JITAlice::Cell::Sel(JIT_R0, Cell);
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_R0);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// GetTup of idDef vector * idRef * instr
TagVal *NativeCodeJitter::InstrGetTup(TagVal *pc) {
  JIT_PRINT_PC("GetTup\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  u_int Tuple  = LoadIdRef(JIT_V1, pc->Sel(1), instrPC);
  KillIdRef(pc->Sel(1));
  Vector *idDefs = Vector::FromWordDirect(pc->Sel(0));
  u_int nArgs    = idDefs->GetLength();
  if (nArgs != 0)
    for (u_int i = nArgs; i--;) {
      TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
      if (idDef != INVALID_POINTER) {
	Generic::Tuple::Sel(JIT_R0, Tuple, i);
	LocalEnvPut(JIT_V2, idDef->Sel(0), JIT_R0);
      }
    }
  return TagVal::FromWordDirect(pc->Sel(2));
}

// Sel of id * idRef * int * instr
TagVal *NativeCodeJitter::InstrSel(TagVal *pc) {
  JIT_PRINT_PC("Sel\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  u_int Tuple  = LoadIdRef(JIT_V1, pc->Sel(1), instrPC);
  KillIdRef(pc->Sel(1));
  Generic::Tuple::Sel(JIT_R0, Tuple, Store::DirectWordToInt(pc->Sel(2)));
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_R0);
  return TagVal::FromWordDirect(pc->Sel(3));
}

// LazyPolySel of id vector * idRef * label vector * instr
TagVal *NativeCodeJitter::InstrLazyPolySel(TagVal *pc) {
  JIT_PRINT_PC("LazyPolySel\n");
  u_int WRecord = LoadIdRefKill(JIT_V1, pc->Sel(1));
  Vector *ids = Vector::FromWordDirect(pc->Sel(0));
  Vector *labels = Vector::FromWordDirect(pc->Sel(2));
  Assert(ids->GetLength() == labels->GetLength());
  JITStore::Deref(WRecord);
  JIT_LOG_MESG("Deref result\n");
  JIT_LOG_REG(WRecord);
  jit_insn *poly_sel = jit_beqi_ui(jit_forward(), JIT_R0, BLKTAG);
  // Record yet unknown: create byneeds
  jit_pushr_ui(WRecord); // save WRecord
  for (u_int i = ids->GetLength(); i--; ) {
    jit_popr_ui(JIT_R0); // restore WRecord
    jit_pushr_ui(JIT_R0); // save WRecord
    LazySelClosureNew(JIT_R0, UniqueString::FromWordDirect(labels->Sub(i)));
    jit_pushr_ui(JIT_V1); // save LazySel closure
    Generic::Byneed::New(JIT_V1);
    jit_popr_ui(JIT_R0); // restore LazySel closure
    Generic::Byneed::InitClosure(JIT_V1, JIT_R0);
    JITStore::SetTransientTag(JIT_V1);
    LocalEnvPut(JIT_V2, ids->Sub(i), JIT_V1);
  }
  jit_popr_ui(JIT_R0); // clear stack (restore WRecord)
  jit_insn *skip = jit_jmpi(jit_forward());
  // Record known: perform selection immediately
  jit_patch(poly_sel);
  if (WRecord != JIT_V1)
    jit_movr_p(JIT_V1, WRecord);
  for (u_int i = ids->GetLength(); i--; ) {
    JITStore::Prepare();
    u_int labelIndex = ImmediateEnv::Register(labels->Sub(i));
    ImmediateSel(JIT_R0, JIT_V2, labelIndex);
    // UniqueString::FromWordDirect does nothing
    jit_pushr_ui(JIT_R0); // label
    jit_pushr_ui(JIT_V1); // record
    void *ptr = (void *) Outline::Record::PolySel;
    JITStore::Call(2, ptr);
    JITStore::Finish();
    LocalEnvPut(JIT_V2, ids->Sub(i), JIT_R0);
  }
  jit_patch(skip);
  return TagVal::FromWordDirect(pc->Sel(3));
}

// Raise of idRef
TagVal *NativeCodeJitter::InstrRaise(TagVal *pc) {
  JIT_PRINT_PC("Raise\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  u_int Reg = LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  KillIdRef(pc->Sel(0));
  Generic::Scheduler::SetCurrentData(Reg);
  JITStore::Prepare();
  jit_pushr_ui(JIT_V2); // Frame ptr
  JITStore::Call(1, (void *) Outline::Backtrace::New);
  JITStore::Finish();
  Generic::Scheduler::SetCurrentBacktrace(JIT_RET);
  jit_movi_ui(JIT_RET, Worker::RAISE);
  RETURN();
  return INVALID_POINTER;
}

// Reraise of idRef
TagVal *NativeCodeJitter::InstrReraise(TagVal *pc) {
  JIT_PRINT_PC("Reraise\n");
  u_int Reg = LoadIdRefKill(JIT_V1, pc->Sel(0));
  // DirectWordToBlock(JIT_V1) does nothing
  Generic::Tuple::Sel(JIT_R0, Reg, 0);
  Generic::Scheduler::SetCurrentData(JIT_R0);
  Generic::Tuple::Sel(JIT_R0, Reg, 1);
  Generic::Scheduler::SetCurrentBacktrace(JIT_R0);
  jit_movi_ui(JIT_RET, Worker::RAISE);
  RETURN();
  return INVALID_POINTER;
}

// Try of instr * idDef * idDef * instr
TagVal *NativeCodeJitter::InstrTry(TagVal *pc) {
  JIT_PRINT_PC("Try\n");
  u_int handlerPC = ImmediateEnv::Register(Store::IntToWord(0));
  ImmediateSel(JIT_R0, JIT_V2, handlerPC);
  JITStore::Prepare();
  jit_pushr_ui(JIT_R0); // Handler PC
  JITStore::Call(1, (void *) ::Scheduler::PushHandler);
  JITStore::Finish();
  CompileBranch(TagVal::FromWordDirect(pc->Sel(0)));
  ImmediateEnv::Replace(handlerPC, Store::IntToWord(GetRelativePC()));
  JIT_LOG_MESG("executing exception handler\n");
  TagVal *idDef1 = TagVal::FromWord(pc->Sel(1));
  if (idDef1 != INVALID_POINTER) {
    Generic::Scheduler::GetZeroArg(JIT_R0);
    LocalEnvPut(JIT_V2, idDef1->Sel(0), JIT_R0);
  }
  TagVal *idDef2 = TagVal::FromWord(pc->Sel(2));
  if (idDef2 != INVALID_POINTER) {
    Generic::Scheduler::GetOneArg(JIT_R0);
    LocalEnvPut(JIT_V2, idDef2->Sel(0), JIT_R0);
  }
  return TagVal::FromWordDirect(pc->Sel(3));
}

// EndTry of instr
TagVal *NativeCodeJitter::InstrEndTry(TagVal *pc) {
  JIT_PRINT_PC("EndTry\n");
  JITStore::Prepare();
  JITStore::Call(0, (void *) ::Scheduler::PopHandler);
  JITStore::Finish();
  return TagVal::FromWordDirect(pc->Sel(0));
}

// EndHandle of instr
TagVal *NativeCodeJitter::InstrEndHandle(TagVal *pc) {
  JIT_PRINT_PC("EndHandle\n");
  return TagVal::FromWordDirect(pc->Sel(0));
}

// Test Helpers
static void *LookupIntTable(IntMap *map, word key) {
  if (map->IsMember(key))
    return map->Get(key);
  else
    return 0;
}

static void *LookupChunkTable(ChunkMap *map, word key) {
  if (map->IsMember(key))
    return map->Get(key);
  else
    return 0;
}

void NativeCodeJitter::LookupTestTable(u_int Key, u_int table, bool isInt) {
  JITStore::Prepare();
  jit_pushr_ui(Key); // Key Argument
  ImmediateSel(JIT_R0, JIT_V2, table);
  jit_pushr_ui(JIT_R0); // Table Argument
  if (isInt)
    JITStore::Call(2, (void *) LookupIntTable);
  else
    JITStore::Call(2, (void *) LookupChunkTable);
  JITStore::Finish();
}

// IntTest of idRef * (int * instr) vector * instr
TagVal *NativeCodeJitter::InstrIntTest(TagVal *pc) {
  JIT_PRINT_PC("IntTest\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  u_int IntVal = LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  KillIdRef(pc->Sel(0));
  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests  = tests->GetLength();
  IntMap *map   = IntMap::New(nTests * 2);
  u_int i1      = ImmediateEnv::Register(map->ToWord());
  LookupTestTable(IntVal, i1);
  jit_insn *else_ref = jit_beqi_ui(jit_forward(), JIT_RET, 0);
  BranchToOffset(JIT_RET);
  // Create Branches (order is significant)
  for (u_int i = 0; i < nTests; i++) {
    Tuple *pair  = Tuple::FromWordDirect(tests->Sub(i));
    word key     = pair->Sel(0);
    u_int offset = GetRelativePC();
    map->Put(key, Store::IntToWord(offset));
    CompileBranch(TagVal::FromWordDirect(pair->Sel(1)));
  }
  jit_patch(else_ref);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// CompactIntTest of idRef * int * instrs * instr
TagVal *NativeCodeJitter::InstrCompactIntTest(TagVal *pc) {
  JIT_PRINT_PC("CompactIntTest\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  u_int IntVal = LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  KillIdRef(pc->Sel(0));
  JITStore::DirectWordToInt(JIT_R0, IntVal);
  s_int indexOffset = Store::DirectWordToInt(pc->Sel(1));
  if (indexOffset != 0)
    jit_subi_i(JIT_R0, JIT_R0, indexOffset);
  Vector *tests      = Vector::FromWordDirect(pc->Sel(2));
  u_int nTests       = tests->GetLength();
  jit_insn *else_ref = jit_bgei_ui(jit_forward(), JIT_R0, nTests);
  Tuple *branches    = Tuple::New(nTests);
  u_int i1           = ImmediateEnv::Register(branches->ToWord());
  ImmediateSel(JIT_V1, JIT_V2, i1);
  Generic::Tuple::IndexSel(JIT_R0, JIT_V1, JIT_R0); // R0 holds branch offset
  BranchToOffset(JIT_R0);
  // Create branches (order is significant)
  for (u_int i = 0; i < nTests; i++) {
    u_int offset = GetRelativePC();
    branches->Init(i, Store::IntToWord(offset));
    CompileBranch(TagVal::FromWordDirect(tests->Sub(i)));
  }
  jit_patch(else_ref);
  return TagVal::FromWordDirect(pc->Sel(3));
}

// RealTest of idRef * (real * instr) vector * instr
TagVal *NativeCodeJitter::InstrRealTest(TagVal *pc) {
  JIT_PRINT_PC("RealTest\n");
  word instrPC  = Store::IntToWord(GetRelativePC());
  u_int RealVal = LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  KillIdRef(pc->Sel(0));
  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests  = tests->GetLength();
  ChunkMap *map = ChunkMap::New(nTests * 2);
  u_int i1      = ImmediateEnv::Register(map->ToWord());
  LookupTestTable(RealVal, i1, false);
  jit_insn *else_ref = jit_beqi_ui(jit_forward(), JIT_RET, 0);
  BranchToOffset(JIT_RET);
  // Create Branches (order is significant)
  for (u_int i = 0; i < nTests; i++) {
    Tuple *pair  = Tuple::FromWordDirect(tests->Sub(i));
    word key     = pair->Sel(0);
    u_int offset = GetRelativePC();
    map->Put(key, Store::IntToWord(offset));
    CompileBranch(TagVal::FromWordDirect(pair->Sel(1)));
  }
  jit_patch(else_ref);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// StringTest of idRef * (string * instr) vector * instr
TagVal *NativeCodeJitter::InstrStringTest(TagVal *pc) {
  JIT_PRINT_PC("StringTest\n");
  word instrPC    = Store::IntToWord(GetRelativePC());
  u_int StringVal = LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  KillIdRef(pc->Sel(0));
  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests  = tests->GetLength();
  ChunkMap *map = ChunkMap::New(nTests * 2);
  u_int i1      = ImmediateEnv::Register(map->ToWord());
  LookupTestTable(StringVal, i1, false);
  jit_insn *else_ref = jit_beqi_ui(jit_forward(), JIT_RET, 0);
  BranchToOffset(JIT_RET);
  // Create Branches (order is significant)
  for (u_int i = 0; i < nTests; i++) {
    Tuple *pair  = Tuple::FromWordDirect(tests->Sub(i));
    word key     = pair->Sel(0);
    u_int offset = GetRelativePC();
    map->Put(key, Store::IntToWord(offset));
    CompileBranch(TagVal::FromWordDirect(pair->Sel(1)));
  }
  jit_patch(else_ref);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// TagTest of idRef * (int * instr) vector
//         * (int * idDef vector * instr) vector * instr
TagVal *NativeCodeJitter::InstrTagTest(TagVal *pc) {
  JIT_PRINT_PC("TagTest\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  u_int tagVal = LoadIdRef(JIT_V1, pc->Sel(0), (word) 0);
  jit_insn *ref[2];
  JITStore::Deref3(tagVal, ref);
  // Integer branch (V1 is int word, nullary constructor)
  KillIdRef(pc->Sel(0));
  Vector *tests1 = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests1  = tests1->GetLength();
  jit_insn *else_ref1;
  if (nTests1 == 0) {
    else_ref1 = jit_jmpi(jit_forward());
  }
  else {
    IntMap *map1 = IntMap::New(nTests1 * 2);
    u_int i1     = ImmediateEnv::Register(map1->ToWord());
    LookupTestTable(tagVal, i1);
    else_ref1 = jit_beqi_ui(jit_forward(), JIT_RET, 0);
    BranchToOffset(JIT_RET);
    // Create Branches (order is significant)
    for (u_int i = 0; i < nTests1; i++) {
      Tuple *pair  = Tuple::FromWordDirect(tests1->Sub(i));
      word key     = pair->Sel(0);
      u_int offset = GetRelativePC();
      map1->Put(key, Store::IntToWord(offset));
      CompileBranch(TagVal::FromWordDirect(pair->Sel(1)));
    }
  }
  jit_patch(ref[0]); // Transient Branch
  BlockOnTransient(tagVal, instrPC);
  jit_patch(ref[1]); // Block Branch
  // Block branch (V1 is Non-nullary constructor)
  KillIdRef(pc->Sel(0));
  Vector *tests2 = Vector::FromWordDirect(pc->Sel(2));
  u_int nTests2  = tests2->GetLength();
  if (nTests2 != 0) {
    JITAlice::TagVal::GetTag(tagVal);
    IntToWord(JIT_R0, JIT_R0);
    jit_pushr_ui(tagVal); // Save TagVal Ptr
    IntMap *map2 = IntMap::New(nTests2 * 2);
    u_int i2     = ImmediateEnv::Register(map2->ToWord());
    LookupTestTable(JIT_R0, i2);
    jit_insn *else_ref2 = jit_beqi_ui(jit_forward(), JIT_RET, 0);
    BranchToOffset(JIT_RET);
    // Create Branches (order is significant)
    for (u_int i = 0; i < nTests2; i++) {
      Tuple *triple  = Tuple::FromWordDirect(tests2->Sub(i));
      word key       = triple->Sel(0);
      u_int offset   = GetRelativePC();
      map2->Put(key, Store::IntToWord(offset));
      Vector *idDefs = Vector::FromWordDirect(triple->Sel(1));
      jit_popr_ui(JIT_V1); // Restore TagVal Ptr
      for (u_int i = idDefs->GetLength(); i--;) {
	TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
	if (idDef != INVALID_POINTER) {
	  JITAlice::TagVal::Sel(JIT_R0, JIT_V1, i);
	  LocalEnvPut(JIT_V2, idDef->Sel(0), JIT_R0);
	}
      }
      CompileBranch(TagVal::FromWordDirect(triple->Sel(2)));
    }
    jit_patch(else_ref2);
    // clear stack
    jit_popr_ui(JIT_V1);
  }
  jit_patch(else_ref1);
  return TagVal::FromWordDirect(pc->Sel(3));
}

// CompactTagTest of idRef * tagTests * instr option
TagVal *NativeCodeJitter::InstrCompactTagTest(TagVal *pc) {
  JIT_PRINT_PC("CompactTagTest\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  u_int tagVal = LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  KillIdRef(pc->Sel(0));
  jit_insn *have_constructor = jit_beqi_ui(jit_forward(), JIT_R0, BLKTAG);
  DirectWordToInt(JIT_R0, tagVal);
  jit_insn *skip_tagload = jit_jmpi(jit_forward());
  // Block branch (v1 is non-nullary constructor)
  jit_patch(have_constructor);
  JITAlice::TagVal::GetTag(tagVal);
  jit_patch(skip_tagload);
  Vector *tests         = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests          = tests->GetLength();
  TagVal *someElseInstr = TagVal::FromWord(pc->Sel(2));
  jit_insn *else_ref    = NULL;
  if (someElseInstr != INVALID_POINTER) {
    else_ref = jit_bgei_ui(jit_forward(), JIT_R0, nTests);
  }
  Tuple *branches       = Tuple::New(nTests);
  u_int i1              = ImmediateEnv::Register(branches->ToWord());
  jit_pushr_ui(tagVal); // Save int/tagval ptr
  ImmediateSel(JIT_V1, JIT_V2, i1);
  Generic::Tuple::IndexSel(JIT_R0, JIT_V1, JIT_R0); // R0 holds branch offset
  BranchToOffset(JIT_R0);
  // Create Branches (order is significant)
  Assert(nTests != 0);
  for (u_int i = 0; i < nTests; i++) {
    u_int offset = GetRelativePC();
    branches->Init(i, Store::IntToWord(offset));
    Tuple *tuple      = Tuple::FromWordDirect(tests->Sub(i));
    TagVal *idDefsOpt = TagVal::FromWord(tuple->Sel(0));
    jit_popr_ui(JIT_V1); // Restore int/tagval ptr
    if (idDefsOpt != INVALID_POINTER) {
      Vector *idDefs = Vector::FromWordDirect(idDefsOpt->Sel(0));
      for (u_int j = idDefs->GetLength(); j--;) {
	TagVal *idDef = TagVal::FromWord(idDefs->Sub(j));
	if (idDef != INVALID_POINTER) {
	  JITAlice::TagVal::Sel(JIT_R0, JIT_V1, j);
	  LocalEnvPut(JIT_V2, idDef->Sel(0), JIT_R0);
	}
      }
    }
    CompileBranch(TagVal::FromWordDirect(tuple->Sel(1)));
  }
  // else branch
  if (someElseInstr != INVALID_POINTER) {
    jit_patch(else_ref);
    return TagVal::FromWordDirect(someElseInstr->Sel(0));
  } else {
    return INVALID_POINTER;
  }
}

// ConTest of idRef * (idRef * instr) vector
//         * (idRef * idDef vector * instr) vector * instr
TagVal *NativeCodeJitter::InstrConTest(TagVal *pc) {
  JIT_PRINT_PC("ConTest\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  u_int ConVal = LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  JITStore::Block::GetLabel(JIT_R0, ConVal);
  jit_insn *nullary_constr = jit_bnei_ui(jit_forward(), JIT_R0, Alice::ConVal);
  // N-ary Constructor
  JITAlice::ConVal::GetConstructor(JIT_R0, ConVal);
  Vector *tests1 = Vector::FromWordDirect(pc->Sel(2));
  u_int nTests1  = tests1->GetLength();
  for (u_int i = 0; i < nTests1; i++) {
    Tuple *triple = Tuple::FromWordDirect(tests1->Sub(i));
    u_int constr = LoadIdRef(JIT_V1, triple->Sel(0), instrPC);
    // Reload conval and its constr
    ConVal = ReloadIdRef(JIT_R0, pc->Sel(0));
    JITAlice::ConVal::GetConstructor(JIT_R0, ConVal);
    jit_insn *next_test_ref = jit_bner_ui(jit_forward(), constr, JIT_R0);
    Vector *idDefs          = Vector::FromWordDirect(triple->Sel(1));
    ConVal                  = ReloadIdRef(JIT_V1, pc->Sel(0));
    for (u_int i = idDefs->GetLength(); i--;) {
      TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
      if (idDef != INVALID_POINTER) {
	JITAlice::ConVal::Sel(JIT_R0, ConVal, i);
	LocalEnvPut(JIT_V2, idDef->Sel(0), JIT_R0);
      }
    }
    KillIdRef(pc->Sel(0)); // Some kills missing
    CompileBranch(TagVal::FromWordDirect(triple->Sel(2)));
    jit_patch(next_test_ref);
  }
  // Nullary Constructor
  jit_patch(nullary_constr);
  Vector *tests2 = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests2 = tests2->GetLength();
  for (u_int i = 0; i < nTests2; i++) {
    Tuple *pair = Tuple::FromWordDirect(tests2->Sub(i));
    u_int Constr = LoadIdRef(JIT_V1, pair->Sel(0), instrPC);
    ConVal = ReloadIdRef(JIT_R0, pc->Sel(0));
    jit_insn *next_test_ref = jit_bner_ui(jit_forward(), Constr, ConVal);
    KillIdRef(pc->Sel(0)); // Some kills missing
    CompileBranch(TagVal::FromWordDirect(pair->Sel(1)));
    jit_patch(next_test_ref);
  }
  return TagVal::FromWordDirect(pc->Sel(3));
}

// VecTest of idRef * (idDef vector * instr) vector * instr
TagVal *NativeCodeJitter::InstrVecTest(TagVal *pc) {
  JIT_PRINT_PC("VecTest\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  u_int VecVal = LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  KillIdRef(pc->Sel(0));
  jit_pushr_ui(VecVal); // Save vector ptr
  JITAlice::Vector::GetLength(JIT_R0, VecVal);
  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests  = tests->GetLength();
  IntMap *map   = IntMap::New(nTests * 2);
  u_int i1      = ImmediateEnv::Register(map->ToWord());
  LookupTestTable(JIT_R0, i1);
  jit_popr_ui(JIT_V1); // Restore vector ptr
  jit_insn *else_ref = jit_beqi_ui(jit_forward(), JIT_RET, 0);
  BranchToOffset(JIT_RET);
  // Create Branches (order is significant)
  for (u_int i = 0; i < nTests; i++) {
    Tuple *pair    = Tuple::FromWordDirect(tests->Sub(i));
    Vector *idDefs = Vector::FromWordDirect(pair->Sel(0));
    word key       = Store::IntToWord(idDefs->GetLength());
    u_int offset   = GetRelativePC();
    map->Put(key, Store::IntToWord(offset));
    for (u_int i = idDefs->GetLength(); i--;) {
      TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
      if (idDef != INVALID_POINTER) {
	JITAlice::Vector::Sel(JIT_R0, JIT_V1, i);
	LocalEnvPut(JIT_V2, idDef->Sel(0), JIT_R0);
      }
    }
    CompileBranch(TagVal::FromWordDirect(pair->Sel(1)));
  }
  jit_patch(else_ref);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// Shared of stamp * instr
TagVal *NativeCodeJitter::InstrShared(TagVal *pc) {
  JIT_PRINT_PC("InstrShared\n");
  word stamp = pc->Sel(0);
  if (sharedTable->IsMember(stamp)) {
    u_int offset = Store::DirectWordToInt(sharedTable->Get(stamp));
    drop_jit_jmpi(codeBuffer + offset);
    return INVALID_POINTER;
  }
  else {
    u_int offset = jit_get_ip().ptr - (char *) codeBuffer;
    sharedTable->Put(stamp, Store::IntToWord(offset));
    return TagVal::FromWordDirect(pc->Sel(1));
  }
}

// Return of idRef args
TagVal *NativeCodeJitter::InstrReturn(TagVal *pc) {
  JIT_PRINT_PC("Return\n");
  TagVal *returnArgs = TagVal::FromWordDirect(pc->Sel(0));
  switch (AbstractCode::GetArgs(returnArgs)) {
  case AbstractCode::OneArg:
    {
      jit_movi_ui(JIT_R0, Scheduler::ONE_ARG);
      Generic::Scheduler::PutNArgs(JIT_R0);
      u_int Reg = LoadIdRefKill(JIT_R0, returnArgs->Sel(0));
      Generic::Scheduler::PutZeroArg(Reg);
    }
    break;
  case AbstractCode::TupArgs:
    {
      Vector *returnIdRefs = Vector::FromWordDirect(returnArgs->Sel(0));
      u_int nArgs          = returnIdRefs->GetLength();
      if (nArgs < Scheduler::maxArgs) {
	jit_movi_ui(JIT_R0, nArgs);
	Generic::Scheduler::PutNArgs(JIT_R0);
	Generic::Scheduler::GetCurrentArgs(JIT_V1);
	for (u_int i = nArgs; i--;) {
	  u_int Reg = LoadIdRefKill(JIT_R0, returnIdRefs->Sub(i));
	  Generic::Scheduler::PutArg(JIT_V1, i, Reg);
	}
      }
      else {
	Generic::Tuple::New(JIT_V1, nArgs);
	for (u_int i = nArgs; i--;) {
	  u_int Reg = LoadIdRefKill(JIT_R0, returnIdRefs->Sub(i));
	  Generic::Tuple::Put(JIT_V1, i, Reg);
	}
	Generic::Scheduler::PutZeroArg(JIT_V1);
	jit_movi_ui(JIT_R0, Scheduler::ONE_ARG);
	Generic::Scheduler::PutNArgs(JIT_R0);
      }
    }
    break;
  }
  // Continuation expects current stack frame size in JIT_FP
  u_int size = NativeCodeFrame::GetFrameSize(currentNLocals);
  jit_movi_ui(JIT_FP, size * sizeof(word));
  NativeCodeFrame::GetContinuation(JIT_R0, JIT_V2);
  jit_addi_p(JIT_R0, JIT_R0, 2 * sizeof(word));
  jit_jmpr(JIT_R0);
  return INVALID_POINTER;
}

char *NativeCodeJitter::CompileProlog(const char *info) {
  char *start = jit_set_ip(codeBuffer).ptr;
  jit_prolog(1);
  int arg1 = jit_arg_p();
  jit_getarg_p(JIT_V2, arg1);
  JIT_LOG_MESG(info); info = info;
  JIT_LOG_REG(JIT_SP);
  RestoreRegister();
  NativeCodeFrame::GetPC(JIT_R0, JIT_V2);
  BranchToOffset(JIT_R0);
  return start;
}

void NativeCodeJitter::CompileBranch(TagVal *pc) {
  LivenessTable *cloneTable;
  if (livenessFreeList == NULL) {
    cloneTable = livenessTable->Clone();
  }
  else {
    cloneTable = livenessFreeList;
    livenessFreeList = ((LivenessTable **) cloneTable)[0];
    livenessTable->Clone(cloneTable);
  }
  CompileInstr(pc);
  ((LivenessTable **) livenessTable)[0] = livenessFreeList;
  livenessFreeList = livenessTable;
  livenessTable = cloneTable;
}

#ifdef INSTRUCTION_COUNTS
static u_int staticCounts[AbstractCode::nInstrs];
static u_int dynamicCounts[AbstractCode::nInstrs];
#endif

void NativeCodeJitter::CompileInstr(TagVal *pc) {
  Assert(pc != INVALID_POINTER);
  do {
    AbstractCode::instr opcode = AbstractCode::GetInstr(pc);
#ifdef INSTRUCTION_COUNTS
    staticCounts[opcode]++;
    jit_ldi_ui(JIT_R0, &dynamicCounts[opcode]);
    jit_addi_ui(JIT_R0, JIT_R0, 1);
    jit_sti_ui(&dynamicCounts[opcode], JIT_R0);
#endif
    switch (opcode) {
    case AbstractCode::Kill:
      pc = InstrKill(pc); break;
    case AbstractCode::PutVar:
      pc = InstrPutVar(pc); break;
    case AbstractCode::PutNew:
      pc = InstrPutNew(pc); break;
    case AbstractCode::PutTag:
      pc = InstrPutTag(pc); break;
    case AbstractCode::PutCon:
      pc = InstrPutCon(pc); break;
    case AbstractCode::PutRef:
      pc = InstrPutRef(pc); break;
    case AbstractCode::PutTup:
      pc = InstrPutTup(pc); break;
    case AbstractCode::PutPolyRec:
      pc = InstrPutPolyRec(pc); break;
    case AbstractCode::PutVec:
      pc = InstrPutVec(pc); break;
    case AbstractCode::Close:
      pc = InstrClose(pc); break;
    case AbstractCode::Specialize:
      pc = InstrSpecialize(pc); break;
    case AbstractCode::AppPrim:
      pc = InstrAppPrim(pc); break;
    case AbstractCode::AppVar:
      pc = InstrAppVar(pc); break;
    case AbstractCode::DirectAppVar:
      pc = InstrAppVar(pc, true); break;
    case AbstractCode::GetRef:
      pc = InstrGetRef(pc); break;
    case AbstractCode::GetTup:
      pc = InstrGetTup(pc); break;
    case AbstractCode::Sel: 
      pc = InstrSel(pc); break;
    case AbstractCode::LazyPolySel:
      pc = InstrLazyPolySel(pc); break;
    case AbstractCode::Raise:
      pc = InstrRaise(pc); break;
    case AbstractCode::Reraise:
      pc = InstrReraise(pc); break;
    case AbstractCode::Try:
      pc = InstrTry(pc); break;
    case AbstractCode::EndTry:
      pc = InstrEndTry(pc); break;
    case AbstractCode::EndHandle:
      pc = InstrEndHandle(pc); break;
    case AbstractCode::IntTest:
      pc = InstrIntTest(pc); break;
    case AbstractCode::CompactIntTest:
      pc = InstrCompactIntTest(pc); break;
    case AbstractCode::RealTest:
      pc = InstrRealTest(pc); break;
    case AbstractCode::StringTest:
      pc = InstrStringTest(pc); break;
    case AbstractCode::TagTest:
      pc = InstrTagTest(pc); break;
    case AbstractCode::CompactTagTest:
      pc = InstrCompactTagTest(pc); break;
    case AbstractCode::ConTest:
      pc = InstrConTest(pc); break;
    case AbstractCode::VecTest:
      pc = InstrVecTest(pc); break;
    case AbstractCode::Shared:
      pc = InstrShared(pc); break;
    case AbstractCode::Return:
      pc = InstrReturn(pc); break;
    default:
      Error("NativeCodeJitter::CompileInstr: invalid abstractCode tag");
    }
  }
  while (pc != INVALID_POINTER);
}

struct InlineEntry {
  INLINED_PRIMITIVE tag;
  const char *name;
};

static InlineEntry inlines[] = {
  { FUTURE_BYNEED, "Future.byneed" },
  { CHAR_ORD,      "Char.ord" },
  { INT_OPPLUS,    "Int.+" },
  { INT_OPSUB,     "Int.-" },
  { INT_OPMUL,     "Int.*" },
  { INT_OPLESS,    "Int.<" },
  { static_cast<INLINED_PRIMITIVE>(0), NULL }
};

::Chunk *NativeCodeJitter::CopyCode(char *start) {
  char *end = jit_get_ip().ptr;
  //  jit_flush_code(start, end);
  u_int size    = (end - start);
  ::Chunk *code = Store::AllocChunk(size, STORE_GEN_OLDEST);
  memcpy(code->GetBase(), start, size);
  Assert(size <= codeBufferSize);
  return code;
}

// NativeCodeJitter Static Constructor
void NativeCodeJitter::Init(u_int bufferSize) {
#ifdef INSTRUCTION_COUNTS
  for (u_int opcode = AbstractCode::nInstrs; opcode--; ) {
    staticCounts[opcode] = 0;
    dynamicCounts[opcode] = 0;
  }
#endif
#if defined(JIT_STORE_DEBUG)
  JITStore::InitLoggging();
#endif
  LazyCompileInterpreter::Init();
  codeBufferSize = bufferSize;
  codeBuffer     = (jit_insn *) malloc(sizeof(jit_insn) * bufferSize);
  ImmediateEnv::Init();
  ActiveSet::Init();
  // InitInlines
  IntMap *inlineMap = IntMap::New(10);
  u_int i = 0;
  do {
    ::Chunk *name = (::Chunk *) (String::New(inlines[i].name));
    word value = PrimitiveTable::LookupValue(name);
    Closure *closure = Closure::FromWordDirect(value);
    ConcreteCode *concreteCode =
      ConcreteCode::FromWord(closure->GetConcreteCode());
    Interpreter *interpreter = concreteCode->GetInterpreter();
    void *cFunction = (void *) interpreter->GetCFunction();
    word wCFunction = Store::UnmanagedPointerToWord(cFunction);
    inlineMap->Put(wCFunction, Store::IntToWord(inlines[i].tag));
  } while (inlines[++i].name != NULL);
  inlineTable = inlineMap->ToWord();
  actualIdRefVector = Vector::New(1)->ToWord();
  RootSet::Add(inlineTable);
  RootSet::Add(actualIdRefVector);
  // Compute Initial PC
  CompileProlog("Dummy Information");
  initialPC = Store::IntToWord(GetRelativePC());
  // Compile Return Continuation
  {
    char *start = jit_set_ip(codeBuffer).ptr;
    Generic::Scheduler::PopFrameReg(JIT_FP);
    JITStore::LoadStatus(JIT_FP);
    jit_movi_ui(JIT_R0, Worker::CONTINUE);
    jit_insn *no_preempt = jit_beqi_ui(jit_forward(), JIT_FP, 0);
    jit_movi_ui(JIT_R0, Worker::PREEMPT);
    jit_patch(no_preempt);
    RETURN();
    NativeCodeInterpreter::returnContinuation = CopyCode(start)->ToWord();
  }
  // Compile Native Continuation
  {
    char *start = jit_set_ip(codeBuffer).ptr;
    Generic::Scheduler::PopFrameReg(JIT_FP);
    JITStore::LoadStatus(JIT_R0);
    jit_insn *no_preempt = jit_beqi_ui(jit_forward(), JIT_R0, 0);
    jit_movi_ui(JIT_R0, Worker::PREEMPT);
    RETURN();
    jit_patch(no_preempt);
    Generic::Scheduler::GetFrame(JIT_V2);
    RestoreRegister();
    NativeCodeFrame::GetPC(JIT_R0, JIT_V2);
    BranchToOffset(JIT_R0);
    NativeCodeInterpreter::nativeContinuation = CopyCode(start)->ToWord();
  }
  RootSet::Add(NativeCodeInterpreter::returnContinuation);
  RootSet::Add(NativeCodeInterpreter::nativeContinuation);
}

// Function of coord * value option vector * string vector *
//             idDef args * instr * liveness
NativeConcreteCode *NativeCodeJitter::Compile(TagVal *abstractCode) {
#if 0
  // Diassemble AbstractCode
  Tuple *coord1 = Tuple::FromWordDirect(abstractCode->Sel(0));
  char *filename = String::FromWordDirect(coord1->Sel(0))->ExportC();
  if (!strcmp(filename, "")) {
  fprintf(stderr, "Disassembling function at %s:%d.%d\n\n",
	  String::FromWordDirect(coord1->Sel(0))->ExportC(),
	  Store::DirectWordToInt(coord1->Sel(1)),
	  Store::DirectWordToInt(coord1->Sel(2)));
  TagVal *pc = TagVal::FromWordDirect(abstractCode->Sel(4));
  AbstractCode::Disassemble(stderr, pc);
  }
#endif
#if defined(JIT_CODE_SIZE_PROFILE)
  static u_int codeSize       = 0;
  static u_int totalSize      = 0;
  static u_int nbProcs        = 0;
  static u_int immediateMax   = 0;
  static u_int immediateTotal = 0;
#endif
  // Setup nodes/immediate value tables
  ImmediateEnv::Init();
  sharedTable = IntMap::New(SHARED_TABLE_SIZE);
  // Start function compilation with prolog
#if defined(JIT_STORE_DEBUG)
  Tuple *coord2 = Tuple::FromWord(abstractCode->Sel(0));
  String *name  = String::FromWord(coord2->Sel(0));
  u_int line    = Store::WordToInt(coord2->Sel(1));
  char info[1024];
  sprintf(info, "%s:%d\n", name->ExportC(), line);
  char *start = CompileProlog(strdup(info));
#else
  char *start = CompileProlog("Dummy info\n");
#endif
  // Perform Register Allocation
  Vector *localNames = Vector::FromWordDirect(abstractCode->Sel(2));
  u_int nLocals = localNames->GetLength();
#if 1
  Vector *liveness = Vector::FromWordDirect(abstractCode->Sel(5));
  assignment = RegisterAllocator::Run(nLocals, liveness);
#if defined(JIT_STORE_DEBUG)
  //RegisterAllocator::Dump(abstractCode->Sel(0), nLocals, assignment);
#endif
  u_int nSlots = MemoryNode::GetNbSlots();
#else
  assignment = Tuple::New(nLocals);
  for (u_int i = 0; i < nLocals; i++)
    assignment->Init(i, Store::IntToWord(i + ALICE_REGISTER_NB));
  u_int nSlots = nLocals + ALICE_REGISTER_NB;
#endif
  currentNLocals      = nSlots;
  currentArgs         = TagVal::FromWord(abstractCode->Sel(3));
  livenessTable       = LivenessTable::New(nSlots);
  livenessFreeList    = NULL;
  u_int frameSize     = NativeCodeFrame::GetFrameSize(currentNLocals);
  ImmediateEnv::Register(Store::IntToWord(frameSize));
  // Compile argument calling convention conversion
  u_int  currentArity = GetArity(currentArgs);
  CompileCCC(currentArity, true);
  StoreResults(currentArity, currentArgs);
  // Initialize global substitution
  Vector *substInfo = Vector::FromWordDirect(abstractCode->Sel(1));
  u_int nSubst      = substInfo->GetLength();
  globalSubst       = Vector::New(nSubst);
  for (u_int i = nSubst; i--;) {
    TagVal *valueOpt = TagVal::FromWord(substInfo->Sub(i));
    TagVal *subst;
    if (valueOpt != INVALID_POINTER) {
      subst = TagVal::New(AbstractCode::Immediate, 1);
      subst->Init(0, valueOpt->Sel(0));
    }
    else {
      subst = TagVal::New(AbstractCode::Global, 1);
      subst->Init(0, Store::IntToWord(i));
    }
    globalSubst->Init(i, subst->ToWord());
  }
  // Compile function body
  CompileInstr(TagVal::FromWordDirect(abstractCode->Sel(4)));
  ::Chunk *code = CopyCode(start);
  // Clear Helper Vector
  Vector::FromWordDirect(actualIdRefVector)->Init(0, Store::IntToWord(0));
#if defined(JIT_CODE_SIZE_PROFILE)
  if (size > codeSize)
    codeSize = size;
  totalSize += size;
  nbProcs++;
  fprintf(stderr, "NativeCodeJitter: %d bytes code in %d procs = %.2f\n",
	  totalSize, nbProcs, ((double) totalSize / (double) nbProcs));
  fflush(stderr);
  immediateTotal += ImmediateEnv::size;
  if (ImmediateEnv::size >= immediateMax)
    immediateMax = ImmediateEnv::size;
  fprintf(stderr, "Jitter: code %d (largest=%d) to %d total\n",
	  size, codeSize, totalSize);
  fprintf(stderr, "Jitter: immediate %d (largest=%d) to %d total\n",
	  ImmediateEnv::size, immediateMax, immediateTotal);
  fprintf(stderr, "Jitter: total of %d procs\n", nbProcs);
  fflush(stderr);
#endif
  // Export ConcreteCode
  return NativeConcreteCode::NewInternal(abstractCode, code,
					 ImmediateEnv::ExportEnv(),
					 Store::IntToWord(nSlots),
					 initialNoCCCPC);
}

#if defined(JIT_STORE_DEBUG)
void NativeCodeJitter::Disassemble(::Chunk *code) {
#if 0
  char *base = code->GetBase();
  disassemble(stderr, base, base + code->GetSize());
#endif
}
#endif

#ifdef INSTRUCTION_COUNTS
void NativeCodeJitter::DumpInstructionCounts() {
  std::fprintf(stderr, "JITter instruction counts (static/dynamic)\n");
  for (u_int opcode = 0; opcode < AbstractCode::nInstrs; opcode++)
    std::fprintf(stderr, "  %s, %d, %d\n",
		 AbstractCode::GetOpcodeName
		   (static_cast<AbstractCode::instr>(opcode)),
		 staticCounts[opcode], dynamicCounts[opcode]);
}
#endif
