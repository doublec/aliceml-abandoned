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
#include "adt/HashTable.hh"
#include "generic/TaskStack.hh"
#include "generic/Scheduler.hh"
#include "generic/Backtrace.hh"
#include "generic/Closure.hh"
#include "generic/ConcreteCode.hh"
#include "generic/Transients.hh"
#include "generic/Transform.hh"
#include "generic/Primitive.hh"
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

#if defined(ALICE_PROFILE)
#include "generic/Profiler.hh"
#endif

// NativeCodeFrame
class NativeCodeFrame : private Generic::StackFrame {
protected:
  static const u_int PC_POS             = 0;
  static const u_int CODE_POS           = 1;
  static const u_int CLOSURE_POS        = 2;
  static const u_int IMMEDIATE_ARGS_POS = 3;
  static const u_int NB_LOCAL_ARGS_POS  = 4;
  static const u_int TASK_STACK_POS     = 5;
  static const u_int BASE_SIZE          = 6;
public:
  static void GetPC(u_int Dest, u_int Ptr) {
    Sel(Dest, Ptr, PC_POS);
  }
  static void PutPC(u_int Ptr, u_int Value) {
    Put(Ptr, PC_POS, Value);
  }
  static void GetCode(u_int Dest, u_int Ptr) {
    Sel(Dest, Ptr, CODE_POS);
  }
  static void GetClosure(u_int Dest, u_int Ptr) {
    Sel(Dest, Ptr, CLOSURE_POS);
  }
  static void GetImmediateArgs(u_int Dest, u_int Ptr) {
    Sel(Dest, Ptr, IMMEDIATE_ARGS_POS);
  }
  static void GetNbLocalArgs(u_int Dest, u_int Ptr) {
    Sel(Dest, Ptr, NB_LOCAL_ARGS_POS);
  }
  static void GetTaskStack(u_int Dest, u_int Ptr) {
    Sel(Dest, Ptr, TASK_STACK_POS);
  }
  static void GetEnv(u_int Dest, u_int Ptr, u_int pos) {
    Sel(Dest, Ptr, BASE_SIZE + pos);
  }
  static void PutEnv(u_int Ptr, u_int pos, u_int Value) {
    Put(Ptr, BASE_SIZE + pos, Value);
  }
  static void ReplaceEnv(u_int Ptr, u_int pos, u_int Value) {
    Replace(Ptr, BASE_SIZE + pos, Value);
  }
};

// NativeCodeHandlerFrame
class NativeCodeHandlerFrame : private Generic::StackFrame {
protected:
  static const u_int PC_POS    = 0;
  static const u_int FRAME_POS = 1;
  static const u_int BASE_SIZE = 2;
public:
  static void New(u_int Ptr) {
    u_int size = Generic::StackFrame::BASE_SIZE + BASE_SIZE;
    JITStore::AllocBlock(Ptr, (BlockLabel) NATIVE_CODE_HANDLER_FRAME, size);
    jit_movi_p(JIT_R0,
	       Store::UnmanagedPointerToWord(NativeCodeInterpreter::self));
    JITStore::InitArg(Ptr, Generic::StackFrame::INTERPRETER_POS, JIT_R0); 
  }
  static void PutPC(u_int Ptr, u_int Value) {
    u_int i = Generic::StackFrame::BASE_SIZE + PC_POS;
    JITStore::InitArg(Ptr, i, Value);
  }
  static void PutFrame(u_int Ptr, u_int Value) {
    u_int i = Generic::StackFrame::BASE_SIZE + FRAME_POS;
    JITStore::InitArg(Ptr, i, Value);
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

static u_int max(u_int a, u_int b) {
  return ((a >= b) ? a : b);
}

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
  static const u_int INDEX_POS = 0;
  static const u_int END_POS   = 1;
  static const u_int NEXT_POS  = 2;
  static const u_int SIZE      = 3;

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
};

// LivenessTable
class LivenessTable : private Chunk {
protected:
  static const char DEAD  = 0;
  static const char ALIVE = 1;
  static const char KILL  = 2;

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
  }
  LivenessTable *Clone() {
    u_int size   = this->GetSize();
    Chunk *clone = Store::AllocChunk(size);
    std::memcpy(clone->GetBase(), this->GetBase(), size);
    return (LivenessTable *) clone;
  }
  void Clone(LivenessTable *clone) {
    u_int size = ((Block *) this)->GetSize();
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

void ImmediateEnv::Put(u_int index, u_int Value) {
  jit_pushr_ui(JIT_V2);
  NativeCodeFrame::GetImmediateArgs(JIT_V2, JIT_V2);
  JITStore::ReplaceArg(JIT_V2, index, Value);
  jit_popr_ui(JIT_V2);
}

//
// NativeCodeJitter Variables
//
jit_insn *NativeCodeJitter::codeBuffer;
u_int NativeCodeJitter::codeBufferSize;
 
word NativeCodeJitter::initialPC;
word NativeCodeJitter::instructionStartPC;
HashTable *NativeCodeJitter::sharedTable;

LivenessTable *NativeCodeJitter::livenessTable;
LivenessTable *NativeCodeJitter::livenessFreeList;
Tuple *NativeCodeJitter::assignment;
#if defined(ALICE_IMPLICIT_KILL)
LivenessInformation *NativeCodeJitter::livenessInfo;
u_int NativeCodeJitter::rowIndex;
#endif

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
  Assert(Dest != JIT_V2);
  jit_movr_p(JIT_FP, JIT_V2);
  NativeCodeFrame::GetClosure(JIT_V2, Ptr);
  Generic::Closure::Sel(JIT_V2, Dest, Store::WordToInt(pos));
  jit_movr_p(JIT_V2, JIT_FP);
}

void NativeCodeJitter::ImmediateSel(u_int Dest, u_int Ptr, u_int pos) {
  Assert(Dest != JIT_V2);
  jit_movr_p(JIT_FP, JIT_V2);
  NativeCodeFrame::GetImmediateArgs(JIT_V2, Ptr);
  JITStore::GetArg(Dest, JIT_V2, pos);
  jit_movr_p(JIT_V2, JIT_FP);
}

// LazySelClosure (belongs to alice, of course)
void NativeCodeJitter::LazySelClosureNew(word tuple, word label) {
  Generic::ConcreteCode::New(JIT_V1, LazySelInterpreter::self, 0);
  jit_pushr_ui(JIT_V1); // Save ConcreteCode Ptr
  Generic::Closure::New(JIT_V1, 3);
  jit_popr_ui(JIT_R0); // Restore ConcreteCode Ptr
  Generic::Closure::InitCC(JIT_V1, JIT_R0);
  u_int reg = LoadIdRefKill(JIT_R0, tuple);
  Generic::Closure::Put(JIT_V1, 0, reg);
  u_int i1 = ImmediateEnv::Register(label);
  ImmediateSel(JIT_R0, JIT_V2, i1);
  Generic::Closure::Put(JIT_V1, 1, JIT_R0);
}

#define RETURN() \
  JITStore::LogReg(JIT_SP); \
  JITStore::LogMesg("returning to base\n"); \
  jit_ret();

void NativeCodeJitter::Prepare() {
  jit_pushr_ui(JIT_R1);
  jit_pushr_ui(JIT_R2);
}

void NativeCodeJitter::Finish() {
  jit_popr_ui(JIT_R2);
  jit_popr_ui(JIT_R1);
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

void NativeCodeJitter::PushCall(u_int Closure) {
  jit_pushr_ui(Closure);
  NativeCodeFrame::GetTaskStack(JIT_R0, JIT_V2);
  jit_pushr_ui(JIT_R0);
  void *ptr = static_cast<Interpreter::Result (*)(TaskStack*,word)>(&TaskStack::PushCall);
  JITStore::Call(2, (void *) ptr);
  RETURN();
}

void NativeCodeJitter::DirectCall(Interpreter *interpreter) {
  NativeCodeFrame::GetTaskStack(JIT_R0, JIT_V2);
  jit_pushr_ui(JIT_R0);
  jit_movi_p(JIT_R0, interpreter);
  jit_pushr_ui(JIT_R0);
  JITStore::Call(2, (void *) Primitive::Execute);
  RETURN();
}

void NativeCodeJitter::TailCall(u_int Closure) {
  JITStore::Deref(Closure);
  jit_pushr_ui(Closure);
  NativeCodeFrame::GetClosure(JIT_V1, JIT_V2);
  JITStore::Deref(JIT_V1);
  jit_popr_ui(JIT_R0); // Restore derefed closure
  jit_insn *self_call = jit_beqr_p(jit_forward(), JIT_V1, JIT_R0);
  jit_pushr_ui(JIT_R0); // Derefed closure
  NativeCodeFrame::GetTaskStack(JIT_V1, JIT_V2);
  Generic::TaskStack::PopFrames(JIT_V1, 1);
  jit_pushr_ui(JIT_V1); // TaskStack
  void *ptr = static_cast<Interpreter::Result (*)(TaskStack*,word)>(&TaskStack::PushCall);
  JITStore::Call(2, ptr);
  RETURN();
  jit_patch(self_call);
  jit_ldi_p(JIT_V1, &NativeCodeJitter::initialPC);
#if defined(ALICE_PROFILE)
  // Profiler requires scheduler to be involved
  NativeCodeFrame::PutPC(JIT_V2, JIT_V1);
  jit_pushr_ui(JIT_V2); // Frame
  JITStore::Call(1, (void *) Profiler::IncCalls);
  RETURN();
#else
  // Jump into the body again
  JITStore::DirectWordToInt(JIT_V1, JIT_V1);
  NativeCodeFrame::GetCode(JIT_R0, JIT_V2);
  jit_addr_ui(JIT_R0, JIT_R0, JIT_V1);
  jit_jmpr(JIT_R0);
#endif
}

void NativeCodeJitter::BranchToOffset(u_int wOffset) {
  Assert(wOffset == JIT_R0);
  NativeCodeFrame::GetCode(JIT_FP, JIT_V2);
  JITStore::DirectWordToInt(wOffset, wOffset);
  jit_addr_ui(wOffset, wOffset, JIT_FP);
  jit_jmpr(wOffset);
}

u_int NativeCodeJitter::GetRelativePC() {
  return ((jit_get_ip().ptr - (char *) codeBuffer) + 2 * sizeof(word));
}

void NativeCodeJitter::PrintPC(const char *instr) {
  //fprintf(stderr, "%s at %p\n", instr, (codeBuffer + GetRelativePC() - 8));
  JITStore::LogMesg(instr);
  JITStore::LogReg(JIT_SP);
}

void NativeCodeJitter::SetRelativePC(word pc) {
  jit_movi_p(JIT_R0, pc);
  NativeCodeFrame::PutPC(JIT_V2, JIT_R0);
}

//
// Calling Convention Conversion
//
void NativeCodeJitter::CompileCCC(TagVal *idDefArgs) {
  switch(AbstractCode::GetArgs(idDefArgs)) {
  case AbstractCode::OneArg:
    {
      JITStore::LogMesg("Interpreter::Construct\n");
      Prepare();
      JITStore::Call(0, (void *) Interpreter::Construct);
      Finish();
      TagVal *idDef = TagVal::FromWord(idDefArgs->Sel(0));
      if (idDef != INVALID_POINTER) {
	Generic::Scheduler::GetZeroArg(JIT_R0);
	LocalEnvPut(JIT_V2, idDef->Sel(0), JIT_R0);
      }
    }
    break;
  case AbstractCode::TupArgs:
    {
      Vector *idDefs = Vector::FromWord(idDefArgs->Sel(0));
      u_int nArgs    = idDefs->GetLength();
      // request unit to be done
      if (nArgs != 0) {
	JITStore::LogMesg("Deconstruct results\n");
	Prepare();
	JITStore::Call(0, (void *) Interpreter::Deconstruct);
	Finish();
	JITStore::LogReg(JIT_RET);
	jit_insn *no_request = jit_beqi_ui(jit_forward(), JIT_R0, 0);
	jit_movi_ui(JIT_RET, Interpreter::REQUEST);
	RETURN();
	jit_patch(no_request);
	Generic::Scheduler::GetCurrentArgs(JIT_V1);
	for (u_int i = idDefs->GetLength(); i--;) {
	  TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
	  if (idDef != INVALID_POINTER) {
	    Generic::Scheduler::SelArg(JIT_R0, JIT_V1, i);
	    LocalEnvPut(JIT_V2, idDef->Sel(0), JIT_R0);
	  }
	}
      }
    }
    break;
  default:
    Error("NativeCodeJitter::CompileCCC: invalid abstractCode tag");
    break;
  }
}

//
// Instruction Helper
//
u_int NativeCodeJitter::LoadIdRefKill(u_int Dest, word idRef) {
  TagVal *tagVal = TagVal::FromWord(idRef);
  switch (AbstractCode::GetIdRef(tagVal)) {
  case AbstractCode::Local:
    {
      u_int index = RefToIndex(tagVal->Sel(0));
      Dest = LocalEnvSel(Dest, JIT_V2, index);
      return Dest;
    }
  case AbstractCode::LastUseLocal:
    {
      u_int index = RefToIndex(tagVal->Sel(0));
      Dest = LocalEnvSel(Dest, JIT_V2, index);
      livenessTable->SetKill(index);
      return Dest;
    }
  case AbstractCode::Global:
    GlobalEnvSel(Dest, JIT_V2, tagVal->Sel(0));
    return Dest;
  case AbstractCode::Immediate:
    {
      word val = tagVal->Sel(0);
      if (PointerOp::IsInt(val)) {
	jit_movi_p(Dest, val);
      }
      else {
	u_int i1 = ImmediateEnv::Register(val);
	ImmediateSel(Dest, JIT_V2, i1);
      }
      return Dest;
    }
  case AbstractCode::Toplevel:
    ImmediateSel(Dest, JIT_V2, Store::DirectWordToInt(tagVal->Sel(0)));
    return Dest;
  default:
    Error("NativeCodeJitter::LoadIdRef: invalid idRef Tag");
  }
}

void NativeCodeJitter::Await(u_int Ptr, word pc) {
  jit_insn *ref[2];
  DerefItem(Ptr, ref);
  jit_patch(ref[0]);
  BlockOnTransient(Ptr, pc);
  jit_patch(ref[1]);
}

u_int NativeCodeJitter::LoadIdRef(u_int Dest, word idRef, word pc) {
  TagVal *tagVal = TagVal::FromWord(idRef);
  switch (AbstractCode::GetIdRef(tagVal)) {
  case AbstractCode::Local:
  case AbstractCode::LastUseLocal:
    {
      u_int index = RefToIndex(tagVal->Sel(0));
      Dest = LocalEnvSel(Dest, JIT_V2, index);
      if (pc != (word) 0)
	Await(Dest, pc);
      return Dest;
    }
  case AbstractCode::Global:
    GlobalEnvSel(Dest, JIT_V2, tagVal->Sel(0));
    if (pc != (word) 0)
      Await(Dest, pc);
    return Dest;
  case AbstractCode::Immediate:
    {
      word val = tagVal->Sel(0);
      if (PointerOp::IsInt(val)) {
	jit_movi_p(Dest, val);
      }
      else {
	u_int i1 = ImmediateEnv::Register(val);
	ImmediateSel(Dest, JIT_V2, i1);
      }
    }
    return Dest;
  case AbstractCode::Toplevel:
    ImmediateSel(Dest, JIT_V2, Store::DirectWordToInt(tagVal->Sel(0)));
    if (pc != (word) 0)
      Await(Dest, pc);
    return Dest;
  default:
    Error("NativeCodeJitter::LoadIdRef: invalid idRef Tag");
  }
}

#if defined(ALICE_IMPLICIT_KILL)
void NativeCodeJitter::KillVariables(word pc) {
  livenessInfo = livenessInfo->AddRow(rowIndex, pc);
  char *row = livenessInfo->GetRow(rowIndex);
  for (u_int i = livenessTable->GetSize(); i--;)
    if (livenessTable->NeedsKill(i))
      livenessInfo->SetRowBit(row, i);
  rowIndex++;
}
#else
void NativeCodeJitter::KillVariables(word) {
  jit_movi_p(JIT_R0, Store::IntToWord(4711));
  for (u_int i = livenessTable->GetSize(); i--;)
    if (livenessTable->NeedsKill(i))
      NativeCodeFrame::PutEnv(JIT_V2, i, JIT_R0);
  SaveRegister();
}
#endif

void NativeCodeJitter::BlockOnTransient(u_int Ptr, word pc) {
  SetRelativePC(pc);
  JITStore::SetTransientTag(Ptr);
  Generic::Scheduler::SetCurrentData(Ptr);
  jit_movi_ui(JIT_R0, 0);
  Generic::Scheduler::PutNArgs(JIT_R0);
  KillVariables(pc);
  jit_movi_ui(JIT_RET, Interpreter::REQUEST);
  RETURN();
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
  PrintPC("PutVar\n");
  u_int reg = LoadIdRefKill(JIT_R0, pc->Sel(1));
  LocalEnvPut(JIT_V2, pc->Sel(0), reg);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// PutNew of id * string * instr
TagVal *NativeCodeJitter::InstrPutNew(TagVal *pc) {
  PrintPC("PutNew\n");
  u_int i1 = ImmediateEnv::Register(pc->Sel(1));
  ImmediateSel(JIT_R0, JIT_V2, i1);
  Prepare();
  jit_pushr_ui(JIT_R0);
  void *ptr = (void *) static_cast<Constructor *(*)(word)>(&Constructor::New);
  JITStore::Call(1, ptr); // Constructor resides in JIT_RET
  Finish();
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_RET);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// PutTag of id * int * idRef vector * instr
TagVal *NativeCodeJitter::InstrPutTag(TagVal *pc) {
  PrintPC("PutTag\n");
  Vector *idRefs = Vector::FromWordDirect(pc->Sel(2));
  u_int nArgs    = idRefs->GetLength();
  JITAlice::TagVal::New(JIT_V1, Store::DirectWordToInt(pc->Sel(1)), nArgs);
  for (u_int i = nArgs; i--;) {
    u_int reg = LoadIdRefKill(JIT_R0, idRefs->Sub(i));
    JITAlice::TagVal::Put(JIT_V1, i, reg);
  }
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_V1);
  return TagVal::FromWordDirect(pc->Sel(3));
}

// PutCon of id * idRef * idRef vector * instr
TagVal *NativeCodeJitter::InstrPutCon(TagVal *pc) {
  word instrPC = Store::IntToWord(GetRelativePC());
  PrintPC("PutCon\n");
  u_int constr = LoadIdRef(JIT_V1, pc->Sel(1), instrPC);
  KillIdRef(pc->Sel(1));
  jit_pushr_ui(constr); // Save Constructor
  Vector *idRefs = Vector::FromWordDirect(pc->Sel(2));
  u_int nArgs    = idRefs->GetLength();
  JITAlice::ConVal::New(JIT_V1, nArgs);
  jit_popr_ui(JIT_R0); // Restore Constructor
  JITAlice::ConVal::InitConstr(JIT_V1, JIT_R0);
  for (u_int i = nArgs; i--;) {
    u_int reg = LoadIdRefKill(JIT_R0, idRefs->Sub(i));
    JITAlice::ConVal::Put(JIT_V1, i, reg);
  }
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_V1);
  return TagVal::FromWordDirect(pc->Sel(3));
}

// PutRef of id * idRef * instr
TagVal *NativeCodeJitter::InstrPutRef(TagVal *pc) {
  PrintPC("PutRef\n");
  JITAlice::Cell::New(JIT_V1);
  u_int reg = LoadIdRefKill(JIT_R0, pc->Sel(1));
  JITAlice::Cell::Put(JIT_V1, reg);
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_V1);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// PutTup of id * idRef vector * instr
TagVal *NativeCodeJitter::InstrPutTup(TagVal *pc) {
  PrintPC("PutTup\n");
  Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
  u_int nArgs    = idRefs->GetLength();
  if (nArgs == 0) {
    jit_movi_p(JIT_V1, Store::IntToWord(0)); // unit
  }
  else {
    Generic::Tuple::New(JIT_V1, nArgs);
    for (u_int i = nArgs; i--;) {
      u_int reg = LoadIdRefKill(JIT_R0, idRefs->Sub(i));
      Generic::Tuple::Put(JIT_V1, i, reg);
    }
  }
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_V1);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// PutPolyRec of id * label vector * idRef vector * instr
TagVal *NativeCodeJitter::InstrPutPolyRec(TagVal *pc) {
  PrintPC("PutPolyRec\n");
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
    u_int reg = LoadIdRefKill(JIT_R0, idRefs->Sub(i));
    JITAlice::Record::InitValue(JIT_V1, i, reg);
  }
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_V1);
  return TagVal::FromWordDirect(pc->Sel(3));
}

// PutVec of id * idRef vector * instr
TagVal *NativeCodeJitter::InstrPutVec(TagVal *pc) {
  PrintPC("PutVec\n");
  Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
  u_int nArgs    = idRefs->GetLength();
  JITAlice::Vector::New(JIT_V1, nArgs);
  for (u_int i = nArgs; i--;) {
    u_int reg = LoadIdRefKill(JIT_R0, idRefs->Sub(i));
    JITAlice::Vector::Put(JIT_V1, i, reg);
  }
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_V1);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// Close of id * idRef vector * value * instr
TagVal *NativeCodeJitter::InstrClose(TagVal *pc) {
  PrintPC("Close\n");
  Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
  u_int nGlobals = idRefs->GetLength();
  Generic::Closure::New(JIT_V1, nGlobals);
  u_int i1 = ImmediateEnv::Register(pc->Sel(2));
  ImmediateSel(JIT_R0, JIT_V2, i1);
  Generic::Closure::InitCC(JIT_V1, JIT_R0);
#if defined(ALICE_PROFILE)
  Prepare();
  jit_pushr_ui(JIT_R0);
  JITStore::Call(1, (void *) Profiler::IncClosures);
  Finish();
#endif
  for (u_int i = nGlobals; i--;) {
    u_int reg = LoadIdRefKill(JIT_R0, idRefs->Sub(i));
    Generic::Closure::Put(JIT_V1, i, reg);
  }
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_V1);
  return TagVal::FromWordDirect(pc->Sel(3));
}

// Specialize of id * idRef vector * template * instr
// template = Template of coord * int * int * idDef args * instr * liveness
// abstractcode:
// Specialized of coord * value vector * int * idDef args * instr * liveness
// Design options: call NativeCodeConctructor directly or using
// AliceLanguageLayer::concreteCodeConstructor
TagVal *NativeCodeJitter::InstrSpecialize(TagVal *pc) {
  PrintPC("Specialize\n");
  // Create specialized abstractCode
  JITAlice::TagVal::New(JIT_V1, AbstractCode::Specialized, 6);
  jit_pushr_ui(JIT_V0); // Save V0
  u_int i1 = ImmediateEnv::Register(pc->Sel(2)); // Save template_
  ImmediateSel(JIT_V0, JIT_V2, i1); // Load template_
#if defined(ALICE_PROFILE)
  Prepare();
  jit_pushr_ui(JIT_V0);
  JITStore::Call(1, (void *) Profiler::IncInstances);
  Finish();
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
  // Create Value Vector
  Vector *idRefs   = Vector::FromWordDirect(pc->Sel(1));
  u_int nToplevels = idRefs->GetLength();
  JITAlice::Vector::New(JIT_V1, nToplevels);
  for (u_int i = nToplevels; i--;) {
    u_int reg = LoadIdRefKill(JIT_R0, idRefs->Sub(i));
    JITAlice::Vector::Put(JIT_V1, i, reg);
  }
  jit_movr_p(JIT_R0, JIT_V1); // Move value vector to R0
  jit_popr_ui(JIT_V1); // Restore abstractCode
  JITAlice::TagVal::Put(JIT_V1, 1, JIT_R0); // Store value vector
  Prepare();
  jit_pushr_ui(JIT_V1); // abstractCode
  Generic::Closure::New(JIT_V1, 0);
  JITStore::Call(1, (void *) AliceLanguageLayer::concreteCodeConstructor);
  Finish();
  Generic::Closure::InitCC(JIT_V1, JIT_RET);
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_V1);
  return TagVal::FromWordDirect(pc->Sel(3));
}

#if defined(JIT_STORE_DEBUG)
static const char *GetPrimitveName(word closureWord) {
  Closure *closure      = Closure::FromWord(closureWord);
  word concreteCodeWord = closure->GetConcreteCode();
  Assert(Store::WordToTransient(concreteCodeWord) == INVALID_POINTER);
  ConcreteCode *concreteCode = ConcreteCode::FromWord(concreteCodeWord);
  Interpreter *interpreter   = concreteCode->GetInterpreter();
  return interpreter->Identify();
}
#endif

// AppPrim of value * idRef vector * (idDef * instr) option
TagVal *NativeCodeJitter::InstrAppPrim(TagVal *pc) {
  PrintPC("AppPrim\n");
#if defined(JIT_STORE_DEBUG)
  JITStore::LogMesg(GetPrimitveName(pc->Sel(0)));
#endif
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
  else {
    NativeCodeFrame::GetTaskStack(JIT_V1, JIT_V2);
    Generic::TaskStack::PopFrames(JIT_V1, 1);
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
  Closure *closure = Closure::FromWord(pc->Sel(0));
  ConcreteCode *concreteCode =
    ConcreteCode::FromWord(closure->GetConcreteCode());
  Interpreter *interpreter = concreteCode->GetInterpreter();
  if (idDefInstrOpt != INVALID_POINTER)
    KillVariables(contPC);
#if defined(ALICE_PROFILE)
  u_int i1 = ImmediateEnv::Register(closure->ToWord());
  ImmediateSel(JIT_V1, JIT_V2, i1);
  PushCall(JIT_V1);
#else
  DirectCall(interpreter);
#endif
  return INVALID_POINTER;
}

// App(Var|Const) of idRef * idRef args * (idDef args * instr) option
TagVal *NativeCodeJitter::InstrAppVar(TagVal *pc) {
  PrintPC("AppVar\n");
  word instrPC  = Store::IntToWord(GetRelativePC());
  u_int closure = LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  jit_pushr_ui(closure); // Save Closure
  TagVal *idDefArgsInstrOpt = TagVal::FromWord(pc->Sel(2));
  word contPC = Store::IntToWord(0);
  if (idDefArgsInstrOpt != INVALID_POINTER) { // SOME ...
    JITStore::LogMesg("non-tail call\n");
    Tuple *idDefArgsInstr = Tuple::FromWordDirect(idDefArgsInstrOpt->Sel(0));
    jit_insn *docall      = jit_jmpi(jit_forward());
    contPC = Store::IntToWord(GetRelativePC());
    CompileCCC(TagVal::FromWordDirect(idDefArgsInstr->Sel(0)));
    CompileBranch(TagVal::FromWordDirect(idDefArgsInstr->Sel(1)));
    jit_patch(docall);
    SetRelativePC(contPC);
  }
  // Load arguments
  TagVal *actualArgs = TagVal::FromWordDirect(pc->Sel(1));
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
  JITStore::LogMesg("created arguments\n");
  KillIdRef(pc->Sel(0));
  jit_popr_ui(JIT_V1); // Restore Closure
  if (idDefArgsInstrOpt != INVALID_POINTER) {
    KillVariables(contPC);
    PushCall(JIT_V1);
  }
  else
    TailCall(JIT_V1);
  return INVALID_POINTER;
}

// GetRef of id * idRef * instr
TagVal *NativeCodeJitter::InstrGetRef(TagVal *pc) {
  PrintPC("GetRef\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  u_int cell   = LoadIdRef(JIT_V1, pc->Sel(1), instrPC);
  KillIdRef(pc->Sel(1));
  JITAlice::Cell::Sel(JIT_R0, cell);
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_R0);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// GetTup of idDef vector * idRef * instr
TagVal *NativeCodeJitter::InstrGetTup(TagVal *pc) {
  PrintPC("GetTup\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  u_int tuple  = LoadIdRef(JIT_V1, pc->Sel(1), instrPC);
  KillIdRef(pc->Sel(1));
  Vector *idDefs = Vector::FromWordDirect(pc->Sel(0));
  u_int nArgs    = idDefs->GetLength();
  if (nArgs != 0)
    for (u_int i = nArgs; i--;) {
      TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
      if (idDef != INVALID_POINTER) {
	Generic::Tuple::Sel(JIT_R0, tuple, i);
	LocalEnvPut(JIT_V2, idDef->Sel(0), JIT_R0);
      }
    }
  return TagVal::FromWordDirect(pc->Sel(2));
}

// Sel of id * idRef * int * instr
TagVal *NativeCodeJitter::InstrSel(TagVal *pc) {
  PrintPC("Sel\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  u_int tuple  = LoadIdRef(JIT_V1, pc->Sel(1), instrPC);
  KillIdRef(pc->Sel(1));
  Generic::Tuple::Sel(JIT_R0, tuple, Store::DirectWordToInt(pc->Sel(2)));
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_R0);
  return TagVal::FromWordDirect(pc->Sel(3));
}

// LazyPolySel of id * idRef * label * instr
// to be done: Eager, if determined
TagVal *NativeCodeJitter::InstrLazyPolySel(TagVal *pc) {
  PrintPC("LazyPolySel\n");
  LazySelClosureNew(pc->Sel(1), pc->Sel(2));
  jit_pushr_ui(JIT_V1); // Save Closure Ptr
  JITStore::AllocTransient(JIT_V1, BYNEED_LABEL);
  jit_popr_ui(JIT_R0); // Restore Closure Ptr
  JITStore::InitArg(JIT_V1, 0, JIT_R0);
  JITStore::SetTransientTag(JIT_V1);
  LocalEnvPut(JIT_V2, pc->Sel(0), JIT_V1);
  return TagVal::FromWordDirect(pc->Sel(3));
}

// Raise of idRef
TagVal *NativeCodeJitter::InstrRaise(TagVal *pc) {
  PrintPC("Raise\n");
  u_int reg = LoadIdRefKill(JIT_R0, pc->Sel(0));
  Generic::Scheduler::SetCurrentData(reg);
  Prepare();
  jit_pushr_ui(JIT_V2); // Frame ptr
  JITStore::Call(1, (void *) Backtrace::New);
  Finish();
  Generic::Scheduler::SetCurrentBacktrace(JIT_RET);
  jit_movi_ui(JIT_RET, Interpreter::RAISE);
  RETURN();
  return INVALID_POINTER;
}

// Reraise of idRef
TagVal *NativeCodeJitter::InstrReraise(TagVal *pc) {
  PrintPC("Reraise\n");
  u_int reg = LoadIdRefKill(JIT_V1, pc->Sel(0));
  // DirectWordToBlock(JIT_V1) does nothing
  Generic::Tuple::Sel(JIT_R0, reg, 0);
  Generic::Scheduler::SetCurrentData(JIT_R0);
  Generic::Tuple::Sel(JIT_R0, reg, 1);
  Generic::Scheduler::SetCurrentBacktrace(JIT_R0);
  jit_movi_ui(JIT_RET, Interpreter::RAISE);
  RETURN();
  return INVALID_POINTER;
}

static void PushHandlerFrame(TaskStack *taskStack,
			     word codeFrame,
			     word handlerFrame) {
  taskStack->PopFrame();
  taskStack->PushFrame(handlerFrame);
  taskStack->PushFrame(codeFrame);
}

TagVal *NativeCodeJitter::InstrTry(TagVal *pc) {
  PrintPC("Try\n");
  NativeCodeHandlerFrame::New(JIT_V1);
  u_int handlerPC = ImmediateEnv::Register(Store::IntToWord(0));
  ImmediateSel(JIT_R0, JIT_V2, handlerPC);
  NativeCodeHandlerFrame::PutPC(JIT_V1, JIT_R0);
  NativeCodeHandlerFrame::PutFrame(JIT_V1, JIT_V2);
  NativeCodeFrame::GetTaskStack(JIT_R0, JIT_V2);
  Prepare();
  jit_pushr_ui(JIT_V1); // NativeCodeHandler Frame
  jit_pushr_ui(JIT_V2); // NativeCode Frame
  jit_pushr_ui(JIT_R0); // TaskStack Ptr
  JITStore::Call(3, (void *) PushHandlerFrame);
  Finish();
  CompileBranch(TagVal::FromWordDirect(pc->Sel(0)));
  ImmediateEnv::Replace(handlerPC, Store::IntToWord(GetRelativePC()));
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

static void PopHandlerFrame(TaskStack *taskStack) {
  word frame = taskStack->GetFrame();
  taskStack->PopFrame();
  taskStack->PopFrame();
  taskStack->PushFrame(frame);
}

// EndTry of instr
TagVal *NativeCodeJitter::InstrEndTry(TagVal *pc) {
  PrintPC("EndTry\n");
  NativeCodeFrame::GetTaskStack(JIT_V1, JIT_V2);
  Prepare();
  jit_pushr_ui(JIT_V1); // TaskStack Ptr
  JITStore::Call(1, (void *) PopHandlerFrame);
  Finish();
  return TagVal::FromWordDirect(pc->Sel(0));
}

// EndHandle of instr
TagVal *NativeCodeJitter::InstrEndHandle(TagVal *pc) {
  PrintPC("EndHandle\n");
  return TagVal::FromWordDirect(pc->Sel(0));
}

// Test Helpers
static void *LookupTable(HashTable *table, word key) {
  if (table->IsMember(key))
    return table->GetItem(key);
  else
    return 0;
}

void NativeCodeJitter::LookupTestTable(u_int Key, u_int table) {
  Prepare();
  jit_pushr_ui(Key); // Key Argument
  ImmediateSel(JIT_R0, JIT_V2, table);
  jit_pushr_ui(JIT_R0); // Table Argument
  JITStore::Call(2, (void *) LookupTable);
  Finish();
}

// IntTest of idRef * (int * instr) vector * instr
TagVal *NativeCodeJitter::InstrIntTest(TagVal *pc) {
  PrintPC("IntTest\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  u_int intVal = LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  KillIdRef(pc->Sel(0));
  Vector *tests    = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests     = tests->GetLength();
  HashTable *table = HashTable::New(HashTable::INT_KEY, nTests * 2);
  u_int i1         = ImmediateEnv::Register(table->ToWord());
  LookupTestTable(intVal, i1);
  jit_insn *else_ref = jit_beqi_ui(jit_forward(), JIT_RET, 0);
  BranchToOffset(JIT_RET);
  // Create Branches (order is significant)
  for (u_int i = 0; i < nTests; i++) {
    Tuple *pair  = Tuple::FromWordDirect(tests->Sub(i));
    word key     = pair->Sel(0);
    u_int offset = GetRelativePC();
    table->InsertItem(key, Store::IntToWord(offset));
    CompileBranch(TagVal::FromWordDirect(pair->Sel(1)));
  }
  jit_patch(else_ref);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// CompactIntTest of idRef * int * instrs * instr
TagVal *NativeCodeJitter::InstrCompactIntTest(TagVal *pc) {
  PrintPC("CompactIntTest\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  u_int intVal = LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  KillIdRef(pc->Sel(0));
  JITStore::DirectWordToInt(JIT_R0, intVal);
  int indexOffset = Store::DirectWordToInt(pc->Sel(1));
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
  PrintPC("RealTest\n");
  word instrPC  = Store::IntToWord(GetRelativePC());
  u_int realVal = LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  KillIdRef(pc->Sel(0));
  Vector *tests    = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests     = tests->GetLength();
  HashTable *table = HashTable::New(HashTable::BLOCK_KEY, nTests * 2);
  u_int i1         = ImmediateEnv::Register(table->ToWord());
  LookupTestTable(realVal, i1);
  jit_insn *else_ref = jit_beqi_ui(jit_forward(), JIT_RET, 0);
  BranchToOffset(JIT_RET);
  // Create Branches (order is significant)
  for (u_int i = 0; i < nTests; i++) {
    Tuple *pair  = Tuple::FromWordDirect(tests->Sub(i));
    word key     = pair->Sel(0);
    u_int offset = GetRelativePC();
    table->InsertItem(key, Store::IntToWord(offset));
    CompileBranch(TagVal::FromWordDirect(pair->Sel(1)));
  }
  jit_patch(else_ref);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// StringTest of idRef * (string * instr) vector * instr
TagVal *NativeCodeJitter::InstrStringTest(TagVal *pc) {
  PrintPC("StringTest\n");
  word instrPC    = Store::IntToWord(GetRelativePC());
  u_int stringVal = LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  KillIdRef(pc->Sel(0));
  Vector *tests    = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests     = tests->GetLength();
  HashTable *table = HashTable::New(HashTable::BLOCK_KEY, nTests * 2);
  u_int i1         = ImmediateEnv::Register(table->ToWord());
  LookupTestTable(stringVal, i1);
  jit_insn *else_ref = jit_beqi_ui(jit_forward(), JIT_RET, 0);
  BranchToOffset(JIT_RET);
  // Create Branches (order is significant)
  for (u_int i = 0; i < nTests; i++) {
    Tuple *pair  = Tuple::FromWordDirect(tests->Sub(i));
    word key     = pair->Sel(0);
    u_int offset = GetRelativePC();
    table->InsertItem(key, Store::IntToWord(offset));
    CompileBranch(TagVal::FromWordDirect(pair->Sel(1)));
  }
  jit_patch(else_ref);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// TagTest of idRef * (int * instr) vector
//         * (int * idDef vector * instr) vector * instr
TagVal *NativeCodeJitter::InstrTagTest(TagVal *pc) {
  PrintPC("TagTest\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  u_int tagVal = LoadIdRef(JIT_V1, pc->Sel(0), (word) 0);
  jit_insn *ref[2];
  Deref3(tagVal, ref);
  // Integer branch (V1 is int word, nullary constructor)
  KillIdRef(pc->Sel(0));
  Vector *tests1 = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests1  = tests1->GetLength();
  jit_insn *else_ref1;
  if (nTests1 == 0) {
    else_ref1 = jit_jmpi(jit_forward());
  }
  else {
    HashTable *table1 = HashTable::New(HashTable::INT_KEY, nTests1 * 2);
    u_int i1          = ImmediateEnv::Register(table1->ToWord());
    LookupTestTable(tagVal, i1);
    else_ref1 = jit_beqi_ui(jit_forward(), JIT_RET, 0);
    BranchToOffset(JIT_RET);
    // Create Branches (order is significant)
    for (u_int i = 0; i < nTests1; i++) {
      Tuple *pair  = Tuple::FromWordDirect(tests1->Sub(i));
      word key     = pair->Sel(0);
      u_int offset = GetRelativePC();
      table1->InsertItem(key, Store::IntToWord(offset));
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
    HashTable *table2 = HashTable::New(HashTable::INT_KEY, nTests2 * 2);
    u_int i2          = ImmediateEnv::Register(table2->ToWord());
    LookupTestTable(JIT_R0, i2);
    jit_insn *else_ref2 = jit_beqi_ui(jit_forward(), JIT_RET, 0);
    BranchToOffset(JIT_RET);
    // Create Branches (order is significant)
    for (u_int i = 0; i < nTests2; i++) {
      Tuple *triple  = Tuple::FromWordDirect(tests2->Sub(i));
      word key       = triple->Sel(0);
      u_int offset   = GetRelativePC();
      table2->InsertItem(key, Store::IntToWord(offset));
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

// CompactTagTest of idRef * tagTests * instr
TagVal *NativeCodeJitter::InstrCompactTagTest(TagVal *pc) {
  PrintPC("CompactTagTest\n");
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
  Vector *tests      = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests       = tests->GetLength();
  jit_insn *else_ref = jit_bgei_ui(jit_forward(), JIT_R0, nTests);
  Tuple *branches    = Tuple::New(nTests);
  u_int i1           = ImmediateEnv::Register(branches->ToWord());
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
  jit_patch(else_ref);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// ConTest of idRef * (idRef * instr) vector
//         * (idRef * idDef vector * instr) vector * instr
TagVal *NativeCodeJitter::InstrConTest(TagVal *pc) {
  PrintPC("ConTest\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  u_int convalSlot = ImmediateEnv::Register(Store::IntToWord(0));
  u_int constrSlot = ImmediateEnv::Register(Store::IntToWord(0));
  u_int conVal = LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  ImmediateEnv::Put(convalSlot, conVal);
  JITStore::Block::GetLabel(JIT_R0, conVal);
  jit_insn *nullary_constr = jit_bnei_ui(jit_forward(), JIT_R0, Alice::ConVal);
  JITAlice::ConVal::GetConstructor(JIT_R0, conVal);
  ImmediateEnv::Put(constrSlot, JIT_R0);
  Vector *tests1 = Vector::FromWordDirect(pc->Sel(2));
  u_int nTests1  = tests1->GetLength();
  for (u_int i = 0; i < nTests1; i++) {
    Tuple *triple = Tuple::FromWordDirect(tests1->Sub(i));
    u_int constr = LoadIdRef(JIT_V1, triple->Sel(0), instrPC);
    ImmediateSel(JIT_R0, JIT_V2, constrSlot);
    jit_insn *next_test_ref = jit_bner_ui(jit_forward(), constr, JIT_R0);
    Vector *idDefs          = Vector::FromWordDirect(triple->Sel(1));
    ImmediateSel(JIT_V1, JIT_V2, convalSlot);
    for (u_int i = idDefs->GetLength(); i--;) {
      TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
      if (idDef != INVALID_POINTER) {
	JITAlice::ConVal::Sel(JIT_R0, JIT_V1, i);
	LocalEnvPut(JIT_V2, idDef->Sel(0), JIT_R0);
      }
      KillIdRef(pc->Sel(0)); // Some kills missing
      CompileBranch(TagVal::FromWordDirect(triple->Sel(2)));
    }
    jit_patch(next_test_ref);
  }
  // Nullary Constructor
  jit_patch(nullary_constr);
  Vector *tests2 = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests2 = tests2->GetLength();
  for (u_int i = 0; i < nTests2; i++) {
    Tuple *pair = Tuple::FromWordDirect(tests2->Sub(i));
    u_int constr = LoadIdRef(JIT_V1, pair->Sel(0), instrPC);
    ImmediateSel(JIT_R0, JIT_V2, convalSlot);
    jit_insn *next_test_ref = jit_bner_ui(jit_forward(), constr, JIT_R0);
    KillIdRef(pc->Sel(0)); // Some kills missing
    CompileBranch(TagVal::FromWordDirect(pair->Sel(1)));
    jit_patch(next_test_ref);
  }
  return TagVal::FromWordDirect(pc->Sel(3));
}

// VecTest of idRef * (idDef vector * instr) vector * instr
TagVal *NativeCodeJitter::InstrVecTest(TagVal *pc) {
  PrintPC("VecTest\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  u_int vecVal = LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  KillIdRef(pc->Sel(0));
  jit_pushr_ui(vecVal); // Save vector ptr
  JITAlice::Vector::GetLength(JIT_R0, vecVal);
  Vector *tests    = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests     = tests->GetLength();
  HashTable *table = HashTable::New(HashTable::INT_KEY, nTests * 2);
  u_int i1         = ImmediateEnv::Register(table->ToWord());
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
    table->InsertItem(key, Store::IntToWord(offset));
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
  PrintPC("InstrShared\n");
  word stamp = pc->Sel(0);
  if (sharedTable->IsMember(stamp)) {
    u_int offset = Store::DirectWordToInt(sharedTable->GetItem(stamp));
    drop_jit_jmpi(codeBuffer + offset);
    return INVALID_POINTER;
  }
  else {
    u_int offset = jit_get_ip().ptr - (char *) codeBuffer;
    sharedTable->InsertItem(stamp, Store::IntToWord(offset));
    return TagVal::FromWordDirect(pc->Sel(1));
  }
}

// Return of idRef args
TagVal *NativeCodeJitter::InstrReturn(TagVal *pc) {
  PrintPC("Return\n");
  TagVal *returnArgs = TagVal::FromWordDirect(pc->Sel(0));
  switch (AbstractCode::GetArgs(returnArgs)) {
  case AbstractCode::OneArg:
    {
      jit_movi_ui(JIT_R0, Scheduler::ONE_ARG);
      Generic::Scheduler::PutNArgs(JIT_R0);
      u_int reg = LoadIdRefKill(JIT_R0, returnArgs->Sel(0));
      Generic::Scheduler::PutZeroArg(reg);
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
	  u_int reg = LoadIdRefKill(JIT_R0, returnIdRefs->Sub(i));
	  Generic::Scheduler::PutArg(JIT_V1, i, reg);
	}
      }
      else {
	Generic::Tuple::New(JIT_V1, nArgs);
	for (u_int i = nArgs; i--;) {
	  u_int reg = LoadIdRefKill(JIT_R0, returnIdRefs->Sub(i));
	  Generic::Tuple::Put(JIT_V1, i, reg);
	}
	Generic::Scheduler::PutZeroArg(JIT_V1);
	jit_movi_ui(JIT_R0, Scheduler::ONE_ARG);
	Generic::Scheduler::PutNArgs(JIT_R0);
      }
    }
    break;
  }
  NativeCodeFrame::GetTaskStack(JIT_V1, JIT_V2);
  Generic::TaskStack::PopFrames(JIT_V1, 1);
  jit_movi_ui(JIT_R0, Interpreter::CONTINUE);
  jit_ldi_ui(JIT_R1, &Scheduler::preempt);
  JITStore::NeedGC(JIT_R2);
  jit_addi_ui(JIT_R1, JIT_R1, JIT_R2);
  jit_insn *no_preempt = jit_beqi_ui(jit_forward(), JIT_R1, 0);
  jit_movi_ui(JIT_R0, Interpreter::PREEMPT);
  jit_patch(no_preempt);
  RETURN();
  return INVALID_POINTER;
}
 
char *NativeCodeJitter::CompileProlog(const char *info) {
  char *start = jit_set_ip(codeBuffer).ptr;
  jit_prolog(1);
  int arg1 = jit_arg_p();
  jit_getarg_p(JIT_V2, arg1);
  JITStore::LogMesg(info);
  JITStore::LogReg(JIT_SP);
  NativeCodeFrame::GetPC(JIT_R0, JIT_V2); // PC is base-adjusted
  DirectWordToInt(JIT_R0, JIT_R0);
  NativeCodeFrame::GetCode(JIT_V1, JIT_V2);
  jit_addr_ui(JIT_R0, JIT_R0, JIT_V1);
  JITStore::LogMesg("branching to ");
  JITStore::LogReg(JIT_R0);
  RestoreRegister();
  jit_jmpr(JIT_R0);
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

void NativeCodeJitter::CompileInstr(TagVal *pc) {
  while (pc != INVALID_POINTER) {
    switch (AbstractCode::GetInstr(pc)) {
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
    case AbstractCode::DirectAppVar:
      pc = InstrAppVar(pc); break;
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
    if (pc != INVALID_POINTER)
      JITStore::LogReg(JIT_SP);
  }
}

// NativeCodeJitter Static Constructor
void NativeCodeJitter::Init(u_int bufferSize) {
  JITStore::InitLoggging();
  LazyCompileInterpreter::Init();
  codeBufferSize = bufferSize;
  codeBuffer     = (jit_insn *) malloc(sizeof(jit_insn) * bufferSize);
  ImmediateEnv::Init();
  ActiveSet::Init();
  // Compute Initial PC
  CompileProlog("Dummy Information");
  initialPC = Store::IntToWord(GetRelativePC());
}

// Function of coord * int * int * idDef args * instr * liveness
// Specialized of coord * value vector * int * idDef args * instr * liveness
NativeConcreteCode *NativeCodeJitter::Compile(TagVal *abstractCode) {
#if 0
  // Diassemble AbstractCode
  Tuple *coord = Tuple::FromWordDirect(abstractCode->Sel(0));
  fprintf(stderr, "Disassembling function at %s:%d.%d\n\n",
	  String::FromWordDirect(coord->Sel(0))->ExportC(),
	  Store::DirectWordToInt(coord->Sel(1)),
	  Store::DirectWordToInt(coord->Sel(2)));
  TagVal *pc = TagVal::FromWordDirect(abstractCode->Sel(4));
  AbstractCode::Disassemble(stderr, pc);
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
  sharedTable  = HashTable::New(HashTable::INT_KEY, SHARED_TABLE_SIZE);
  // Start function compilation with prolog
#if defined(JIT_STORE_DEBUG)
  Tuple *coord = Tuple::FromWord(abstractCode->Sel(0));
  String *name = String::FromWord(coord->Sel(0));
  u_int line   = Store::WordToInt(coord->Sel(1));
  char info[1024];
  sprintf(info, "%s:%d\n", name->ExportC(), line);
  char *start = CompileProlog(strdup(info));
#else
  char *start = CompileProlog("Dummy info\n");
#endif
  // Transfer toplevel values to immediate env
  switch (AbstractCode::GetAbstractCode(abstractCode)) {
  case AbstractCode::Function:
    break;
  case AbstractCode::Specialized:
    {
      Vector *values = Vector::FromWordDirect(abstractCode->Sel(1));
      u_int nValues  = values->GetLength();
      for (u_int i = 0; i < nValues; i++)
	ImmediateEnv::Register(values->Sub(i));
    }
    break;
  default:
    Error("NativeCodeJitter::Compile: invalid abstractCode tag");
  };
  // Perform Register Allocation
  u_int nLocals = Store::DirectWordToInt(abstractCode->Sel(2));
#if 1
  Vector *liveness = Vector::FromWordDirect(abstractCode->Sel(5));
  assignment = RegisterAllocator::Run(nLocals, liveness);
#if defined(JIT_STORE_DEBUG)
  RegisterAllocator::Dump(abstractCode->Sel(0), nLocals, assignment);
#endif
  u_int nSlots = MemoryNode::GetNbSlots();
#else
  assignment = Tuple::New(nLocals);
  for (u_int i = 0; i < nLocals; i++)
    assignment->Init(i, Store::IntToWord(i + ALICE_REGISTER_NB));
  u_int nSlots = nLocals + ALICE_REGISTER_NB;
#endif
  livenessTable    = LivenessTable::New(nSlots);
  livenessFreeList = NULL;
#if defined(ALICE_IMPLICIT_KILL)
  livenessInfo = LivenessInformation::New(10, nSlots);
  rowIndex     = 0;
#endif
  // Compile argument calling convention conversion
  CompileCCC(TagVal::FromWord(abstractCode->Sel(3)));
  // Compile function body
  CompileInstr(TagVal::FromWordDirect(abstractCode->Sel(4)));
  char *end = jit_get_ip().ptr;
  jit_flush_code(start, end);
  // Copy generated code
  u_int size    = (end - start);
  ::Chunk *code = Store::AllocChunk(size);
  memcpy(code->GetBase(), start, size);
  Assert(size <= codeBufferSize);
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
#if defined(ALICE_IMPLICIT_KILL)
  livenessInfo->SetN(rowIndex);
  //LivenessInformation *rows = livenessInfo->CloneRows(rowIndex);
  return NativeConcreteCode::NewInternal(abstractCode, code,
					 ImmediateEnv::ExportEnv(),
					 livenessInfo->ToWord(),
					 Store::IntToWord(nSlots));
#else
  return NativeConcreteCode::NewInternal(abstractCode, code,
					 ImmediateEnv::ExportEnv(),
					 Store::IntToWord(nSlots));
#endif
}

#if defined(JIT_STORE_DEBUG)
void NativeCodeJitter::Disassemble(::Chunk *code) {
  char *base = code->GetBase();
  disassemble(stderr, base, base + code->GetSize());
}
#endif
