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
#pragma implementation "alice/JitterGenericData.hh"
#pragma implementation "alice/JitterAliceData.hh"
#pragma implementation "alice/JitterImmediateEnv.hh"
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
#include "alice/Data.hh"
#include "alice/AbstractCode.hh"
#include "alice/LazySelInterpreter.hh"
#include "alice/NativeCodeInterpreter.hh"
#include "alice/NativeCodeJitter.hh"

#include "alice/JitterGenericData.hh"
#include "alice/JitterAliceData.hh"
#include "alice/JitterImmediateEnv.hh"
#include "alice/AliceLanguageLayer.hh"

#include "generic/Debug.hh"

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
  static void GetPC(u_int Dest) {
    Sel(Dest, PC_POS);
  }
  static void PutPC(u_int Value) {
    Put(PC_POS, Value);
  }
  static void GetCode(u_int Dest) {
    Sel(Dest, CODE_POS);
  }
  static void GetClosure(u_int Dest) {
    Sel(Dest, CLOSURE_POS);
  }
  static void GetImmediateArgs(u_int Dest) {
    Sel(Dest, IMMEDIATE_ARGS_POS);
  }
  static void GetNbLocalArgs(u_int Dest) {
    Sel(Dest, NB_LOCAL_ARGS_POS);
  }
  static void GetTaskStack(u_int Dest) {
    Sel(Dest, TASK_STACK_POS);
  }
  static void GetEnv(u_int Dest, u_int pos) {
    Sel(Dest, BASE_SIZE + pos);
  }
  static void PutEnv(u_int pos, u_int Value) {
    Put(BASE_SIZE + pos, Value);
  }
};

// NativeCodeHandlerFrame
class NativeCodeHandlerFrame : private Generic::StackFrame {
protected:
  static const u_int PC_POS    = 0;
  static const u_int FRAME_POS = 1;
  static const u_int BASE_SIZE = 2;
public:
  static void New() {
    u_int size = Generic::StackFrame::BASE_SIZE + BASE_SIZE;
    JITStore::AllocBlock((BlockLabel) NATIVE_CODE_HANDLER_FRAME, size);
    jit_movi_p(JIT_R0,
	       Store::UnmanagedPointerToWord(NativeCodeInterpreter::self));
    JITStore::InitArg(JIT_V1, Generic::StackFrame::INTERPRETER_POS, JIT_R0); 
  }
  static void PutPC(u_int Value) {
    u_int i = Generic::StackFrame::BASE_SIZE + PC_POS;
    JITStore::InitArg(JIT_V1, i, Value);
  }
  static void PutFrame(u_int Value) {
    u_int i = Generic::StackFrame::BASE_SIZE + FRAME_POS;
    JITStore::InitArg(JIT_V1, i, Value);
  }
};

namespace JITAlice {
  class NativeConcreteCode {
  protected:
  static const u_int TRANSFORM_POS     = 0;
  static const u_int NATIVE_CODE_POS   = 1;
  static const u_int IMMEDIATE_ENV_POS = 2;
  static const u_int NLOCALS_POS       = 3;
  static const u_int SIZE              = 4;
  public:
    static void New() {
      Generic::ConcreteCode::New(NativeCodeInterpreter::self, SIZE);
    }
    static void PutTransform(u_int Value) {
      Generic::ConcreteCode::Put(TRANSFORM_POS, Value);
    }
    static void PutNLocals(u_int Value) {
      Generic::ConcreteCode::Put(NLOCALS_POS, Value);
    }
  };
};

//
// ImmediateEnv Variables
//
u_int ImmediateEnv::index;
HashTable *ImmediateEnv::valueTable;

//
// NativeCodeJitter Variables
//
jit_insn *NativeCodeJitter::codeBuffer;
u_int NativeCodeJitter::codeBufferSize;
 
word NativeCodeJitter::initialPC;
word NativeCodeJitter::instructionStartPC;
HashTable *NativeCodeJitter::sharedTable;

Tuple *NativeCodeJitter::livenessTable;

//
// Environment Accessors
//
void NativeCodeJitter::LocalEnvSel(u_int Dest, word pos) {
  NativeCodeFrame::GetEnv(Dest, Store::WordToInt(pos));
}

void NativeCodeJitter::LocalEnvPut(word pos, u_int Value) {
  u_int n = Store::WordToInt(pos);
  livenessTable->Init(n, Store::IntToWord(1));
  NativeCodeFrame::PutEnv(n, Value);
}

void NativeCodeJitter::LocalEnvKill(word pos) {
  u_int n = Store::WordToInt(pos);
  livenessTable->Init(n, Store::IntToWord(0));
  jit_movi_p(JIT_R1, Store::IntToWord(0));
  NativeCodeFrame::PutEnv(n, JIT_R1);
}

void NativeCodeJitter::KillIdRef(word idRef) {
  TagVal *tagVal = TagVal::FromWordDirect(idRef);
  if (AbstractCode::GetIdRef(tagVal) == AbstractCode::LastUseLocal)
    LocalEnvKill(tagVal->Sel(0));
}

void NativeCodeJitter::GlobalEnvSel(u_int Dest, word pos) {
  Assert(Dest != JIT_V2);
  jit_pushr_ui(JIT_V2);
  NativeCodeFrame::GetClosure(JIT_V2);
  Generic::Closure::Sel(JIT_V2, Dest, Store::WordToInt(pos));
  jit_popr_ui(JIT_V2);
}

void NativeCodeJitter::ImmediateSel(u_int Dest, u_int pos) {
  JITStore::GetArg(Dest, JIT_V0, pos);
}

// LazySelClosure (belongs to alice, of course)
void NativeCodeJitter::LazySelClosureNew(word tuple, word index) {
  Generic::ConcreteCode::New(LazySelInterpreter::self, 0);
  jit_pushr_ui(JIT_V1); // Save ConcreteCode Ptr
  Generic::Closure::New(3);
  jit_popr_ui(JIT_R0); // Restore ConcreteCode Ptr
  Generic::Closure::InitCC(JIT_R0);
  LoadIdRefKill(JIT_R0, tuple);
  Generic::Closure::Put(0, JIT_R0);
  jit_movi_p(JIT_R1, index);
  Generic::Closure::Put(1, JIT_R1);
}

//
// Manipulate TaskStack
//

static Interpreter::Result CPushCall(TaskStack *taskStack, word closure) {
  return taskStack->PushCall(closure);
}

static Interpreter::Result CDirectCall(TaskStack *taskStack, word closureWord) {
  Assert(Store::WordToInt(closureWord) == INVALID_INT);
  Assert(Store::WordToTransient(closureWord) == INVALID_POINTER);
  // ClosureWord can be bound transient; why?
  Closure *closure      = Closure::FromWord(closureWord);
  word concreteCodeWord = closure->GetConcreteCode();
  Assert(Store::WordToTransient(concreteCodeWord) == INVALID_POINTER);
  ConcreteCode *concreteCode = ConcreteCode::FromWordDirect(concreteCodeWord);
  Interpreter *interpreter   = concreteCode->GetInterpreter();
  interpreter->PushCall(taskStack, closure);
  return interpreter->Run(taskStack);
}

#define PUSH_CALL() (void *) CPushCall
#define DIRECT_CALL() (void *) CDirectCall

#define RETURN() \
  JITStore::LogReg(JIT_SP); \
  jit_popr_ui(JIT_R2); \
  jit_popr_ui(JIT_R1); \
  JITStore::LogMesg("returning to base\n"); \
  jit_ret();

void NativeCodeJitter::PushCall(u_int Closure, void *proc) {
  jit_pushr_ui(Closure);
  NativeCodeFrame::GetTaskStack(JIT_R0);
  jit_pushr_ui(JIT_R0);
  JITStore::Call(2, proc);
  RETURN();
}

void NativeCodeJitter::BranchToOffset() {
  NativeCodeFrame::GetCode(JIT_R1);
  JITStore::DirectWordToInt(JIT_R0);
  jit_addr_ui(JIT_R0, JIT_R0, JIT_R1);
  jit_jmpr(JIT_R0);
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
  NativeCodeFrame::PutPC(JIT_R0);
}

//
// Calling Convention Conversion
//
void NativeCodeJitter::CompileCC(TagVal *idDefArgs) {
  switch(AbstractCode::GetArgs(idDefArgs)) {
  case AbstractCode::OneArg:
    {
      JITStore::LogMesg("Interpreter::Construct\n");
      JITStore::Call(0, (void *) Interpreter::Construct);
      TagVal *idDef = TagVal::FromWord(idDefArgs->Sel(0));
      if (idDef != INVALID_POINTER) {
	Generic::Scheduler::GetZeroArg(JIT_R0);
	LocalEnvPut(idDef->Sel(0), JIT_R0);
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
	JITStore::Call(0, (void *) Interpreter::Deconstruct);
	JITStore::LogReg(JIT_RET);
	jit_insn *no_request = jit_beqi_ui(jit_forward(), JIT_R0, 0);
	jit_movi_ui(JIT_RET, Interpreter::REQUEST);
	RETURN();
	jit_patch(no_request);
	Generic::Scheduler::GetCurrentArgs();
	for (u_int i = idDefs->GetLength(); i--;) {
	  TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
	  if (idDef != INVALID_POINTER) {
	    Generic::Scheduler::SelArg(JIT_R0, i);
	    LocalEnvPut(idDef->Sel(0), JIT_R0);
	  }
	}
      }
    }
    break;
  default:
    Error("NativeCodeJitter::CompileCC: invalid abstractCode tag");
    break;
  }
}

//
// Instruction Helper
//
void NativeCodeJitter::LoadIdRefKill(u_int Dest, word idRef) {
  TagVal *tagVal = TagVal::FromWord(idRef);
  switch (AbstractCode::GetIdRef(tagVal)) {
  case AbstractCode::Local:
    LocalEnvSel(Dest, tagVal->Sel(0));
    break;
  case AbstractCode::LastUseLocal:
    {
      Assert(Dest != JIT_R1);
      word id = tagVal->Sel(0);
      LocalEnvSel(Dest, id);
      LocalEnvKill(id);
    }
    break;
  case AbstractCode::Global:
    GlobalEnvSel(Dest, tagVal->Sel(0));
    break;
  case AbstractCode::Immediate:
    {
      word val = tagVal->Sel(0);
      if (PointerOp::IsInt(val)) {
	jit_movi_p(Dest, val);
      }
      else {
	u_int i1 = ImmediateEnv::Register(val);
	ImmediateSel(Dest, i1);
      }
    }
    break;
  case AbstractCode::Toplevel:
    ImmediateSel(Dest, Store::DirectWordToInt(tagVal->Sel(0)));
    break;
  default:
    Error("NativeCodeJitter::LoadIdRef: invalid idRef Tag");
  }
}

void NativeCodeJitter::Await(word pc) {
  jit_insn *ref[2];
  DerefItem(ref);
  jit_patch(ref[0]);
  BlockOnTransient(pc);
  jit_patch(ref[1]);
}

void NativeCodeJitter::LoadIdRef(u_int Dest, word idRef, word pc) {
  TagVal *tagVal = TagVal::FromWord(idRef);
  switch (AbstractCode::GetIdRef(tagVal)) {
  case AbstractCode::Local:
  case AbstractCode::LastUseLocal:
    LocalEnvSel(Dest, tagVal->Sel(0));
    if (pc != (word) 0)
      Await(pc);
    break;
  case AbstractCode::Global:
    GlobalEnvSel(Dest, tagVal->Sel(0));
    if (pc != (word) 0)
      Await(pc);
    break;
  case AbstractCode::Immediate:
    {
      word val = tagVal->Sel(0);
      if (PointerOp::IsInt(val)) {
	jit_movi_p(Dest, val);
      }
      else {
	u_int i1 = ImmediateEnv::Register(val);
	ImmediateSel(Dest, i1);
      }
    }
    break;
  case AbstractCode::Toplevel:
    ImmediateSel(Dest, Store::DirectWordToInt(tagVal->Sel(0)));
    if (pc != (word) 0)
      Await(pc);
    break;
  default:
    Error("NativeCodeJitter::LoadIdRef: invalid idRef Tag");
  }
}

void NativeCodeJitter::BlockOnTransient(word pc) {
  jit_movi_ui(JIT_R0, pc);
  NativeCodeFrame::PutPC(JIT_R0);
  JITStore::PatchTag(); // Restore transient tag
  Generic::Scheduler::SetCurrentData(JIT_V1);
  jit_movi_ui(JIT_R0, 0);
  Generic::Scheduler::PutNArgs(JIT_R0);
  u_int size = ((::Block *) livenessTable)->GetSize();
  if (size != 0) {
    //    jit_movi_ui(JIT_R0, Store::IntToWord(4711));
    for (u_int i = size; i--;)
      if (livenessTable->Sel(i) == Store::IntToWord(2)) {
	// NativeCodeFrame::PutEnv(i, JIT_R0);
      }
  }
  jit_movi_ui(JIT_RET, Interpreter::REQUEST);
  RETURN();
}

//
// Instructions
//

// Kill of id vector * instr
TagVal *NativeCodeJitter::InstrKill(TagVal *pc) {
  Vector *kills = Vector::FromWord(pc->Sel(0));
  for (u_int i = kills->GetLength(); i--;) {
    u_int n = Store::WordToInt(kills->Sub(i));
    if (livenessTable->Sel(n) == Store::IntToWord(1))
      livenessTable->Init(n, Store::IntToWord(2));
  }
  return TagVal::FromWord(pc->Sel(1));
}

// PutVar of id * idRef * instr
TagVal *NativeCodeJitter::InstrPutVar(TagVal *pc) {
  PrintPC("PutVar\n");
  LoadIdRefKill(JIT_R0, pc->Sel(1));
  LocalEnvPut(pc->Sel(0), JIT_R0);
  return TagVal::FromWord(pc->Sel(2));
}

static void ConstructorNew() {
  jit_movi_p(JIT_R0, static_cast<Constructor *(*)(word)>(&Constructor::New));
  jit_callr(JIT_R0); // constructor resides in JIT_RET
  jit_addi_ui(JIT_SP, JIT_SP, 1 * sizeof(word));
}

// PutNew of id * string * instr
TagVal *NativeCodeJitter::InstrPutNew(TagVal *pc) {
  PrintPC("PutNew\n");
  u_int i1 = ImmediateEnv::Register(pc->Sel(1));
  ImmediateSel(JIT_R0, i1);
  jit_pushr_ui(JIT_R0);
  ConstructorNew();
  LocalEnvPut(pc->Sel(0), JIT_R0);
  return TagVal::FromWord(pc->Sel(2));
}

// PutTag of id * int * idRef vector * instr
TagVal *NativeCodeJitter::InstrPutTag(TagVal *pc) {
  PrintPC("PutTag\n");
  Vector *idRefs = Vector::FromWord(pc->Sel(2));
  u_int nArgs    = idRefs->GetLength();
  JITAlice::TagVal::New(Store::WordToInt(pc->Sel(1)), nArgs);
  for (u_int i = nArgs; i--;) {
    LoadIdRefKill(JIT_R0, idRefs->Sub(i));
    JITAlice::TagVal::Put(i, JIT_R0);
  }
  LocalEnvPut(pc->Sel(0), JIT_V1);
  return TagVal::FromWord(pc->Sel(3));
}

// PutCon of id * idRef * idRef vector * instr
TagVal *NativeCodeJitter::InstrPutCon(TagVal *pc) {
  word instrPC = Store::IntToWord(GetRelativePC());
  PrintPC("PutCon\n");
  LoadIdRef(JIT_V1, pc->Sel(1), instrPC);
  KillIdRef(pc->Sel(1));
  jit_movr_p(JIT_R1, JIT_V1);
  Vector *idRefs = Vector::FromWord(pc->Sel(2));
  u_int nArgs    = idRefs->GetLength();
  JITAlice::ConVal::New(JIT_R1, nArgs);
  for (u_int i = nArgs; i--;) {
    LoadIdRefKill(JIT_R0, idRefs->Sub(i));
    JITAlice::ConVal::Put(i, JIT_R0);
  }
  LocalEnvPut(pc->Sel(0), JIT_V1);
  return TagVal::FromWord(pc->Sel(3));
}

// PutRef * id * idRef * instr
TagVal *NativeCodeJitter::InstrPutRef(TagVal *pc) {
  PrintPC("PutRef\n");
  JITAlice::Cell::New();
  LoadIdRefKill(JIT_R0, pc->Sel(1));
  JITAlice::Cell::Put(JIT_R0);
  LocalEnvPut(pc->Sel(0), JIT_V1);
  return TagVal::FromWord(pc->Sel(2));
}

// PutTup of id * idRef vector * instr
TagVal *NativeCodeJitter::InstrPutTup(TagVal *pc) {
  PrintPC("PutTup\n");
  Vector *idRefs = Vector::FromWord(pc->Sel(1));
  u_int nArgs    = idRefs->GetLength();
  if (nArgs == 0) {
    jit_movi_p(JIT_V1, Store::IntToWord(0)); // unit
  }
  else {
    Generic::Tuple::New(nArgs);
    for (u_int i = nArgs; i--;) {
      LoadIdRefKill(JIT_R0, idRefs->Sub(i));
      Generic::Tuple::Put(i, JIT_R0);
    }
  }
  LocalEnvPut(pc->Sel(0), JIT_V1);
  return TagVal::FromWord(pc->Sel(2));
}

// PutVec * id * idRef vector * instr
TagVal *NativeCodeJitter::InstrPutVec(TagVal *pc) {
  PrintPC("PutVec\n");
  Vector *idRefs = Vector::FromWord(pc->Sel(1));
  u_int nArgs    = idRefs->GetLength();
  JITAlice::Vector::New(nArgs);
  for (u_int i = nArgs; i--;) {
    LoadIdRefKill(JIT_R0, idRefs->Sub(i));
    JITAlice::Vector::Put(i, JIT_R0);
  }
  LocalEnvPut(pc->Sel(0), JIT_V1);
  return TagVal::FromWord(pc->Sel(2));
}

// Close of id * idRef vector * value * instr
TagVal *NativeCodeJitter::InstrClose(TagVal *pc) {
  PrintPC("Close\n");
  u_int i1 = ImmediateEnv::Register(pc->Sel(2));
  ImmediateSel(JIT_R1, i1);
  Vector *idRefs = Vector::FromWord(pc->Sel(1));
  u_int nGlobals = idRefs->GetLength();
  Generic::Closure::New(JIT_R1, nGlobals);
  for (u_int i = nGlobals; i--;) {
    LoadIdRefKill(JIT_R0, idRefs->Sub(i));
    Generic::Closure::Put(i, JIT_R0);
  }
  LocalEnvPut(pc->Sel(0), JIT_V1);
  return TagVal::FromWord(pc->Sel(3));
}

// Specialize of id * idRef vector * template * instr
// template = Template of coord * int * int * idDef args * instr
// abstractcode: Specialized of coord * value vector * int * idDef args * instr
TagVal *NativeCodeJitter::InstrSpecialize(TagVal *pc) {
  PrintPC("Specialize\n");
  // Create specialized abstractCode
  JITAlice::TagVal::New(AbstractCode::Specialized, 5);
  jit_pushr_ui(JIT_V0); // Save Immediate Env
  u_int i1 = ImmediateEnv::Register(pc->Sel(2)); // Save template_
  ImmediateSel(JIT_V0, i1); // Load template_
  JITAlice::TagVal::Sel(JIT_R0, JIT_V0, 0);
  JITAlice::TagVal::Put(0, JIT_R0);
  // position one will be filled in later
  JITAlice::TagVal::Sel(JIT_R0, JIT_V0, 2);
  JITAlice::TagVal::Put(2, JIT_R0);
  JITAlice::TagVal::Sel(JIT_R0, JIT_V0, 3);
  JITAlice::TagVal::Put(3, JIT_R0);
  JITAlice::TagVal::Sel(JIT_R0, JIT_V0, 4);
  JITAlice::TagVal::Put(4, JIT_R0);
  jit_popr_ui(JIT_V0); // Restore Immediate Env
  jit_pushr_ui(JIT_V1); // Save abstractCode
  // Create Value Vector
  Vector *idRefs   = Vector::FromWordDirect(pc->Sel(1));
  u_int nToplevels = idRefs->GetLength();
  JITAlice::Vector::New(nToplevels);
  for (u_int i = nToplevels; i--;) {
    LoadIdRefKill(JIT_R0, idRefs->Sub(i));
    JITAlice::Vector::Put(i, JIT_R0);
  }
  jit_movr_p(JIT_R0, JIT_V1); // Move value vector to R0
  jit_popr_ui(JIT_V1); // Restore abstractCode
  JITAlice::TagVal::Put(1, JIT_R0); // Store value vector
  JITAlice::TagVal::Sel(JIT_R0, 2); // Load nLocals
  jit_pushr_ui(JIT_R0); // Save nLocals
  jit_pushr_ui(JIT_V1); // Save abstractCode
  Generic::Transform::New();
  jit_ldi_p(JIT_R0, &AliceLanguageLayer::TransformNames::function);
  Generic::Transform::PutName(JIT_R0);
  jit_popr_ui(JIT_R0); // Restore abstractCode
  Generic::Transform::PutArgument(JIT_R0);
  jit_movr_p(JIT_R1, JIT_V1); // Save transform
  JITAlice::NativeConcreteCode::New();
  JITAlice::NativeConcreteCode::PutTransform(JIT_R1);
  jit_popr_ui(JIT_R1); // Restore nLocals
  JITAlice::NativeConcreteCode::PutNLocals(JIT_R1);
  jit_pushr_ui(JIT_V1); // NativeConcreteCode
  JITStore::Call(1, (void *) NativeCodeJitter::Specialize);
  jit_movr_p(JIT_R1, JIT_V1);
  Generic::Closure::New(JIT_R1, 0);
  LocalEnvPut(pc->Sel(0), JIT_V1);
  return TagVal::FromWordDirect(pc->Sel(3));
}

// AppPrim of value * idRef vector * (idDef * instr) option
TagVal *NativeCodeJitter::InstrAppPrim(TagVal *pc) {
  PrintPC("AppPrim\n");
  TagVal *idDefInstrOpt = TagVal::FromWord(pc->Sel(2));
  if (idDefInstrOpt != INVALID_POINTER) { // SOME (idDef * instr)
    Tuple *idDefInstr = Tuple::FromWord(idDefInstrOpt->Sel(0));
    jit_insn *docall  = jit_jmpi(jit_forward());
    word contPC = Store::IntToWord(GetRelativePC());
    TagVal *idDef = TagVal::FromWord(idDefInstr->Sel(0));
    if (idDef != INVALID_POINTER) {
      Generic::Scheduler::GetZeroArg(JIT_R0);
      LocalEnvPut(idDef->Sel(0), JIT_R0);
    }
    CompileInstr(TagVal::FromWord(idDefInstr->Sel(1)));
    jit_patch(docall);
    SetRelativePC(contPC);
  }
  else {
    NativeCodeFrame::GetTaskStack(JIT_V1);
    Generic::TaskStack::PopFrames(JIT_V1, 1);
  }
  // Load Arguments
  Vector *actualIdRefs = Vector::FromWord(pc->Sel(1));
  u_int nArgs          = actualIdRefs->GetLength();
  jit_movi_ui(JIT_R0, ((nArgs == 1) ? Scheduler::ONE_ARG : nArgs));
  Generic::Scheduler::PutNArgs(JIT_R0);
  Generic::Scheduler::GetCurrentArgs();
  for (u_int i = nArgs; i--;) {
    LoadIdRefKill(JIT_R0, actualIdRefs->Sub(i));
    Generic::Scheduler::PutArg(i, JIT_R0);
  }
  u_int i1 = ImmediateEnv::Register(pc->Sel(0));
  ImmediateSel(JIT_V1, i1);
  PushCall(JIT_V1, DIRECT_CALL());
  return INVALID_POINTER;
}

// App(Var|Const) of idRef * idRef args * (idDef args * instr) option
TagVal *NativeCodeJitter::InstrAppVar(TagVal *pc) {
  PrintPC("AppVar\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  jit_pushr_ui(JIT_V1); // Save Closure
  TagVal *idDefArgsInstrOpt = TagVal::FromWord(pc->Sel(2));
  if (idDefArgsInstrOpt != INVALID_POINTER) { // SOME ...
    JITStore::LogMesg("non-tail call\n");
    Tuple *idDefArgsInstr = Tuple::FromWord(idDefArgsInstrOpt->Sel(0));
    jit_insn *docall      = jit_jmpi(jit_forward());
    word contPC = Store::IntToWord(GetRelativePC());
    CompileCC(TagVal::FromWord(idDefArgsInstr->Sel(0)));
    CompileInstr(TagVal::FromWord(idDefArgsInstr->Sel(1)));
    jit_patch(docall);
    SetRelativePC(contPC);
  }
  else {
    JITStore::LogMesg("tailcall; popping frame\n");
    NativeCodeFrame::GetTaskStack(JIT_V1);
    Generic::TaskStack::PopFrames(JIT_V1, 1);
  }
  // Load arguments
  TagVal *actualArgs = TagVal::FromWord(pc->Sel(1));
  switch (AbstractCode::GetArgs(actualArgs)) {
  case AbstractCode::OneArg:
    {
      jit_movi_ui(JIT_R0, Scheduler::ONE_ARG);
      Generic::Scheduler::PutNArgs(JIT_R0);
      LoadIdRefKill(JIT_R0, actualArgs->Sel(0));
      Generic::Scheduler::PutZeroArg(JIT_R0);
    }
    break;
  case AbstractCode::TupArgs:
    {
      Vector *actualIdRefs = Vector::FromWord(actualArgs->Sel(0));
      u_int nArgs          = actualIdRefs->GetLength();
      jit_movi_ui(JIT_R0, nArgs);
      Generic::Scheduler::PutNArgs(JIT_R0);
      Generic::Scheduler::GetCurrentArgs();
      for (u_int i = nArgs; i--;) {
	LoadIdRefKill(JIT_R0, actualIdRefs->Sub(i));
	Generic::Scheduler::PutArg(i, JIT_R0);
      }
    }
    break;
  }
  JITStore::LogMesg("created arguments\n");
  KillIdRef(pc->Sel(0));
  jit_popr_ui(JIT_V1); // Restore Closure
  PushCall(JIT_V1, PUSH_CALL());
  return INVALID_POINTER;
}

// GetRef of id * idRef * instr
TagVal *NativeCodeJitter::InstrGetRef(TagVal *pc) {
  PrintPC("GetRef\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  LoadIdRef(JIT_V1, pc->Sel(1), instrPC);
  KillIdRef(pc->Sel(1));
  JITAlice::Cell::Sel(JIT_R0);
  LocalEnvPut(pc->Sel(0), JIT_R0);
  return TagVal::FromWord(pc->Sel(2));
}

// GetTup of idDef vector * idRef * instr
TagVal *NativeCodeJitter::InstrGetTup(TagVal *pc) {
  PrintPC("GetTup\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  LoadIdRef(JIT_V1, pc->Sel(1), instrPC);
  KillIdRef(pc->Sel(1));
  Vector *idDefs = Vector::FromWord(pc->Sel(0));
  u_int nArgs    = idDefs->GetLength();
  if (nArgs != 0)
    for (u_int i = nArgs; i--;) {
      TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
      if (idDef != INVALID_POINTER) {
	Generic::Tuple::Sel(JIT_R0, i);
	LocalEnvPut(idDef->Sel(0), JIT_R0);
      }
    }
  return TagVal::FromWord(pc->Sel(2));
}

// Sel of id * idRef * int * instr
TagVal *NativeCodeJitter::InstrSel(TagVal *pc) {
  PrintPC("Sel\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  LoadIdRef(JIT_V1, pc->Sel(1), instrPC);
  KillIdRef(pc->Sel(1));
  Generic::Tuple::Sel(JIT_R0, Store::WordToInt(pc->Sel(2)));
  LocalEnvPut(pc->Sel(0), JIT_R0);
  return TagVal::FromWord(pc->Sel(3));
}

// LazySel of id * idRef * int * instr
TagVal *NativeCodeJitter::InstrLazySel(TagVal *pc) {
  PrintPC("LazySel\n");
  LazySelClosureNew(pc->Sel(1), pc->Sel(2));
  jit_pushr_ui(JIT_V1); // Save Closure Ptr
  JITStore::AllocTransient(BYNEED_LABEL);
  jit_popr_ui(JIT_R0); // Restore Closure Ptr
  JITStore::InitArg(JIT_V1, 0, JIT_R0);
  JITStore::PatchTag(); // add transient tag
  LocalEnvPut(pc->Sel(0), JIT_V1);
  return TagVal::FromWord(pc->Sel(3));
}

// Raise of idRef
TagVal *NativeCodeJitter::InstrRaise(TagVal *pc) {
  PrintPC("Raise\n");
  LoadIdRefKill(JIT_R0, pc->Sel(0));
  Generic::Scheduler::SetCurrentData(JIT_R0);
  jit_pushr_ui(JIT_V2); // Frame ptr
  JITStore::Call(1, (void *) Backtrace::New);
  Generic::Scheduler::SetCurrentBacktrace(JIT_RET);
  jit_movi_ui(JIT_RET, Interpreter::RAISE);
  RETURN();
  return INVALID_POINTER;
}

// Reraise of idRef
TagVal *NativeCodeJitter::InstrReraise(TagVal *pc) {
  PrintPC("Reraise\n");
  LoadIdRefKill(JIT_V1, pc->Sel(0));
  // DirectWordToBlock(JIT_V1) does nothing
  Generic::Tuple::Sel(JIT_R0, 0);
  Generic::Scheduler::SetCurrentData(JIT_R0);
  Generic::Tuple::Sel(JIT_V1, 1);
  Generic::Scheduler::SetCurrentBacktrace(JIT_V1);
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

// Try of instr * idDef * idDef * instr
TagVal *NativeCodeJitter::InstrTry(TagVal *pc) {
  PrintPC("Try\n");
  jit_insn *execute_try = jit_jmpi(jit_forward());
  word handlerPC = Store::IntToWord(GetRelativePC());
  TagVal *idDef1 = TagVal::FromWord(pc->Sel(1));
  if (idDef1 != INVALID_POINTER) {
    Generic::Scheduler::GetZeroArg(JIT_R0);
    LocalEnvPut(idDef1->Sel(0), JIT_R0);
  }
  TagVal *idDef2 = TagVal::FromWord(pc->Sel(2));
  if (idDef2 != INVALID_POINTER) {
    Generic::Scheduler::GetOneArg(JIT_R0);
    LocalEnvPut(idDef2->Sel(0), JIT_R0);
  }
  CompileInstr(TagVal::FromWord(pc->Sel(3)));
  jit_patch(execute_try);
  NativeCodeHandlerFrame::New();
  jit_movi_p(JIT_R0, handlerPC);
  NativeCodeHandlerFrame::PutPC(JIT_R0);
  NativeCodeHandlerFrame::PutFrame(JIT_V2);
  NativeCodeFrame::GetTaskStack(JIT_R0);
  jit_pushr_ui(JIT_V1); // NativeCodeHandler Frame
  jit_pushr_ui(JIT_V2); // NativeCode Frame
  jit_pushr_ui(JIT_R0); // TaskStack Ptr
  JITStore::Call(3, (void *) PushHandlerFrame);
  return TagVal::FromWord(pc->Sel(0));
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
  NativeCodeFrame::GetTaskStack(JIT_V1);
  jit_pushr_ui(JIT_V1); // TaskStack Ptr
  JITStore::Call(1, (void *) PopHandlerFrame);
  return TagVal::FromWord(pc->Sel(0));
}

// EndHandle of instr
TagVal *NativeCodeJitter::InstrEndHandle(TagVal *pc) {
  PrintPC("EndHandle\n");
  return TagVal::FromWord(pc->Sel(0));
}

// Test Helpers
static void *LookupTable(HashTable *table, word key) {
  if (table->IsMember(key))
    return table->GetItem(key);
  else
    return 0;
}

void NativeCodeJitter::LookupTestTable(u_int Key, u_int table) {
  jit_pushr_ui(Key); // Key Argument
  ImmediateSel(JIT_R0, table);
  jit_pushr_ui(JIT_R0); // Table Argument
  JITStore::Call(2, (void *) LookupTable);
}

// IntTest of idRef * (int * instr) vector * instr
TagVal *NativeCodeJitter::InstrIntTest(TagVal *pc) {
  PrintPC("IntTest\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  KillIdRef(pc->Sel(0));
  Vector *tests    = Vector::FromWord(pc->Sel(1));
  u_int nTests     = tests->GetLength();
  HashTable *table = HashTable::New(HashTable::INT_KEY, nTests * 2);
  u_int i1         = ImmediateEnv::Register(table->ToWord());
  LookupTestTable(JIT_V1, i1);
  jit_insn *else_ref = jit_beqi_ui(jit_forward(), JIT_RET, 0);
  BranchToOffset();
  // Create Branches
  for (u_int i = nTests; i--;) {
    Tuple *pair  = Tuple::FromWord(tests->Sub(i));
    word key     = pair->Sel(0);
    u_int offset = GetRelativePC();
    table->InsertItem(key, Store::IntToWord(offset));
    CompileBranch(TagVal::FromWord(pair->Sel(1)));
  }
  jit_patch(else_ref);
  return TagVal::FromWord(pc->Sel(2));
}

// CompactIntTest of idRef * int * instrs * instr
TagVal *NativeCodeJitter::InstrCompactIntTest(TagVal *pc) {
  PrintPC("CompactIntTest\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  KillIdRef(pc->Sel(0));
  DirectWordToInt();
  int indexOffset = Store::DirectWordToInt(pc->Sel(1));
  if (indexOffset != 0)
    jit_subi_i(JIT_R0, JIT_R0, indexOffset);
  Vector *tests      = Vector::FromWordDirect(pc->Sel(2));
  u_int nTests       = tests->GetLength();
  jit_insn *else_ref = jit_bgei_ui(jit_forward(), JIT_R0, nTests);
  Tuple *branches    = Tuple::New(nTests);
  u_int i1           = ImmediateEnv::Register(branches->ToWord());
  ImmediateSel(JIT_V1, i1);
  Generic::Tuple::IndexSel(JIT_R0, JIT_R0); // R0 holds branch offset word
  BranchToOffset();
  // Create branches
  for (u_int i = nTests; i--;) {
    u_int offset = GetRelativePC();
    branches->Init(i, Store::IntToWord(offset));
    CompileBranch(TagVal::FromWord(tests->Sub(i)));
  }
  jit_patch(else_ref);
  return TagVal::FromWord(pc->Sel(3));
}

// RealTest of idRef * (real * instr) vector * instr
TagVal *NativeCodeJitter::InstrRealTest(TagVal *pc) {
  PrintPC("RealTest\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  KillIdRef(pc->Sel(0));
  Vector *tests    = Vector::FromWord(pc->Sel(1));
  u_int nTests     = tests->GetLength();
  HashTable *table = HashTable::New(HashTable::BLOCK_KEY, nTests * 2);
  u_int i1         = ImmediateEnv::Register(table->ToWord());
  LookupTestTable(JIT_V1, i1);
  jit_insn *else_ref = jit_beqi_ui(jit_forward(), JIT_RET, 0);
  BranchToOffset();
  // Create Branches
  for (u_int i = nTests; i--;) {
    Tuple *pair  = Tuple::FromWord(tests->Sub(i));
    word key     = pair->Sel(0);
    u_int offset = GetRelativePC();
    table->InsertItem(key, Store::IntToWord(offset));
    CompileBranch(TagVal::FromWord(pair->Sel(1)));
  }
  jit_patch(else_ref);
  return TagVal::FromWord(pc->Sel(2));
}

// StringTest of idRef * (string * instr) vector * instr
TagVal *NativeCodeJitter::InstrStringTest(TagVal *pc) {
  PrintPC("StringTest\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  KillIdRef(pc->Sel(0));
  Vector *tests    = Vector::FromWord(pc->Sel(1));
  u_int nTests     = tests->GetLength();
  HashTable *table = HashTable::New(HashTable::BLOCK_KEY, nTests * 2);
  u_int i1         = ImmediateEnv::Register(table->ToWord());
  LookupTestTable(JIT_V1, i1);
  jit_insn *else_ref = jit_beqi_ui(jit_forward(), JIT_RET, 0);
  BranchToOffset();
  // Create Branches
  for (u_int i = nTests; i--;) {
    Tuple *pair  = Tuple::FromWord(tests->Sub(i));
    word key     = pair->Sel(0);
    u_int offset = GetRelativePC();
    table->InsertItem(key, Store::IntToWord(offset));
    CompileBranch(TagVal::FromWord(pair->Sel(1)));
  }
  jit_patch(else_ref);
  return TagVal::FromWord(pc->Sel(2));
}

// TagTest of idRef * (int * instr) vector
//         * (int * idDef vector * instr) vector * instr
TagVal *NativeCodeJitter::InstrTagTest(TagVal *pc) {
  PrintPC("TagTest\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  LoadIdRef(JIT_V1, pc->Sel(0), (word) 0);
  jit_insn *ref[2];
  Deref3(ref);
  // Integer branch (V1 is int word, nullary constructor)
  KillIdRef(pc->Sel(0));
  Vector *tests1 = Vector::FromWord(pc->Sel(1));
  u_int nTests1  = tests1->GetLength();
  jit_insn *else_ref1;
  if (nTests1 == 0) {
    else_ref1 = jit_jmpi(jit_forward());
  }
  else {
    HashTable *table1 = HashTable::New(HashTable::INT_KEY, nTests1 * 2);
    u_int i1          = ImmediateEnv::Register(table1->ToWord());
    LookupTestTable(JIT_V1, i1);
    else_ref1 = jit_beqi_ui(jit_forward(), JIT_RET, 0);
    BranchToOffset();
    // Create Branches
    for (u_int i = nTests1; i--;) {
      Tuple *pair  = Tuple::FromWord(tests1->Sub(i));
      word key     = pair->Sel(0);
      u_int offset = GetRelativePC();
      table1->InsertItem(key, Store::IntToWord(offset));
      CompileBranch(TagVal::FromWord(pair->Sel(1)));
    }
  }
  jit_patch(ref[0]); // Transient Branch
  BlockOnTransient(instrPC);
  jit_patch(ref[1]); // Block Branch
  // Block branch (V1 is Non-nullary constructor)
  KillIdRef(pc->Sel(0));
  Vector *tests2 = Vector::FromWord(pc->Sel(2));
  u_int nTests2  = tests2->GetLength();
  if (nTests2 != 0) {
    JITAlice::TagVal::GetTag();
    IntToWord(JIT_R0, JIT_R0);
    jit_pushr_ui(JIT_V1); // Save TagVal Ptr
    HashTable *table2 = HashTable::New(HashTable::INT_KEY, nTests2 * 2);
    u_int i2          = ImmediateEnv::Register(table2->ToWord());
    LookupTestTable(JIT_R0, i2);
    jit_insn *else_ref2 = jit_beqi_ui(jit_forward(), JIT_RET, 0);
    BranchToOffset();
    // Create Branches
    for (u_int i = nTests2; i--;) {
      Tuple *triple  = Tuple::FromWord(tests2->Sub(i));
      word key       = triple->Sel(0);
      u_int offset   = GetRelativePC();
      table2->InsertItem(key, Store::IntToWord(offset));
      Vector *idDefs = Vector::FromWord(triple->Sel(1));
      jit_popr_ui(JIT_V1); // Restore TagVal Ptr
      for (u_int i = idDefs->GetLength(); i--;) {
	TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
	if (idDef != INVALID_POINTER) {
	  JITAlice::TagVal::Sel(JIT_R0, i);
	  LocalEnvPut(idDef->Sel(0), JIT_R0);
	}
      }
      CompileBranch(TagVal::FromWord(triple->Sel(2)));
    }
    jit_patch(else_ref2);
    // clear stack
    jit_popr_ui(JIT_V1);
  }
  jit_patch(else_ref1);
  return TagVal::FromWord(pc->Sel(3));
}

// CompactTagTest of idRef * tagTests * instr
TagVal *NativeCodeJitter::InstrCompactTagTest(TagVal *pc) {
  PrintPC("CompactTagTest\n");
  word instrPC = Store::IntToWord(GetRelativePC());
  LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  KillIdRef(pc->Sel(0));
  jit_insn *have_constructor = jit_beqi_ui(jit_forward(), JIT_R0, BLKTAG);
  DirectWordToInt();
  jit_insn *skip_tagload = jit_jmpi(jit_forward());
  // Block branch (v1 is non-nullary constructor)
  jit_patch(have_constructor);
  JITAlice::TagVal::GetTag();
  jit_patch(skip_tagload);
  Vector *tests      = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests       = tests->GetLength();
  jit_insn *else_ref = jit_bgei_ui(jit_forward(), JIT_R0, nTests);
  Tuple *branches    = Tuple::New(nTests);
  u_int i1           = ImmediateEnv::Register(branches->ToWord());
  jit_pushr_ui(JIT_V1); // Save int/tagval ptr
  ImmediateSel(JIT_V1, i1);
  Generic::Tuple::IndexSel(JIT_R0, JIT_R0); // R0 holds branch offset
  BranchToOffset();
  // Create Branches
  Assert(nTests != 0);
  for (u_int i = nTests; i--;) {
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
	  JITAlice::TagVal::Sel(JIT_R0, j);
	  LocalEnvPut(idDef->Sel(0), JIT_R0);
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
  LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  JITStore::Block::GetLabel();
  jit_insn *nullary_constr = jit_bnei_ui(jit_forward(), JIT_R0, Alice::ConVal);
  JITAlice::ConVal::GetConstructor(JIT_R1);
  jit_movr_p(JIT_R2, JIT_V1); // Save ConVal Ptr
  Vector *tests1 = Vector::FromWord(pc->Sel(2));
  u_int nTests1  = tests1->GetLength();
  for (u_int i = 0; i < nTests1; i++) {
    Tuple *triple = Tuple::FromWord(tests1->Sub(i));
    LoadIdRef(JIT_V1, triple->Sel(0), instrPC);
    jit_insn *next_test_ref = jit_bner_ui(jit_forward(), JIT_V1, JIT_R1);
    Vector *idDefs          = Vector::FromWord(triple->Sel(1));
    jit_movr_p(JIT_V1, JIT_R2); // Restore ConVal Ptr
    for (u_int i = idDefs->GetLength(); i--;) {
      TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
      if (idDef != INVALID_POINTER) {
	JITAlice::ConVal::Sel(JIT_R0, i);
	LocalEnvPut(idDef->Sel(0), JIT_R0);
      }
      KillIdRef(pc->Sel(0)); // Some kills missing
      CompileBranch(TagVal::FromWord(triple->Sel(2)));
    }
    jit_patch(next_test_ref);
  }
  // Nullary Constructor
  jit_patch(nullary_constr);
  // Save Constructor ptr
  jit_movr_p(JIT_R1, JIT_V1);
  Vector *tests2 = Vector::FromWord(pc->Sel(1));
  u_int nTests2 = tests2->GetLength();
  for (u_int i = 0; i < nTests2; i++) {
    Tuple *pair = Tuple::FromWord(tests2->Sub(i));
    LoadIdRef(JIT_V1, pair->Sel(0), instrPC);
    jit_insn *next_test_ref = jit_bner_ui(jit_forward(), JIT_V1, JIT_R1);
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
  LoadIdRef(JIT_V1, pc->Sel(0), instrPC);
  KillIdRef(pc->Sel(0));
  jit_pushr_ui(JIT_V1); // Save vector ptr
  JITAlice::Vector::GetLength(JIT_R0);
  Vector *tests    = Vector::FromWord(pc->Sel(1));
  u_int nTests     = tests->GetLength();
  HashTable *table = HashTable::New(HashTable::INT_KEY, nTests * 2);
  u_int i1         = ImmediateEnv::Register(table->ToWord());
  LookupTestTable(JIT_R0, i1);
  jit_popr_ui(JIT_V1); // Restore vector ptr
  jit_insn *else_ref = jit_beqi_ui(jit_forward(), JIT_RET, 0);
  BranchToOffset();
  // Create Branches
  for (u_int i = nTests; i--;) {
    Tuple *pair    = Tuple::FromWord(tests->Sub(i));
    Vector *idDefs = Vector::FromWord(pair->Sel(0));
    word key       = Store::IntToWord(idDefs->GetLength());
    u_int offset   = GetRelativePC();
    table->InsertItem(key, Store::IntToWord(offset));
    for (u_int i = idDefs->GetLength(); i--;) {
      TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
      if (idDef != INVALID_POINTER) {
	JITAlice::Vector::Sel(JIT_R0, i);
	LocalEnvPut(idDef->Sel(0), JIT_R0);
      }
    }
    CompileBranch(TagVal::FromWord(pair->Sel(1)));
  }
  jit_patch(else_ref);
  return TagVal::FromWord(pc->Sel(2));
}

// Shared of stamp * instr
TagVal *NativeCodeJitter::InstrShared(TagVal *pc) {
  PrintPC("InstrShared\n");
  word stamp = pc->Sel(0);
  if (sharedTable->IsMember(stamp)) {
    u_int pos = Store::DirectWordToInt(sharedTable->GetItem(stamp));
    drop_jit_jmpi((char *) pos);
    return INVALID_POINTER;
  }
  else {
    u_int pos = (u_int) jit_get_ip().ptr;
    sharedTable->InsertItem(stamp, Store::IntToWord(pos));
    return TagVal::FromWord(pc->Sel(1));
  }
}

// Return 
TagVal *NativeCodeJitter::InstrReturn(TagVal *pc) {
  PrintPC("Return\n");
  TagVal *returnArgs = TagVal::FromWord(pc->Sel(0));
  switch (AbstractCode::GetArgs(returnArgs)) {
  case AbstractCode::OneArg:
    jit_movi_ui(JIT_R0, Scheduler::ONE_ARG);
    Generic::Scheduler::PutNArgs(JIT_R0);
    LoadIdRefKill(JIT_R0, returnArgs->Sel(0));
    Generic::Scheduler::PutZeroArg(JIT_R0);
    break;
  case AbstractCode::TupArgs:
    {
      Vector *returnIdRefs = Vector::FromWord(returnArgs->Sel(0));
      u_int nArgs          = returnIdRefs->GetLength();
      if (nArgs < Scheduler::maxArgs) {
	jit_movi_ui(JIT_R0, nArgs);
	Generic::Scheduler::PutNArgs(JIT_R0);
	Generic::Scheduler::GetCurrentArgs();
	for (u_int i = nArgs; i--;) {
	  LoadIdRefKill(JIT_R0, returnIdRefs->Sub(i));
	  Generic::Scheduler::PutArg(i, JIT_R0);
	}
      }
      else {
	Generic::Tuple::New(nArgs);
	for (u_int i = nArgs; i--;) {
	  LoadIdRefKill(JIT_R0, returnIdRefs->Sub(i));
	  Generic::Tuple::Put(i, JIT_R0);
	}
	Generic::Scheduler::PutZeroArg(JIT_V1);
	jit_movi_ui(JIT_R0, Scheduler::ONE_ARG);
	Generic::Scheduler::PutNArgs(JIT_R0);
      }
    }
    break;
  }
  NativeCodeFrame::GetTaskStack(JIT_V1);
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
  jit_pushr_ui(JIT_R1);
  jit_pushr_ui(JIT_R2);
  JITStore::LogMesg(info);
  JITStore::LogReg(JIT_SP);
  NativeCodeFrame::GetImmediateArgs(JIT_V0);
  NativeCodeFrame::GetPC(JIT_V1); // PC is base-adjusted
  DirectWordToInt();
  NativeCodeFrame::GetCode(JIT_V1);
  jit_addr_ui(JIT_R0, JIT_R0, JIT_V1);
  JITStore::LogMesg("branching to ");
  JITStore::LogReg(JIT_R0);
  jit_jmpr(JIT_R0);
  return start;
}
 
void NativeCodeJitter::CompileBranch(TagVal *pc) {
  u_int size        = ((::Block *) livenessTable)->GetSize();
  Tuple *cloneTable = Tuple::New(size);
  for (u_int i = size; i--;)
    cloneTable->Init(i, livenessTable->Sel(i));
  CompileInstr(pc);
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
    case AbstractCode::LazySel:
      pc = InstrLazySel(pc); break;
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
  codeBufferSize = bufferSize;
  codeBuffer     = (jit_insn *) malloc(sizeof(jit_insn) * bufferSize);
  // Compute Initial PC
  CompileProlog("Dummy Information");
  initialPC = Store::IntToWord(GetRelativePC());
}

// String Handling: to be done
static char *ExportCString(Chunk *s) {
  u_int sLen = s->GetSize();
  Chunk *e   = Store::AllocChunk(sLen + 1);
  char *eb   = e->GetBase();
  std::memcpy(eb, s->GetBase(), sLen);
  eb[sLen] = '\0';
  return eb;
}

// Function of coord * int * int * idDef args * instr
// Specialized of coord * value vector * int * idDef args * instr
Chunk *NativeCodeJitter::CompileCode(TagVal *abstractCode, bool needCC) {
  // Setup nodes/immediate value tables
  ImmediateEnv::Init();
  sharedTable = HashTable::New(HashTable::INT_KEY, SHARED_TABLE_SIZE);
  // Start function compilation with prolog
  Tuple *coord = Tuple::FromWord(abstractCode->Sel(0));
  String *name = String::FromWord(coord->Sel(0));
  u_int line   = Store::WordToInt(coord->Sel(1));
  char info[1024];
  sprintf(info, "%s:%d\n", ExportCString((::Chunk *) name), line);
  char *start = CompileProlog(strdup(info));
  // Transfer immediate arguments
  switch (AbstractCode::GetAbstractCode(abstractCode)) {
  case AbstractCode::Function:
    break;
  case AbstractCode::Specialized:
    {
      // Now import immediate Values
      Vector *values = Vector::FromWordDirect(abstractCode->Sel(1));
      u_int nValues  = values->GetLength();
      for (u_int i = 0; i < nValues; i++)
	ImmediateEnv::Register(values->Sub(i));
    }
    break;
  default:
    Error("NativeCodeJitter::Compile: invalid abstractCode tag");
  };
  // Init liveness table
  u_int nLocals = Store::DirectWordToInt(abstractCode->Sel(2));
  livenessTable = Tuple::New(nLocals);
  for (u_int i = nLocals; i--;)
    livenessTable->Init(i, 0);
  // Compile argument calling convention conversion
  JITStore::LogMesg("Calling convention conversion...\n");
  if (needCC)
    CompileCC(TagVal::FromWord(abstractCode->Sel(3)));
  JITStore::LogMesg("passed ccc\n");
  // Compile function body
  CompileInstr(TagVal::FromWordDirect(abstractCode->Sel(4)));
  char *end = jit_get_ip().ptr;
  jit_flush_code(start, end);
  // Copy generated code
  u_int size    = (end - start);
  ::Chunk *code = Store::AllocChunk(size);
  memcpy(code->GetBase(), start, size);
  return code;
}

NativeConcreteCode *
NativeCodeJitter::Compile(TagVal *abstractCode, bool needCC) {
  // Compile Code
  ::Chunk *code = CompileCode(abstractCode, needCC);
  // Export resulting ConcreteCode
  NativeConcreteCode *concreteCode =
    NativeConcreteCode::New(abstractCode, code,
			    ImmediateEnv::ExportEnv(),
			    abstractCode->Sel(2));
  return concreteCode;
}

void NativeCodeJitter::Specialize(word cCode) {
  NativeConcreteCode *concreteCode = NativeConcreteCode::FromWordDirect(cCode);
  Transform *transform =
    (Transform *) concreteCode->GetAbstractRepresentation(); 
  TagVal *abstractCode = TagVal::FromWordDirect(transform->GetArgument());
  ::Chunk *code        = CompileCode(abstractCode, true);
  concreteCode->UpdateCode(code, ImmediateEnv::ExportEnv());
}

void NativeCodeJitter::Disassemble(::Chunk *code) {
  char *base = code->GetBase();
  disassemble(stderr, base, base + code->GetSize());
}
