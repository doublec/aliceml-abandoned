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

#ifndef __ALICE__NATIVE_CODE_JITTER_HH_
#define __ALICE__NATIVE_CODE_JITTER_HH_

#if HAVE_LIGHTNING

#if defined(INTERFACE)
#pragma interface "alice/NativeCodeJitter.hh"
#endif

#include "alice/NativeConcreteCode.hh"
#include "alice/JitterImmediateEnv.hh"
#include "alice/NativeCodeInterpreter.hh"
#include "alice/JitterAliceData.hh"

class LivenessTable;

typedef enum {
  HOLE_HOLE,
  FUTURE_BYNEED,
  CHAR_ORD,
  INT_OPPLUS,
  INT_OPSUB,
  INT_OPMUL,
  INT_OPLESS
} INLINED_PRIMITIVE;

typedef enum {
  NORMAL_CALL,
  SELF_CALL,
  NATIVE_CALL
} CALL_MODE;

typedef struct {
  CALL_MODE mode;
  u_int pc;
  u_int closure;
  u_int nLocals;
} CallInfo;

#define ALICE_REGISTER_NB 3

class NativeCodeJitter : public JITAlice, ImmediateEnv {
protected:
  static word inlineTable;
  static u_int codeBufferSize;

  word initialPC;
  word initialNoCCCPC;
  jit_insn *codeBuffer;

  IntMap *sharedTable;
  LivenessTable *livenessTable;
  LivenessTable *livenessFreeList;
  Tuple *assignment;
  Vector *globalSubst;
  word currentConcreteCode;
  u_int currentNLocals;
  TagVal *currentArgs;
  u_int currentStack;

  // NativeCodeFrame
  enum {
    NATIVECODEFRAME_PC_POS,
    NATIVECODEFRAME_CODE_POS,
    NATIVECODEFRAME_CLOSURE_POS,
    NATIVECODEFRAME_IMMEDIATE_ARGS_POS,
    NATIVECODEFRAME_CONTINUATION_POS,
    NATIVECODEFRAME_BASE_SIZE
  };
  // Side-Effect: Scratches JIT_R0, JIT_FP
  void NativeCodeFrame_New(u_int This, u_int nLocals) {
    StackFrame_New(This, NATIVECODEFRAME_BASE_SIZE + nLocals,
		   NativeCodeInterpreter::self);
  }
  // Side-Effect: Scratches JIT_R0, JIT_FP
  void NativeCodeFrame_NewNoCheck(u_int This, u_int nLocals) {
    StackFrame_NewNoCheck(This, NATIVECODEFRAME_BASE_SIZE + nLocals,
			  NativeCodeInterpreter::self);
  }
  void NativeCodeFrame_NewPopAndPush(u_int This,
				     u_int newNLocals,
				     u_int oldNLocals) {
    StackFrame_NewPopAndPush(This,
			     NATIVECODEFRAME_BASE_SIZE + newNLocals,
			     NATIVECODEFRAME_BASE_SIZE + oldNLocals,
			     NativeCodeInterpreter::self);
  }
  u_int NativeCodeFrame_GetFrameSize(u_int nLocals) {
    return (STACKFRAME_BASE_SIZE + NATIVECODEFRAME_BASE_SIZE + nLocals);
  }
  void NativeCodeFrame_GetImmediateArgs(u_int Dest, u_int This) {
    StackFrame_Sel(Dest, This, NATIVECODEFRAME_IMMEDIATE_ARGS_POS);
  }
  void NativeCodeFrame_GetSize(u_int Dest, u_int This) {
    NativeCodeFrame_GetImmediateArgs(Dest, This);
    Tuple_Sel(Dest, Dest, 0);
  }
  void NativeCodeFrame_GetPC(u_int Dest, u_int This) {
    StackFrame_Sel(Dest, This, NATIVECODEFRAME_PC_POS);
  }
  void NativeCodeFrame_PutPC(u_int This, u_int Value) {
    StackFrame_Put(This, NATIVECODEFRAME_PC_POS, Value);
  }
  void NativeCodeFrame_GetCode(u_int Dest, u_int This) {
    StackFrame_Sel(Dest, This, NATIVECODEFRAME_CODE_POS);
  }
  void NativeCodeFrame_PutCode(u_int This, u_int Value) {
    StackFrame_Put(This, NATIVECODEFRAME_CODE_POS, Value);
  }
  void NativeCodeFrame_GetClosure(u_int Dest, u_int This) {
    StackFrame_Sel(Dest, This, NATIVECODEFRAME_CLOSURE_POS);
  }
  void NativeCodeFrame_PutClosure(u_int This, u_int Value) {
    StackFrame_Put(This, NATIVECODEFRAME_CLOSURE_POS, Value);
  }
  void NativeCodeFrame_PutImmediateArgs(u_int This, u_int Value) {
    StackFrame_Put(This, NATIVECODEFRAME_IMMEDIATE_ARGS_POS, Value);
  }
  void NativeCodeFrame_GetContinuation(u_int Dest, u_int This) {
    StackFrame_Sel(Dest, This, NATIVECODEFRAME_CONTINUATION_POS);
  }
  void NativeCodeFrame_PutContinuation(u_int This, u_int Value) {
    StackFrame_Put(This, NATIVECODEFRAME_CONTINUATION_POS, Value);
  }
  void NativeCodeFrame_ReplaceClosure(u_int This, u_int Closure) {
    StackFrame_Replace(This, NATIVECODEFRAME_CLOSURE_POS, Closure);
  }
  void NativeCodeFrame_GetEnv(u_int Dest, u_int This, u_int pos) {
    StackFrame_Sel(Dest, This, NATIVECODEFRAME_BASE_SIZE + pos);
  }
  void NativeCodeFrame_PutEnv(u_int This, u_int pos, u_int Value) {
    StackFrame_Put(This, NATIVECODEFRAME_BASE_SIZE + pos, Value);
  }
  void NativeCodeFrame_ReplaceEnv(u_int This, u_int pos, u_int Value) {
    StackFrame_Replace(This, NATIVECODEFRAME_BASE_SIZE + pos, Value);
  }

  // NativeConcreteCode
  enum {
    NATIVECONCRETECODE_TRANSFORM_POS,
    NATIVECONCRETECODE_NATIVE_CODE_POS,
    NATIVECONCRETECODE_IMMEDIATE_ENV_POS,
    NATIVECONCRETECODE_NLOCALS_POS,
    NATIVECONCRETECODE_SKIP_CCC_PC_POS,
    NATIVECONCRETECODE_SIZE
  };
  void NativeConcreteCode_GetNativeCode(u_int Dest, u_int This) {
    ConcreteCode_Sel(Dest, This, NATIVECONCRETECODE_NATIVE_CODE_POS);
  }
  void NativeConcreteCode_GetImmediateArgs(u_int Dest, u_int This) {
    ConcreteCode_Sel(Dest, This, NATIVECONCRETECODE_IMMEDIATE_ENV_POS);
  }

  // Environment Accessors
  u_int RefToIndex(word ref);
  u_int LocalEnvSel(u_int Dest, u_int Ptr, u_int pos);
  void LocalEnvPut(u_int Ptr, word pos, u_int Value);
  void MoveIndexValToLocalEnv(word pos, u_int This, u_int i);
  void MoveBlockValToLocalEnv(word pos, u_int This, u_int i);
  void MoveMemValToLocalEnv(word pos, void *addr);
  void BindIdDefs(word wIdDefs, u_int This, u_int baseOffset);
  void KillIdRef(word idRef);
  void GlobalEnvSel(u_int Dest, u_int Ptr, word pos);
  void ImmediateSel(u_int Dest, u_int Ptr, u_int pos);
  TagVal *LookupSubst(u_int index);
  void LazySelClosureNew(u_int Record, UniqueString *label);
  // StackFrame Accessors
  void SaveRegister();
  void RestoreRegister();
  void PushCall(CallInfo *info);
  void DirectCall(Interpreter *interpreter);
  void TailCall(CallInfo *info);
  void BranchToOffset(u_int wOffset);
  u_int GetRelativePC();
  void SetRelativePC(word pc);
  // Calling Convention Conversion (Arity Raising)
  void CompileCCC(u_int calleeArity, bool update = false);
  void StoreResults(u_int calleeArity, TagVal *idDefArgs);
  // NativeCodeJitter Instruction Helpers
  u_int LoadIdRefKill(u_int Dest, word idRef);
  void Await(u_int Ptr, word pc);
  u_int LoadIdRef(u_int Dest, word idRef, word pc);
  u_int ReloadIdRef(u_int Dest, word idRef);
  void KillVariables();
  void BlockOnTransient(u_int Ptr, word pc);
  void LoadStatus(u_int Dest);
  void CheckPreempt(u_int pc);
  void CheckPreemptImmediate(u_int pc);
  void LookupTestTable(u_int Key, u_int table, bool isInt = true);
  u_int InlinePrimitive(word wPrimitive, Vector *actualIdRefs);
  void CompileContinuation(TagVal *idDefArgsInstrOpt, u_int nLocals = 0);
  void LoadArguments(TagVal *actualArgs);
  TagVal *CheckBoolTest(word pos, u_int Result, word next);
  TagVal *Apply(TagVal *pc, Closure *closure, bool direct);
  void CompileConsequent(word conseq, u_int TagValue);
  void NullaryBranches(u_int Tag, Vector *tests);
  void NonNullaryBranches(u_int Tag, Vector *tests);
  // AbstractCode Instructions
  TagVal *InstrKill(TagVal *pc);
  TagVal *InstrPutVar(TagVal *pc);
  TagVal *InstrPutNew(TagVal *pc);
  TagVal *InstrPutTag(TagVal *pc);
  TagVal *InstrPutCon(TagVal *pc);
  TagVal *InstrPutRef(TagVal *pc);
  TagVal *InstrPutTup(TagVal *pc);
  TagVal *InstrPutPolyRec(TagVal *pc);
  TagVal *InstrPutVec(TagVal *pc);
  TagVal *InstrClose(TagVal *pc);
  TagVal *InstrSpecialize(TagVal *pc);
  TagVal *InstrAppPrim(TagVal *pc);
  TagVal *InstrAppVar(TagVal *pc, bool direct = false);
  TagVal *InstrGetRef(TagVal *pc);
  TagVal *InstrGetTup(TagVal *pc);
  TagVal *InstrSel(TagVal *pc);
  TagVal *InstrLazyPolySel(TagVal *pc);
  TagVal *InstrRaise(TagVal *pc);
  TagVal *InstrReraise(TagVal *pc);
  TagVal *InstrTry(TagVal *pc);
  TagVal *InstrEndTry(TagVal *pc);
  TagVal *InstrEndHandle(TagVal *pc);
  TagVal *InstrIntTest(TagVal *pc);
  TagVal *InstrCompactIntTest(TagVal *pc);
  TagVal *InstrRealTest(TagVal *pc);
  TagVal *InstrStringTest(TagVal *pc);
  TagVal *InstrTagTest(TagVal *pc);
  TagVal *InstrCompactTagTest(TagVal *pc);
  TagVal *InstrConTest(TagVal *pc);
  TagVal *InstrVecTest(TagVal *pc);
  TagVal *InstrShared(TagVal *pc);
  TagVal *InstrReturn(TagVal *pc);

  // Function compilation
  char *CompileProlog(const char *info);
  void CompileBranch(TagVal *pc);
  void CompileInstr(TagVal *pc);
  Tuple *AllocateRegister(u_int nLocals, Tuple *liveness);
  Chunk *CopyCode(char *start);
public:
  static void Init(u_int codeSize);

  NativeCodeJitter();
  ~NativeCodeJitter();

  NativeConcreteCode *Compile(word concreteCode, TagVal *abstractCode);
#ifdef INSTRUCTION_COUNTS
  void DumpInstructionCounts();
#endif
};

#endif

#endif
