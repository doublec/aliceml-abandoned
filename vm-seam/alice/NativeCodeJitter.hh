//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002-2003
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

#include <setjmp.h>

class LivenessTable;
class TableAllocator;

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
  NATIVE_CALL,
  PRIMITIVE_CALL,
} CALL_TYPE;

typedef enum {
  MODE_REQUEST_CONCRETE_CODE,
  MODE_REQUEST_ALL,
  MODE_NO_REQUEST,
  MODE_INLINE_PRIMITIVE,
  MODE_CALL_PRIMITIVE
} CALL_MODE;

typedef struct {
  CALL_TYPE type;
  CALL_MODE mode;
  u_int pc;
  u_int closure;
  u_int nLocals;
  u_int inArity;
  u_int outArity;
  Interpreter *interpreter;
} CallInfo;

#define ALICE_REGISTER_NB 3
#define CODE_BUFFER_SECURITY STORE_MEMCHUNK_SIZE

class NativeCodeJitter : public JITAlice, ImmediateEnv {
protected:
  static word inlineTable;
  static u_int codeBufferSize;
  static u_int boolTest;

  word initialPC;
  word initialNoCCCPC;
  jit_insn *codeBuffer;
  char *codeStart;

  IntMap *sharedTable;
  LivenessTable *livenessTable;
  TableAllocator *tableAllocator;
  Tuple *assignment;
  Vector *globalSubst;
  word currentConcreteCode;
  u_int currentNLocals;
  u_int currentOutArity;
  Vector *currentArgs;
  u_int currentStack;
  jmp_buf jumpEnv;

  // NativeCodeFrame
  enum {
    NATIVECODEFRAME_PC_POS,
    NATIVECODEFRAME_CODE_POS,
    NATIVECODEFRAME_CLOSURE_POS,
    NATIVECODEFRAME_IMMEDIATE_ARGS_POS,
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
  void StoreResults(u_int calleeArity, Vector *idDefs);
  // NativeCodeJitter Instruction Helpers
  u_int LoadIdRefKill(u_int Dest, word idRef);
  void Await(u_int Ptr, word pc);
  u_int LoadIdRef(u_int Dest, word idRef, word pc);
  u_int ReloadIdRef(u_int Dest, word idRef);
  void KillVariables(bool speculativePath = false);
  void BlockOnTransient(u_int Ptr, word pc);
  void LoadStatus(u_int Dest);
  void CheckPreempt(u_int pc);
  void CheckPreemptImmediate(u_int pc);
  void LookupTestTable(u_int Key, u_int table, bool isInt = true);
  u_int InlinePrimitive(word wPrimitive, Vector *actualIdRefs);
  word CompileContinuation(TagVal *idDefArgsInstrOpt,
			   u_int outArity, u_int nLocals = 0);
  void LoadArguments(Vector *actualIdRefs);
  TagVal *CheckBoolTest(word pos, u_int Result, word next);
  void AnalyzeApply(CallInfo *info, TagVal *pc, word wClosure);
  TagVal *CompileApplyPrimitive(CallInfo *info, TagVal *pc);
  TagVal *CompileApply(CallInfo *info, TagVal *pc);
  void CompileConsequent(word conseq, u_int tagSel, u_int TagValue);
  void NullaryBranches(u_int Tag, Vector *tests);
  void NonNullaryBranches(u_int Tag, u_int tagSel, Vector *tests);
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
  TagVal *InstrAppVar(TagVal *pc);
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
  void CompileBranch(TagVal *pc, u_int nLocals = 0);
  void CompileInstr(TagVal *pc);
  Tuple *AllocateRegister(u_int nLocals, Tuple *liveness);
  Chunk *CopyCode(char *start);
  void CheckCodeBuffer(void);
public:
  static void Init(u_int codeSize);

  NativeCodeJitter();
  ~NativeCodeJitter();

  void Disassemble(Chunk*);

  word Compile(LazyCompileClosure *lazyCompileClosure);
#ifdef INSTRUCTION_COUNTS
  void DumpInstructionCounts();
#endif
};

#endif

#endif
