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

class IntMap;
class TagVal;
class Vector;
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

class NativeCodeJitter : private JITStore {
protected:
  static jit_insn *codeBuffer;
  static u_int codeBufferSize;

  static word initialPC;
  static word initialNoCCCPC;
  static word instructionStartPC;

  static const u_int SHARED_TABLE_SIZE = 512;
  static IntMap *sharedTable;
  
  static LivenessTable *livenessTable;
  static LivenessTable *livenessFreeList;
  static Tuple *assignment;
  static Vector *globalSubst;
  static word inlineTable;
  static u_int currentNLocals;
  static TagVal *currentArgs;
  static u_int currentStack;
  // Environment Accessors
  static u_int RefToIndex(word ref);
  static u_int LocalEnvSel(u_int Dest, u_int Ptr, u_int pos);
  static void LocalEnvPut(u_int Ptr, word pos, u_int Value);
  static void MoveIndexValToLocalEnv(word pos, u_int This, u_int i);
  static void MoveBlockValToLocalEnv(word pos, u_int This, u_int i);
  static void MoveMemValToLocalEnv(word pos, void *addr);
  static void BindIdDefs(word wIdDefs, u_int This, u_int baseOffset);
  static void KillIdRef(word idRef);
  static void GlobalEnvSel(u_int Dest, u_int Ptr, word pos);
  static void ImmediateSel(u_int Dest, u_int Ptr, u_int pos);
  static TagVal *LookupSubst(u_int index);
  static void LazySelClosureNew(u_int Record, UniqueString *label);
  // StackFrame Accessors
  static void SaveRegister();
  static void RestoreRegister();
  static void PushCall(CallInfo *info);
  static void DirectCall(Interpreter *interpreter);
  static void TailCall(CallInfo *info);
  static void BranchToOffset(u_int wOffset);
  static u_int GetRelativePC();
  static void SetRelativePC(word pc);
  // Calling Convention Conversion
  static void CompileCCC(u_int calleeArity, bool update = false);
  static void StoreResults(u_int calleeArity, TagVal *idDefArgs);
  // NativeCodeJitter Instruction Helpers
  static u_int LoadIdRefKill(u_int Dest, word idRef);
  static void Await(u_int Ptr, word pc);
  static u_int LoadIdRef(u_int Dest, word idRef, word pc);
  static u_int ReloadIdRef(u_int Dest, word idRef);
  static void KillVariables();
  static void BlockOnTransient(u_int Ptr, word pc);
  static void LoadStatus(u_int Dest);
  static void CheckPreempt(u_int pc);
  static void CheckPreemptImmediate(u_int pc);
  static void LookupTestTable(u_int Key, u_int table, bool isInt = true);
  static u_int InlinePrimitive(word wPrimitive, Vector *actualIdRefs);
  static void CompileContinuation(TagVal *idDefArgsInstrOpt, u_int nLocals = 0);
  static void LoadArguments(TagVal *actualArgs);
  static TagVal *CheckBoolTest(word pos, u_int Result, word next);
  static TagVal *Apply(TagVal *pc, Closure *closure, bool direct);
  static void CompileConsequent(word conseq, u_int TagValue);
  static void NullaryBranches(u_int Tag, Vector *tests);
  static void NonNullaryBranches(u_int Tag, Vector *tests);
  // NativeCodeJitter Instructions
  static TagVal *InstrKill(TagVal *pc);
  static TagVal *InstrPutVar(TagVal *pc);
  static TagVal *InstrPutNew(TagVal *pc);
  static TagVal *InstrPutTag(TagVal *pc);
  static TagVal *InstrPutCon(TagVal *pc);
  static TagVal *InstrPutRef(TagVal *pc);
  static TagVal *InstrPutTup(TagVal *pc);
  static TagVal *InstrPutPolyRec(TagVal *pc);
  static TagVal *InstrPutVec(TagVal *pc);
  static TagVal *InstrClose(TagVal *pc);
  static TagVal *InstrSpecialize(TagVal *pc);
  static TagVal *InstrAppPrim(TagVal *pc);
  static TagVal *InstrAppVar(TagVal *pc, bool direct = false);
  static TagVal *InstrGetRef(TagVal *pc);
  static TagVal *InstrGetTup(TagVal *pc);
  static TagVal *InstrSel(TagVal *pc);
  static TagVal *InstrLazyPolySel(TagVal *pc);
  static TagVal *InstrRaise(TagVal *pc);
  static TagVal *InstrReraise(TagVal *pc);
  static TagVal *InstrTry(TagVal *pc);
  static TagVal *InstrEndTry(TagVal *pc);
  static TagVal *InstrEndHandle(TagVal *pc);
  static TagVal *InstrIntTest(TagVal *pc);
  static TagVal *InstrCompactIntTest(TagVal *pc);
  static TagVal *InstrRealTest(TagVal *pc);
  static TagVal *InstrStringTest(TagVal *pc);
  static TagVal *InstrTagTest(TagVal *pc);
  static TagVal *InstrCompactTagTest(TagVal *pc);
  static TagVal *InstrConTest(TagVal *pc);
  static TagVal *InstrVecTest(TagVal *pc);
  static TagVal *InstrShared(TagVal *pc);
  static TagVal *InstrReturn(TagVal *pc);
  // Function compilation
  static char *CompileProlog(const char *info);
  static void CompileBranch(TagVal *pc);
  static void CompileInstr(TagVal *pc);
  static Tuple *AllocateRegister(u_int nLocals, Tuple *liveness);
public:
  static word currentConcreteCode; // Set by LazyCompile::Run 
  // NativeCodeJitter Static Constructor
  static ::Chunk *CopyCode(char *start);
  static void Init(u_int bufferSize);
  // NativeCodeJitter Methods
  static word GetInitialPC() {
    return initialPC;
  }
  static NativeConcreteCode *Compile(TagVal *abstractCode);
#if defined(JIT_STORE_DEBUG)
  static void Disassemble(::Chunk *code);
#endif
#ifdef INSTRUCTION_COUNTS
  static void DumpInstructionCounts();
#endif
};

#endif

#endif
