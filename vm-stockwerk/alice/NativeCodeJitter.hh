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

#if defined(INTERFACE)
#pragma interface "alice/NativeCodeJitter.hh"
#endif

#include "store/JITStore.hh"
#include "alice/NativeConcreteCode.hh"

class HashTable;
class TagVal;
class LivenessTable;
#if defined(ALICE_IMPLICIT_KILL)
class LivenessInformation;
#endif

typedef enum {
  FUTURE_BYNEED,
  CHAR_ORD,
  INT_OPPLUS,
  INT_OPMUL,
  INT_OPLESS
} INLINED_PRIMITIVE;

#define ALICE_REGISTER_NB 3

class NativeCodeJitter : private JITStore {
protected:
  static jit_insn *codeBuffer;
  static u_int codeBufferSize;

  static word initialPC;
  static word instructionStartPC;

  static const u_int SHARED_TABLE_SIZE = 512;
  static HashTable *sharedTable;
  
  static LivenessTable *livenessTable;
  static LivenessTable *livenessFreeList;
  static Tuple *assignment;
#if defined(ALICE_IMPLICIT_KILL)
  static LivenessInformation *livenessInfo;
  static u_int rowIndex;
#endif
  static word inlineTable;
  // Environment Accessors
  static u_int RefToIndex(word ref);
  static u_int LocalEnvSel(u_int Dest, u_int Ptr, u_int pos);
  static void LocalEnvPut(u_int Ptr, word pos, u_int Value);
  static void KillIdRef(word idRef);
  static void GlobalEnvSel(u_int Dest, u_int Ptr, word pos);
  static void ImmediateSel(u_int Dest, u_int Ptr, u_int pos);
  static void LazySelClosureNew(u_int tuple, u_int label);
  // StackFrame Accessors
  static void Prepare();
  static void Finish();
  static void SaveRegister();
  static void RestoreRegister();
  static void PushCall(u_int Closure);
  static void DirectCall(Interpreter *interpreter);
  static void TailCall(u_int Closure, bool isSelf);
  static void BranchToOffset(u_int wOffset);
  static u_int GetRelativePC();
  static void SetRelativePC(word pc);
  // Calling Convention Conversion
  static void CompileCCC(TagVal *idDefArgs);
  // NativeCodeJitter Instruction Helpers
  static u_int LoadIdRefKill(u_int Dest, word idRef);
  static void Await(u_int Ptr, word pc);
  static u_int LoadIdRef(u_int Dest, word idRef, word pc);
  static void KillVariables(word pc);
  static void BlockOnTransient(u_int Ptr, word pc);
  static void LookupTestTable(u_int Key, u_int table);
  static void InlineAppPrim(INLINED_PRIMITIVE primitive, TagVal *pc);
  static void NormalAppPrim(Closure *closure, TagVal *pc);
  static void AppVar(TagVal *pc, bool isSelf);
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
  static TagVal *InstrAppVar(TagVal *pc);
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
  static void Init(u_int bufferSize);
  // NativeCodeJitter Methods
  static word GetInitialPC() {
    return initialPC;
  }
  static NativeConcreteCode *Compile(TagVal *abstractCode);
  static void PrintPC(const char *instr);
#if defined(JIT_STORE_DEBUG)
  static void Disassemble(::Chunk *code);
#endif
};

#endif
