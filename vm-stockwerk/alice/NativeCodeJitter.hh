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

class NativeCodeJitter : private JITStore {
protected:
  static jit_insn *codeBuffer;
  static u_int codeBufferSize;

  static word initialPC;
  static word instructionStartPC;

  static const u_int SHARED_TABLE_SIZE = 512;
  static HashTable *sharedTable;
  
  static Tuple *livenessTable;
  // Environment Accessors
  static void LocalEnvSel(u_int Dest, word pos);
  static void LocalEnvPut(word pos, u_int Value);
  static void LocalEnvKill(word pos);
  static void KillIdRef(word idRef);
  static void GlobalEnvSel(u_int Dest, word pos);
  static void ImmediateSel(u_int Dest, u_int pos);
  static void LazySelClosureNew(word tuple, word index);
  // StackFrame Accessors
  static void PushCall(u_int Closure, void *proc);
  static void BranchToOffset();
  static u_int GetRelativePC();
  static void SetRelativePC(word pc);
  // Calling Convention Conversion
  static void CompileCC(TagVal *idDefArgs);
  // NativeCodeJitter Instruction Helpers
  static void LoadIdRefKill(u_int Dest, word idRef);
  static void Await(word pc);
  static void LoadIdRef(u_int Dest, word idRef, word pc);
  static void BlockOnTransient(word pc);
  static void LookupTestTable(u_int Key, u_int table);
  // NativeCodeJitter Instructions
  static TagVal *InstrKill(TagVal *pc);
  static TagVal *InstrPutVar(TagVal *pc);
  static TagVal *InstrPutNew(TagVal *pc);
  static TagVal *InstrPutTag(TagVal *pc);
  static TagVal *InstrPutCon(TagVal *pc);
  static TagVal *InstrPutRef(TagVal *pc);
  static TagVal *InstrPutTup(TagVal *pc);
  static TagVal *InstrPutVec(TagVal *pc);
  static TagVal *InstrClose(TagVal *pc);
  static TagVal *InstrSpecialize(TagVal *pc);
  static TagVal *InstrAppPrim(TagVal *pc);
  static TagVal *InstrAppVar(TagVal *pc);
  static TagVal *InstrGetRef(TagVal *pc);
  static TagVal *InstrGetTup(TagVal *pc);
  static TagVal *InstrSel(TagVal *pc);
  static TagVal *InstrLazySel(TagVal *pc);
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
  static ::Chunk *CompileCode(TagVal *abstractcode, bool needCC);
public:
  // NativeCodeJitter Static Constructor
  static void Init(u_int bufferSize);
  // NativeCodeJitter Methods
  static word GetInitialPC() {
    return initialPC;
  }
  static NativeConcreteCode *Compile(TagVal *function, bool needCC);
  static void Specialize(word cCode);
  static void PrintPC(const char *instr);
  static void Disassemble(::Chunk *code);
};

#endif
