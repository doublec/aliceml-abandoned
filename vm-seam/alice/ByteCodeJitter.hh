//
// Author:
//   Christian Mueller <cmueller@ps.uni-sb.de>
//
// Copyright:
//   Christian Mueller, 2005
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE_BYTE_CODE_JITTER_HH__
#define __ALICE_BYTE_CODE_JITTER_HH__

#if defined(INTERFACE)
#pragma interface "alice/ByteCodeJitter.hh"
#endif

#include "alice/Base.hh"
#include "alice/Data.hh"
#include "alice/JitterImmediateEnv.hh"

#define DO_REG_ALLOC
//#undef DO_REG_ALLOC

#define INITIAL_SHAREDTABLE_SIZE 256

/* We can try to omit a range check if we fill the missing slots in the
   jump table with the PC of the "else" part. This optimization is used if
   actualNumberOfTests * OPTIMIZE_TAGTEST_LEVEL > maxTag
*/
#define OPTIMIZE_TAGTEST_LEVEL 5

#define NUMBER_OF_SCRATCH_REGISTERS 4

class LazyByteCompileClosure;

class AliceDll ByteCodeJitter {
private:
  u_int currentNLocals;
  u_int PC;
  ImmediateEnv imEnv;
  IntMap *sharedTable;
  Vector *globalSubst;

#ifdef DO_REG_ALLOC
  u_int *mapping;
#endif
  
  // inlining of primitives
  enum { INT_PLUS, INT_MINUS, 
	 REF_ASSIGN, 
	 FUTURE_AWAIT, FUTURE_BYNEED,
	 HOLE_HOLE, HOLE_FILL,
	 INLINE_TABLE_SIZE }; 
  static void* inlineTable[INLINE_TABLE_SIZE]; 
  
  // scratch registers
  u_int scratch;
  u_int nRegisters;
  u_int delayedScratchInc;

  u_int GetNewScratch() { return scratch++; }

  u_int IdToReg(word id) {
#ifdef DO_REG_ALLOC
    return mapping[Store::DirectWordToInt(id)];
#else
    return Store::DirectWordToInt(id);
#endif
  }

  TagVal *LookupSubst(u_int index) {
    return TagVal::FromWordDirect(globalSubst->Sub(index));
  }

  u_int LoadIdRefKill(word idRef, bool doIncScratch);

  // inlined primitives
  TagVal *Inline_IntPlus(Vector *args, TagVal *idDefInstrOpt);
  TagVal *Inline_IntMinus(Vector *args, TagVal *idDefInstrOpt);
  TagVal *Inline_RefAssign(Vector *args, TagVal *idDefInstrOpt);
  TagVal *Inline_FutureAwait(Vector *args, TagVal *idDefInstrOpt);
  TagVal *Inline_FutureByneed(Vector *args, TagVal *idDefInstrOpt);
  TagVal *Inline_HoleHole(Vector *args, TagVal *idDefInstrOpt);
  TagVal *Inline_HoleFill(Vector *args, TagVal *idDefInstrOpt);

  bool InlinePrimitive(void *cFunction, 
		       Vector *args, TagVal *idDefInstrOpt, 
		       TagVal **continuation);

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

  void CompileCCC(u_int inArity,Vector *rets);
  void CompileInstr(TagVal *pc);
  void CompileApplyPrimitive(Closure *closure, Vector *args, bool isTailcall);

public:
  ByteCodeJitter();
  ~ByteCodeJitter();
  static void Init(); // initializes static variables
  word Compile(LazyByteCompileClosure *compClosure);
};

#endif
