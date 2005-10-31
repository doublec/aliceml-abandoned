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
#include "alice/ByteCodeInliner.hh"

#define DO_REG_ALLOC
//#undef DO_REG_ALLOC

// We can try to omit a range check if we fill the missing slots in the
// jump table with the PC of the "else" part. This optimization is used if
// actualNumberOfTests * OPTIMIZE_TAGTEST_LEVEL > maxTag
#define OPTIMIZE_TAGTEST_LEVEL 5

class LazyByteCompileClosure;

class ByteCodeImmediateEnv {
protected:
  u_int index;
  u_int size;
  Tuple *values;
  Map *map;
public:
  ByteCodeImmediateEnv() {}

  void Init() {
    index  = 0;
    size   = 5;
    values = Tuple::New(size);
    // remember addresses for sharing
    map = Map::New(size);
  }
  u_int Register(word item) {
    Assert(item != (word) 0);
    if(map->IsMember(item))
      return Store::DirectWordToInt(map->Get(item));
    if (index >= size) {
      u_int oldsize = size;
      size = ((size * 3) >> 1); 
      Tuple *newValues = Tuple::New(size);
      for (u_int i = oldsize; i--;)
	newValues->Init(i, values->Sel(i));
      values = newValues;
    }
    values->Init(index, item);
    map->Put(item,Store::IntToWord(index));
    return index++;
  }
  word Sel(u_int index) {
    return values->Sel(index);
  }
  void Replace(u_int index, word item) {
    values->Init(index, item);
  }
  word ExportEnv() {
    return values->ToWord();
  }
};


class AliceDll ByteCodeJitter {
private:
  u_int currentNLocals;
  u_int PC;
  ByteCodeImmediateEnv imEnv;
  IntMap *sharedTable;
  Vector *globalSubst;
  word currentConcreteCode;
  u_int skipCCCPC;
  Vector *currentFormalInArgs;
  
  // offset for inline functions
  u_int localOffset;
  Vector *currentFormalArgs;

  // patch forward jumps in CompileInlineFunctions
  class PatchTable {
  private:
    u_int *table;
    u_int size, top;
    static u_int jumpInstrSize;
  public:
    PatchTable();
    ~PatchTable() { delete table; }
    static void Init();
    void Add(u_int addr) {
      if(top >= size) {
	size = size * 3 / 2;
	u_int *newTable = new u_int[size];
	memcpy(newTable,table,top*sizeof(u_int));
	delete table;
	table = newTable;
      }
      table[top++] = addr;
    }
    u_int Sub(u_int i) { return table[i]; }
    u_int GetLength() { return top; }
    u_int Clear() { top = 0; }
    u_int GetJumpInstrSize() { return jumpInstrSize; }
  };
  PatchTable *patchTable;

  // this is the result of the inlining analysis
  InlineInfo *inlineInfo;

  // this variable indicates the current compilation depth 
  u_int inlineDepth;

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
//     fprintf(stderr,"localOffset %d, id %d, %d -> %d\n",
// 	    localOffset,Store::DirectWordToInt(id),
// 	    Store::DirectWordToInt(id) + localOffset,
// 	    mapping[Store::DirectWordToInt(id) + localOffset]);
    return mapping[Store::DirectWordToInt(id) + localOffset];
#else
    return Store::DirectWordToInt(id) + localOffset;
#endif
  }

  word ExtractImmediate(word idRef);

  TagVal *LookupSubst(u_int index) {
    return TagVal::FromWordDirect(globalSubst->Sub(index));
  }

  void LoadIdRefInto(u_int dst, word idRef);
  u_int LoadIdRefKill(word idRef, bool doIncScratch);

  // inlined primitives
  void InlinePrimitiveReturn(u_int reg);
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
  void CompileSelfCall(TagVal *instr, bool isTailcall);
  // inlining
  void CompileInlineCCC(Vector *formalArgs, Vector *args, bool isReturn);
  TagVal *CompileInlineFunction(TagVal *abstractCode, 
				InlineInfo *info,
				Vector *subst,		   
				u_int offset,
				Vector *args,
				TagVal *idDefsInstrOpt);

public:
  ByteCodeJitter();
  ~ByteCodeJitter();
  static void Init(); // initializes static variables
  word Compile(LazyByteCompileClosure *compClosure);
};

#endif
