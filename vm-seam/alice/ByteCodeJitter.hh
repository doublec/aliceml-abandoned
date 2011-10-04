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
#include "alice/ByteCode.hh"
#include "alice/ByteCodeInliner.hh"
#include "alice/ByteCodeConstProp.hh"

#define DO_REG_ALLOC
//#undef DO_REG_ALLOC

#define DO_INLINING
//#undef DO_INLINING

#ifdef DO_INLINING
#define DO_CONSTANT_PROPAGATION
#else
#undef DO_CONSTANT_PROPAGATION
#endif
//#undef DO_CONSTANT_PROPAGATION

// We can try to omit a range check if we fill the missing slots in the
// jump table with the PC of the "else" part. This optimization is used if
// actualNumberOfTests * OPTIMIZE_TAGTEST_LEVEL > maxTag
#define OPTIMIZE_TAGTEST_LEVEL 10


class HotSpotCode;

//! class for construction of immediate environment
/*!
  This class is used by the compiler to construct an immediate
  environment.
*/
class ByteCodeImmediateEnv {
protected:
  u_int index;
  u_int size;
  Tuple *values; //!< actual values
  Map *map; //!< mapping to detect sharing
public:
  //! constructor
  ByteCodeImmediateEnv() {}

  //! initialization of the immediate environment
  void Init() {
    index  = 0;
    size   = 5;
    values = Tuple::New(size);
    // remember addresses for sharing
    map = Map::New(size);
  }

  //! save value in the immediate environment
  /*!
    Method to register a value in the immediate environment. Sharing
    is automatically detected
    @param item value that should be registered
    @return index into environment where the value is stored
  */
  u_int Register(word item) {
    Assert(item != reinterpret_cast<word>(0));
    if(!PointerOp::IsTransient(item) && map->IsMember(item))
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
    if(!PointerOp::IsTransient(item))
      map->Put(item,Store::IntToWord(index));
    return index++;
  }

  //! read from immediate environment
  /*!
    Select value from immediate environment. There is no range check.
    @param index index into environment.
    @return value that is stored under \p index
   */
  word Sel(u_int index) {
    return values->Sel(index);
  }

  //! replace a value in the environment
  /*!
    The slot \p index is updated with the value \p item.
    @param index index into immediate environment
    @param item value that should be stored unter \p index
   */
  void Replace(u_int index, word item) {
    values->Init(index, item);
  }
  //! export immediate environment
  /*!
    This method exports the immediate environment after compilation
    is finished.
    @return immediate environment is a tuple casted to word
   */
  word ExportEnv() {
    return values->ToWord();
  }
};

//! byte code JIT compiler
/*!
  This is the byte code jitter. It traverses the abstract code graph and
  emits a sematically equivalent byte code sequence.
*/

class AliceDll ByteCodeJitter {
private:
  u_int currentNLocals;
  u_int currentOutArity;
  u_int PC;
  ByteCodeImmediateEnv imEnv;
  IntMap *sharedTable;
  Vector *globalSubst;
  AppVarInfo *inlineAppVar;                /** AppVarInfo for AppVar instr that is currently being inlined, or INVALID_POINTER when inlineDepth == 0 */
  word currentConcreteCode;
  u_int skipCCCPC;
  Vector *currentFormalInArgs;
  
  // offset for inline functions
  u_int localOffset;
  Vector *currentFormalArgs;

  // result of constant propagation
#ifdef DO_CONSTANT_PROPAGATION
  ConstPropInfo *constPropInfo;
#endif

  // patch forward jumps in CompileInlineFunctions
  class PatchTable {
  private:
    u_int *table;
    u_int size, top;
    static u_int jumpInstrSize;
  public:
    PatchTable();
    ~PatchTable() { delete[] table; }
    static void Init();
    void Add(u_int addr) {
      if(top >= size) {
	size = size * 3 / 2;
	u_int *newTable = new u_int[size];
	memcpy(newTable,table,top*sizeof(u_int));
	delete[] table;
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
         INT_LESS, INT_GREATER, INT_LESS_EQ, INT_GREATER_EQ,
	 REF_ASSIGN, 
	 FUTURE_AWAIT, FUTURE_BYNEED,
	 HOLE_HOLE, HOLE_FILL,
	 EQUAL,
	 ARRAY_SUB, UNSAFE_ARRAY_SUB, ARRAY_LENGTH,
	 VECTOR_SUB, UNSAFE_VECTOR_SUB, VECTOR_LENGTH,
	 INLINE_TABLE_SIZE };
  static void* inlineTable[INLINE_TABLE_SIZE]; 
  
  // scratch registers
  u_int scratch;
  u_int nRegisters;
  bool topScratchReusable;

  u_int GetNewScratch(bool useReusable = false) {
    if (useReusable) {
      if (!topScratchReusable) {
        topScratchReusable = true;
        scratch++;
      }
    } else {
      topScratchReusable = false;
      scratch++;
    }
    return scratch - 1;
  }

  u_int IdToReg(word id) {
#ifdef DO_REG_ALLOC
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
  u_int LoadIdRefKill(word idRef, bool useReusableScratch);

  /*! @name Inlining of Primitives
    The methods in this group are used for inlining of primitives.
  */
  //@{
  //! main procedure for primitive inlining
  /*!
    This is the main procedure for primitive inlining.

    @param cFunction The function pointer points to the primitive. The 
    method reflects on this pointer to find primitives that can be inlined.

    @param args Actual arguments of the primitive \a cFunction.

    @param idDefInstrOpt Pair of formal argument for the return value
    and the continuation.

    @return The boolean indicates whether the primitive could be inlined or 
    not.
  */
  bool InlinePrimitive(void *cFunction, Vector *args, TagVal *idDefInstrOpt);

  //! compile return from an inlined primitive
  /*!
    The return from an inlined primitive call is normally trivial. If, however,
    procedure integration is enabled, things get more complex. In this case
    the return is simulated as a jump instruction to the caller.

    @param reg Register in which the return value is stored.
   */
  void InlinePrimitiveReturn(u_int reg);
  
  void InlineUnaryPrimitive(ByteCodeInstr::instr op, Vector *args, TagVal *idDefInstrOpt);
  void InlineBinaryPrimitive(ByteCodeInstr::instr op, Vector *args, TagVal *idDefInstrOpt);


  //@{
  //! various methods to inline specific primitives
  /*!
    Each method is used to inline a specific primitive.

    @param args Actual arguments of the primitive.

    @param idDefInstrOpt Pair of formal argument for the return value and
    of the contination.
  */
  void Inline_IntPlus(Vector *args, TagVal *idDefInstrOpt);
  void Inline_IntMinus(Vector *args, TagVal *idDefInstrOpt);
  void Inline_RefAssign(Vector *args, TagVal *idDefInstrOpt);
  void Inline_FutureAwait(Vector *args, TagVal *idDefInstrOpt);
  void Inline_FutureByneed(Vector *args, TagVal *idDefInstrOpt);
  void Inline_HoleHole(Vector *args, TagVal *idDefInstrOpt);
  void Inline_HoleFill(Vector *args, TagVal *idDefInstrOpt);
  void Inline_Equal(Vector *args, TagVal *idDefInstrOpt);
  //@}
  //@}


  /*! @name Code Generation
    Each method generates byte code for a specific Abstract Code instruction.
   */
  //@{

  //! Little helper to compile tuple generation.
  /*!
    Compilation of tuple generation.    
    @param dst Destination register where the initialized tuple is stored.
    @param idRefs Actual values of the tuple components.
  */
  void NewTup(u_int dst, Vector *idRefs);

  //! Little helper to compile tagged value selection
  /*!
    Compilation of tagged value selection. The method emits specialized byte code
    instructions whenever possible.
    @param testVal value that should be deconstructed
    @param idDefs destination identifiers
    @param isBig Indicates if \a testVal is a big tagged value or not.
   */
  void LoadTagVal(u_int testVal, Vector *idDefs, bool isBig);

  //! Determine if the specified TagTest's target branch is known statically.
  TagVal *StaticTagTestBranch(TagVal *pc, word idRef, bool isBigTag);
  
  //@{
  //! Compilation of one abstract code instruction
  /*!
    Each method generates code for the node \a pc and returns the successor of that node. 
    @param pc abstract code instruction that is to compile
    @return continuation; INVALID_POINTER signals a leaf node
   */
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
  //@}
  //@}

  void CompileCCC(Vector *rets, s_int outArity);
  void CompileInstr(TagVal *pc);
  void CompileApplyPrimitive(Closure *closure, Vector *args, bool isTailcall);
  void CompileSelfCall(TagVal *instr, bool isTailcall);
 
 // inlining
  void CompileInlineCCC(Vector *formalArgs, Vector *args, bool isReturn);
  TagVal *CompileInlineFunction(TagVal *appVar, AppVarInfo *avi, Vector *args, TagVal *idDefsInstrOpt);

public:
  //! constructor
  ByteCodeJitter();

  //! destructor
  ~ByteCodeJitter();

  //! initialization of all static variables
  static void Init();

  //! actual compilation method
  /*!
    Compilation is started with this method. The abstract code graph is
    wrapped into hotspot code that is converted to byte concrete code.

    @param[in,out] hsc code to compile
  */
  void Compile(HotSpotCode *hsc);
};

#endif
