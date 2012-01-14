//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE__ABSTRACT_CODE_HH__
#define __ALICE__ABSTRACT_CODE_HH__

#if defined(interface)
#pragma interface "alice/AbstractCode.hh"
#endif

#include "alice/Data.hh"
#include "alice/Types.hh"


/**
 * Information about the uncurried form of a particular
 * function (with a single level of uncurrying - uncurry
 * the concrete code to get the next level).
 */
class UncurryInfo : private Tuple {
private:
  enum { CONCRETE_CODE_POS, CHILD_ARGS_OFFSET_POS, SIZE };
public:
  using Tuple::ToWord;
  
  static UncurryInfo *FromWord(word info) {
    return static_cast<UncurryInfo *>(Tuple::FromWord(info));
  }
  
  static UncurryInfo *FromWordDirect(word info) {
    return static_cast<UncurryInfo *>(Tuple::FromWordDirect(info));
  }
  
  static UncurryInfo *New(TagVal *abstractCode, u_int childArgsOffset);
  
  ConcreteCode *GetConcreteCode(){
    return ConcreteCode::FromWord(Sel(CONCRETE_CODE_POS));
  }
  
  /**
   * @return The index where the child functions argument's start in
   *         the idDef vector. Arguments before this index came from
   *         the parent function.
   */
  u_int GetChildArgsOffset(){
    return Store::DirectWordToInt(Sel(CHILD_ARGS_OFFSET_POS));
  }
  
  u_int GetInArity() {
    ConcreteCode *cc = GetConcreteCode();
    return cc->GetInterpreter()->GetInArity(cc);
  }
};


// Opcodes
class AliceDll AbstractCode {
private:
  static UncurryInfo *MakeUncurryInfo(TagVal *clsTpl, Vector *clsIdRefs, Vector *parentIdDefs, Vector *parentSubst);
public:
  static const u_int functionWidth = 7;

  //
  // All constructors here must be lexicographically ordered
  // so that the right integer tags are computed.
  //
  enum idDef {
    IdDef, Wildcard
  };

  enum con {
    Con, StaticCon
  };

  enum annotation {
    Debug, Simple
  };

  enum args {
    OneArg, TupArgs
  };

  enum instr {
    AppPrim, AppVar, Close, CompactIntTest, CompactTagTest, ConTest,
    Coord, EndHandle, EndTry, Entry, Exit, GetRef, GetTup, IntTest,
    Kill, LazyPolySel, PutCon, PutNew, PutPolyRec, PutRef, PutTag,
    PutTup, PutVar, PutVec, Raise, RealTest, Reraise, Return, Sel,
    Shared, Specialize, StringTest, TagTest, Try, VecTest
  };
  static const u_int nInstrs = VecTest + 1;

  static const char *GetOpcodeName(instr opcode);
  static const char *GetOpcodeName(TagVal *pc);

  enum idRef {
    Global, Immediate, LastUseLocal, Local
  };

  enum abstractCode {
    Function
  };

  enum entryPoint {
    AppEntry, ConEntry, CondEntry, HandleEntry, RaiseEntry, SelEntry,
    SpawnEntry, StrictEntry
  };

  enum exitPoint {
    AppExit, ConExit, CondExit, HandleExit, RaiseExit, SelExit,
    SpawnExit, StrictExit
  };

  static con GetCon(TagVal *tagVal) {
    return static_cast<con>(tagVal->GetTag());
  }
  static args GetArgs(TagVal *tagVal) {
    return static_cast<args>(tagVal->GetTag());
  }
  static instr GetInstr(TagVal *tagVal) {
    return static_cast<instr>(tagVal->GetTag());
  }
  static idRef GetIdRef(TagVal *tagVal) {
    return static_cast<idRef>(tagVal->GetTag());
  }
  static abstractCode GetAbstractCode(TagVal *tagVal) {
    return static_cast<abstractCode>(tagVal->GetTag());
  }
  static entryPoint GetEntryPoint(TagVal *tagVal) {
    return static_cast<entryPoint>(tagVal->GetTag());
  }
  static exitPoint GetExitPoint(TagVal *tagVal) {
    return static_cast<exitPoint>(tagVal->GetTag());
  }
  static annotation GetAnnotation(TagVal *tagVal) {
    return static_cast<annotation>(tagVal->GetTag());
  }

  static u_int GetNumberOfLocals(TagVal *abstractCode) {
    TagVal *annotation = TagVal::FromWordDirect(abstractCode->Sel(2));
    switch (AbstractCode::GetAnnotation(annotation)) {
      case AbstractCode::Simple:
	return Store::DirectWordToInt(annotation->Sel(0));
      case AbstractCode::Debug:
	return Vector::FromWordDirect(annotation->Sel(0))->GetLength();
    }
  }
  
  static u_int GetNumberOfGlobals(Vector *idRefs) {
    u_int n = 0;
    for (u_int i=0; i<idRefs->GetLength(); i++){
      TagVal *idRef = TagVal::FromWordDirect(idRefs->Sub(i));
      if (GetIdRef(idRef) == Global) {
	n++;
      }
    }
    return n;
  }
  
  static bool IsId(TagVal *idRef, u_int id) {
    AbstractCode::idRef tag = GetIdRef(idRef);
    return (tag == Local || tag == LastUseLocal) && Store::DirectWordToInt(idRef->Sel(0)) == id;
  }
  
  static bool IsLocal(TagVal *idRef, u_int id) {
    return GetIdRef(idRef) == Local && Store::DirectWordToInt(idRef->Sel(0)) == id;
  }
  
  static bool IsLastUseLocal(TagVal *idRef, u_int id) {
    return GetIdRef(idRef) == LastUseLocal && Store::DirectWordToInt(idRef->Sel(0)) == id;
  }
  
  static bool IdRefsContain(Vector *idRefs, u_int id) {
    for (u_int i=0; i<idRefs->GetLength(); i++) {
      if (IsId(TagVal::FromWordDirect(idRefs->Sub(i)), id)) {
	return true;
      }
    }
    return false;
  }
  
  static Vector *ShiftIdDefs(Vector *idDefs, s_int offset) {
    u_int size = idDefs->GetLength();
    Vector *dsts = Vector::New(size);
    for(u_int i = size; i--; ) {
      TagVal *argOpt = TagVal::FromWord(idDefs->Sub(i));
      if(argOpt != INVALID_POINTER) {
	u_int id = Store::DirectWordToInt(argOpt->Sel(0)) + offset;
	TagVal *newOpt = TagVal::New1(Types::SOME, Store::IntToWord(id));
	dsts->Init(i, newOpt->ToWord());
      } else {
	dsts->Init(i, idDefs->Sub(i));
      }
    }
    return dsts;
  }

  static bool AllIdDefsWildcards(Vector *idDefs) {
    for (u_int i=idDefs->GetLength(); i--; ) {
      if (TagVal::FromWord(idDefs->Sub(i)) != INVALID_POINTER) {
	return false;
      }
    }
    return true;
  }
  
  static TagVal *SkipDebugInstrs(TagVal *instr);
  
  /**
   * If instr has a single continuation instr, return its index
   * in the TagVal, otherwise return -1.
   */
  static s_int GetContinuationPos(instr instr);
  
  /**
   * Return how many program points (i.e. points where different
   * sets of local variables can be alive) the specified kind of
   * instr has, or -1 if the count is dependent on the
   * instruction operands.
   * 
   * See /compiler/backend-seam/MkLiveness.aml for related info.
   */
  static s_int GetNumProgramPoints(instr instr);
  
  /**
   * @return The maximum program point in the specified abstractCode
   */
  static u_int GetMaxProgramPoint(TagVal *abstractCode);
  
  /**
   * @return An UncurryInfo if this abstractCode can be uncurried
   *         or INVALID_POINTER if not.
   */
  static UncurryInfo *GetUncurryInfo(TagVal *abstractCode);
  
  /**
  * Collect information about the in-arity of shared nodes by depth
  * first iteration. This is needed for (for e.g.) constant propagation.
  * 
  * @return A map from shared node stamp to number of edges leading to it.
  */
  static IntMap *SharedInArity(TagVal *abstractCode);
  
  static void Disassemble(std::FILE *f, TagVal *pc);
};

#endif
