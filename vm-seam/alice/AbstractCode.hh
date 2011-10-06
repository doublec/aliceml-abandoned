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

// Opcodes
class AliceDll AbstractCode {
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
  * Collect information about the in-arity of shared nodes by depth
  * first iteration. This is needed for (for e.g.) constant propagation.
  * 
  * @return A map from shared node stamp to number of edges leading to it.
  */
  static IntMap *SharedInArity(TagVal *abstractCode);
  
  static void Disassemble(std::FILE *f, TagVal *pc);
};

#endif
