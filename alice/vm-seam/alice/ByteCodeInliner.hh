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

#ifndef __ALICE_BYTECODE_INLINER_HH__
#define __ALICE_BYTECODE_INLINER_HH__

#if defined(INTERFACE)
#pragma interface "alice/ByteCodeInliner.hh"
#endif

#include "alice/Authoring.hh"
#include "alice/AbstractCode.hh"


/**
 * Inlining information for a function.
 */
class InlineInfo : private Tuple {
private:
  enum { INLINE_MAP_POS, OMITTED_APP_VARS_POS, UNCURRIED_APP_VARS_POS, LIVENESS_POS, ALIASES_POS, NNODES_POS, SIZE };
public:
  using Tuple::ToWord;

  static InlineInfo *FromWord(word info) {
    return static_cast<InlineInfo *>(Tuple::FromWord(info));
  }
  
  static InlineInfo *FromWordDirect(word info) {
    return static_cast<InlineInfo *>(Tuple::FromWordDirect(info));
  }
  
  static InlineInfo *New(Map *inlineMap, Map *omittedAppVars, Map *uncurriedAppVars, Vector *liveness, Vector *aliases, u_int nNodes) {
    Tuple *tup = Tuple::New(SIZE); 
    tup->Init(INLINE_MAP_POS, inlineMap->ToWord());
    tup->Init(OMITTED_APP_VARS_POS, omittedAppVars->ToWord());
    tup->Init(UNCURRIED_APP_VARS_POS, uncurriedAppVars->ToWord());
    tup->Init(LIVENESS_POS, liveness->ToWord());
    tup->Init(ALIASES_POS, aliases->ToWord());
    tup->Init(NNODES_POS, Store::IntToWord(nNodes));
    return static_cast<InlineInfo *>(tup);
  }
  
  /**
   * maps AppVar instr to AppVarInfo
   */
  Map *GetInlineMap() {
    return Map::FromWordDirect(Sel(INLINE_MAP_POS)); 
  }
  
  /**
   * maps AppVar instr to continuation instr
   */
  Map *GetOmittedAppVars() {
    return Map::FromWordDirect(Sel(OMITTED_APP_VARS_POS));
  }
  
  /**
   * maps AppVar instr to UncurriedAppVarInfo
   */
  Map *GetUncurriedAppVars() {
    return Map::FromWordDirect(Sel(UNCURRIED_APP_VARS_POS));
  }
  
  Vector *GetLiveness() {
    return Vector::FromWordDirect(Sel(LIVENESS_POS)); 
  }
  
  Vector *GetAliases() {
    return Vector::FromWordDirect(Sel(ALIASES_POS));
  }
  
  u_int GetNLocals() { 
    return GetLiveness()->GetLength()/2;
  }
  
  u_int GetNNodes() {
    return Store::DirectWordToInt(Sel(NNODES_POS)); 
  }

  /**
   * Recursively count how many inlined AppVars there are
   * inside this function
   */
  u_int NumInlinedAppVars();
  
};


/**
 * Information about an inlined AppVar instruction.
 */
class AppVarInfo : private Tuple {
private:
  enum { ABSTRACT_CODE_POS, SUBST_POS, ARGS_POS, LOCAL_OFFSET_POS, INLINE_INFO_POS, CLOSURE_POS, SIZE };
public:
  using Tuple::ToWord;

  static AppVarInfo *FromWord(word info) {
    return static_cast<AppVarInfo *>(Tuple::FromWord(info));
  }
  
  static AppVarInfo *FromWordDirect(word info) {
    return static_cast<AppVarInfo *>(Tuple::FromWordDirect(info));
  }
  
  static AppVarInfo *New(TagVal *abstractCode, Vector *subst, Vector *args, u_int localOffset, InlineInfo* inlineInfo, Closure* closure) {
    Tuple *tup = Tuple::New(SIZE);
    tup->Init(ABSTRACT_CODE_POS, abstractCode->ToWord());
    tup->Init(SUBST_POS, subst->ToWord());
    tup->Init(ARGS_POS, args->ToWord());
    tup->Init(LOCAL_OFFSET_POS, Store::IntToWord(localOffset));
    tup->Init(INLINE_INFO_POS, inlineInfo->ToWord());
    tup->Init(CLOSURE_POS, closure->ToWord());
    return static_cast<AppVarInfo*>(tup);
  }
  
  TagVal *GetAbstractCode(){
    return TagVal::FromWordDirect(Sel(ABSTRACT_CODE_POS));
  }
  
  Vector *GetSubst(){
    return Vector::FromWordDirect(Sel(SUBST_POS));
  }
  
  Vector *GetArgs(){
    return Vector::FromWordDirect(Sel(ARGS_POS));
  }
  
  u_int GetLocalOffset(){
    return Store::DirectWordToInt(Sel(LOCAL_OFFSET_POS));
  }
  
  InlineInfo *GetInlineInfo() {
    return InlineInfo::FromWordDirect(Sel(INLINE_INFO_POS));
  }
  
  Closure *GetClosure(){
    return Closure::FromWordDirect(Sel(CLOSURE_POS));
  }
};


class UncurriedAppVarInfo : private Tuple {
private:
  enum { CLOSURE_POS, ARG_IDREFS_POS, ARG_CCC_SIZES_POS, SIZE };
public:
  using Tuple::ToWord;
  
  static UncurriedAppVarInfo *FromWord(word info) {
    return static_cast<UncurriedAppVarInfo *>(Tuple::FromWord(info));
  }
  
  static UncurriedAppVarInfo *FromWordDirect(word info) {
    return static_cast<UncurriedAppVarInfo *>(Tuple::FromWordDirect(info));
  }
  
  static UncurriedAppVarInfo *New(Closure *closure, Vector *argIdRefs, Vector *argCCCSizes) {
    Tuple *t = Tuple::New(SIZE);
    t->Init(CLOSURE_POS, closure->ToWord());
    t->Init(ARG_IDREFS_POS, argIdRefs->ToWord());
    t->Init(ARG_CCC_SIZES_POS, argCCCSizes->ToWord());
    return reinterpret_cast<UncurriedAppVarInfo*>(t);
  }
  
  Closure *GetClosure(){
    return Closure::FromWordDirect(Sel(CLOSURE_POS));
  }
  
  /**
   * An idRef vector vector
   */
  Vector *GetArgIdRefs() {
    return Vector::FromWordDirect(Sel(ARG_IDREFS_POS));
  }
  
  /**
   * Each element in here corresponds to an idRef vector at the
   * same index within ArgsIdRefs, and says how many argument
   * values the idRefs needs to be CCC'd to.
   */
  Vector *GetArgCCCSizes() {
    return Vector::FromWordDirect(Sel(ARG_CCC_SIZES_POS));
  }
  
  /**
   * The CCC is considered trivial if the number of idRefs given for each
   * stage of the application matches the number of idDefs for that stage.
   */
  bool TrivialCCC() {
    Vector *argsIdRefs = GetArgIdRefs();
    Vector *argsCCCSizes = GetArgCCCSizes();
    Assert(argsIdRefs->GetLength() == argsCCCSizes->GetLength());
    for (u_int i=0; i<argsIdRefs->GetLength(); i++) {
      Vector *idRefs = Vector::FromWordDirect(argsIdRefs->Sub(i));
      u_int expSize = Store::DirectWordToInt(argsCCCSizes->Sub(i));
      if (idRefs->GetLength() != expSize) {
	return false;
      }
    }
    return true;
  }
  
  u_int GetInArity(){
    ConcreteCode *cc = ConcreteCode::FromWordDirect(GetClosure()->GetConcreteCode());
    return cc->GetInterpreter()->GetInArity(cc);
  }
  
  u_int GetOutArity() {
    ConcreteCode *cc = ConcreteCode::FromWordDirect(GetClosure()->GetConcreteCode());
    return cc->GetInterpreter()->GetOutArity(cc);
  }
};


class ByteCodeInliner {
public:

  // Checks if a function is inlinable
  // At the moment we use a very simple heuristic to decide whether we 
  // inline a function or not:
  // We impose a very ad-hoc size level, which is based on the number
  // of nodes inside an abstract code function. More profiling should
  // make this size barrier less ad-hoc.
  static InlineInfo *Analyse(TagVal *abstractCode);
};

#endif // __ALICE_BYTECODE_INLINER_HH__
