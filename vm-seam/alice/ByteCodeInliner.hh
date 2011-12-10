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
  enum { INLINE_MAP_POS, LIVENESS_POS, ALIASES_POS, NNODES_POS, SIZE };
public:
  using Tuple::ToWord;

  static InlineInfo *New(Map *inlineMap, Vector *liveness, Vector *aliases, u_int nNodes) {
    Tuple *tup = Tuple::New(SIZE); 
    tup->Init(INLINE_MAP_POS, inlineMap->ToWord());
    tup->Init(LIVENESS_POS, liveness->ToWord());
    tup->Init(ALIASES_POS, aliases->ToWord());
    tup->Init(NNODES_POS, Store::IntToWord(nNodes));
    return static_cast<InlineInfo *>(tup);
  }
  
  /**
   * maps AppVar instr to AppVarInfo
   */
  Map *GetInlineMap() {
    return Map::FromWordDirect(Tuple::Sel(INLINE_MAP_POS)); 
  }
  
  Vector *GetLiveness() {
    return Vector::FromWordDirect(Tuple::Sel(LIVENESS_POS)); 
  }
  
  Vector *GetAliases() {
    return Vector::FromWordDirect(Tuple::Sel(ALIASES_POS));
  }
  
  u_int GetNLocals() { 
    return GetLiveness()->GetLength()/2;
  }
  
  u_int GetNNodes() { 
    return Store::DirectWordToInt(Tuple::Sel(NNODES_POS)); 
  }

  static InlineInfo *FromWord(word info) {
    return static_cast<InlineInfo *>(Tuple::FromWord(info));
  }
  
  static InlineInfo *FromWordDirect(word info) {
    return static_cast<InlineInfo *>(Tuple::FromWordDirect(info));
  }
};


/**
 * Information about an inlined AppVar instruction.
 */
class AppVarInfo : private Tuple {
private:
  enum { ABSTRACT_CODE_POS, SUBST_POS, LOCAL_OFFSET_POS, INLINE_INFO_POS, CLOSURE_POS, SIZE };
public:
  using Tuple::ToWord;
  
  static AppVarInfo *New(TagVal *abstractCode, Vector *subst, u_int localOffset, InlineInfo* inlineInfo, Closure* closure) {
    Tuple *tup = Tuple::New(SIZE);
    tup->Init(ABSTRACT_CODE_POS, abstractCode->ToWord());
    tup->Init(SUBST_POS, subst->ToWord());
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
  
  u_int GetLocalOffset(){
    return Store::DirectWordToInt(Sel(LOCAL_OFFSET_POS));
  }
  
  InlineInfo *GetInlineInfo() {
    return InlineInfo::FromWordDirect(Sel(INLINE_INFO_POS));
  }
  
  Closure *GetClosure(){
    return Closure::FromWordDirect(Sel(CLOSURE_POS));
  }

  static AppVarInfo *FromWord(word info) {
    return static_cast<AppVarInfo *>(Tuple::FromWord(info));
  }
  
  static AppVarInfo *FromWordDirect(word info) {
    return static_cast<AppVarInfo *>(Tuple::FromWordDirect(info));
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
