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
class AbstractCode {
public:
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

  enum args {
    OneArg, TupArgs
  };

  enum function {
    Function
  };

  enum instr {
    AppPrim, AppVar, CompactIntTest, CompactTagTest, ConTest, DirectAppVar,
    EndHandle, EndTry, GetRef, GetTup, IntTest, Kill, LazySel, PutCon, PutFun,
    PutNew, PutRef, PutTag, PutTup, PutVar, PutVec, Raise, RealTest, Reraise,
    Return, Sel, Shared, StringTest, TagTest, Try, VecTest
  };

  enum idRef {
    Global, Immediate, Local
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
};

#endif
