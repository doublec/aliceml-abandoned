//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __EMULATOR_PICKLE_HH__
#define __EMULATOR_PICKLE_HH__

// Opcodes
class Pickle {
public:
  //
  // All constructors here must be lexicographically ordered
  // so that the right integer tags are computed.
  //
  enum idDef {
    IdDef, Wildcard
  };

  enum idRef {
    Global, Local // to be done
  };

  enum con {
    Con, StaticCon
  };

  enum args {
    OneArg, TupArgs
  };

  enum instr {
    AppConst, AppPrim, AppVar, ConTest, EndHandle, EndTry, GetRef, GetTup,
    IntTest, Kill, LazySel, PutCon, PutConst, PutFun, PutNew, PutRef,
    PutTag, PutTup, PutVar, PutVec, Raise, RealTest, Reraise,
    Return, Sel, Shared, StringTest, TagTest, Try, VecTest, WideStringTest
  };

  enum function {
    Function
  };

  static idRef GetIdRef(TagVal *tagVal) {
    return static_cast<idRef>(tagVal->GetTag());
  }
  static con GetCon(TagVal *tagVal) {
    return static_cast<con>(tagVal->GetTag());
  }
  static args GetArgs(TagVal *tagVal) {
    return static_cast<args>(tagVal->GetTag());
  }
  static instr GetInstr(TagVal *tagVal) {
    return static_cast<instr>(tagVal->GetTag());
  }
};

#endif
