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

#ifndef __PICKLE_HH__
#define __PICKLE_HH__

//
// Opcodes
//

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
    Local, Global
  };

  enum con {
    Con, StaticCon
  };

  enum args {
    OneArg, TupArgs
  };

  enum instr {
    AppConst, AppPrim, AppVar, ConTest, EndHandle, EndTry, GetRef, GetTup,
    IntTest, Kill, PutCon, PutConst, PutFun, PutNew, PutRef, PutTag, PutTup,
    PutVar, PutVec, Raise, RealTest, Return, StringTest, TagTest, Try, VecTest
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
