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
    AppPrim, AppVar, ConTest, EndHandle, EndTry, GetTup, IntTest, Kill,
    PutCon, PutConst, PutFun, PutGlobal, PutNew, PutRef, PutSel, PutTag,
    PutTup, PutVec, RealTest, Return, StringTest, TagTest, Try, VecTest
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
};

#endif
