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

#include <cstdio>
#include "alice/Data.hh"

// Opcodes
class AbstractCode {
public:
  static const u_int functionWidth = 6;

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

  enum instr {
    AppPrim, AppVar, Close, CompactIntTest, CompactTagTest, ConTest,
    DirectAppVar, EndHandle, EndTry, GetRef, GetTup, IntTest, Kill,
    LazyPolySel, PutCon, PutNew, PutRef, PutTag, PutTup, PutPolyRec,
    PutVar, PutVec, Raise, RealTest, Reraise, Return, Sel, Shared,
    Specialize, StringTest, TagTest, Try, VecTest
  };
  static const u_int nInstrs = VecTest + 1;

  enum idRef {
    Global, Immediate, LastUseLocal, Local, Toplevel
  };

  enum abstractCode {
    Function, Specialized
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

  static void Disassemble(std::FILE *f, TagVal *pc);
};

#endif
