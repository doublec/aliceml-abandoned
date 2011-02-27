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
    EndHandle, EndTry, Entry, Exit, GetRef, GetTup, IntTest, Kill,
    LazyPolySel, PutCon, PutNew, PutPolyRec, PutRef, PutTag, PutTup,
    PutVar, PutVec, Raise, RealTest, Reraise, Return, Sel, Shared,
    Specialize, StringTest, TagTest, Try, VecTest
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

  static void Disassemble(std::FILE *f, TagVal *pc);
};

#endif
