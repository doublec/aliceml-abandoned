//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE__NATIVE_CONCRETE_CODE_HH__
#define __ALICE__NATIVE_CONCRETE_CODE_HH__

#if defined(INTERFACE)
#pragma interface "alice/NativeConcreteCode.hh"
#endif

#include "generic/ConcreteCode.hh"
#include "alice/Data.hh"
#include "alice/NativeCodeInterpreter.hh"

class NativeConcreteCode : private ConcreteCode {
protected:
  static const u_int TRANSFORM_POS     = 0;
  static const u_int NATIVE_CODE_POS   = 1;
  static const u_int IMMEDIATE_ENV_POS = 2;
  static const u_int NLOCALS_POS       = 3;
  static const u_int SIZE              = 4;
public:
  using Block::ToWord;
  using ConcreteCode::GetInterpreter;
  // NativeConcreteCode Accessors
  Chunk *GetNativeCode() {
    return Store::DirectWordToChunk(Get(NATIVE_CODE_POS));
  }
  Tuple *GetImmediateArgs() {
    return Tuple::FromWordDirect(Get(IMMEDIATE_ENV_POS));
  }
  u_int GetNLocals() {
    return (u_int) Store::DirectWordToInt(Get(NLOCALS_POS));
  }
  Block *GetAbstractRepresentation() {
    return Store::DirectWordToBlock(Get(TRANSFORM_POS));
  }
  void UpdateCode(Chunk *chunk, word immediateEnv) {
    Init(NATIVE_CODE_POS, chunk->ToWord());
    Init(IMMEDIATE_ENV_POS, immediateEnv);
  }
  // NativeConcreteCode Constructor
  static NativeConcreteCode *New(TagVal *abstractCode,
				 Chunk *code,
				 word immediateEnv,
				 word nbLocals);
  // NativeConcreteCode Untagging
  static NativeConcreteCode *FromWord(word code) {
    ConcreteCode *concreteCode = ConcreteCode::FromWord(code);
    Assert(concreteCode == INVALID_POINTER ||
	   concreteCode->GetInterpreter() == NativeCodeInterpreter::self);
    return (NativeConcreteCode *) concreteCode;
  }
  static NativeConcreteCode *FromWordDirect(word code) {
    ConcreteCode *concreteCode = ConcreteCode::FromWordDirect(code);
    Assert(concreteCode->GetInterpreter() == NativeCodeInterpreter::self);
    return (NativeConcreteCode *) concreteCode;
  }
};

#endif
