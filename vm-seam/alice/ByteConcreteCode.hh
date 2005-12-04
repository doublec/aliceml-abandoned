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

#ifndef __ALICE_BYTE_CONCRETE_CODE_HH__
#define __ALICE_BYTE_CONCRETE_CODE_HH__

#if defined(INTERFACE)
#pragma interface "alice/ByteConcreteCode.hh"
#endif

#include "alice/Data.hh"
#include "alice/ByteCodeInterpreter.hh"
#include "alice/ByteCodeInliner.hh"

class HotSpotCode;

class AliceDll ByteConcreteCode : private ConcreteCode {
protected:
  enum {
    // ATTENTION:
    // Assure that the first three fields have the same layout as the fields
    // from HotSpotCode. Otherwise the Transform methode will not work.
    TRANSFORM_POS,
    INLINE_INFO_POS,  
    BYTE_CODE_POS, 
    IMMEDIATE_ENV_POS, IN_ARITY_POS, OUT_ARITY_POS, NLOCALS_POS, 
    SIZE_INTERNAL
  };
public:
  using Block::ToWord;
  using ConcreteCode::GetInterpreter;

  // the size is needed in HotSpotConcreteCode
  enum { SIZE = SIZE_INTERNAL };
 
  // ByteConcreteCode Accessors
  Chunk *GetByteCode() {
    return Store::DirectWordToChunk(Get(BYTE_CODE_POS));
  }
  Tuple *GetImmediateArgs() {
    return Tuple::FromWordDirect(Get(IMMEDIATE_ENV_POS));
  }
  u_int GetNLocals() {
    return (u_int) Store::DirectWordToInt(Get(NLOCALS_POS));
  } 
  Transform *GetAbstractRepresentation() {
    return Transform::FromWordDirect(Get(TRANSFORM_POS));
  }
  u_int GetInArity() {
    return (u_int) Store::DirectWordToInt(Get(IN_ARITY_POS));
  }
  s_int GetOutArity() {
    return Store::DirectWordToInt(Get(OUT_ARITY_POS));
  }
  InlineInfo *GetInlineInfo() {
    return InlineInfo::FromWordDirect(Get(INLINE_INFO_POS));
  }
  void Disassemble(std::FILE *file);
  void UpdateCode(Chunk *chunk, word immediateEnv) {
    ConcreteCode::Init(BYTE_CODE_POS, chunk->ToWord());
    ConcreteCode::Init(IMMEDIATE_ENV_POS, immediateEnv);
  }
  // ByteConcreteCode Constructor
  static word New(TagVal *abstractCode);
  static ByteConcreteCode *NewInternal(TagVal *abstractCode,
				       Chunk *code,
				       word immediateEnv,
				       word nbLocals,
				       word inlineInfo);
  static void Convert(HotSpotCode *hsc,
		      Chunk *code,
		      word immediateEnv,
		      word nbLocals,
		      word inlineInfo);			
  // ByteConcreteCode Untagging
  static ByteConcreteCode *FromWord(word code) {
    ConcreteCode *concreteCode = ConcreteCode::FromWord(code);
    Assert(concreteCode == INVALID_POINTER ||
	   concreteCode->GetInterpreter() == ByteCodeInterpreter::self);
    return STATIC_CAST(ByteConcreteCode *, concreteCode);
  }
  static ByteConcreteCode *FromWordDirect(word code) {
    ConcreteCode *concreteCode = ConcreteCode::FromWordDirect(code);
    Assert(concreteCode->GetInterpreter() == ByteCodeInterpreter::self);
    return STATIC_CAST(ByteConcreteCode *, concreteCode);
  }
};

#endif
