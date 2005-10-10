//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Christian Mueller <cmueller@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
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

class LazyByteCompileClosure : public Closure {
protected:
  enum { ABSTRACT_CODE, BYNEED_POS, N_LOCALS_POS, INLINE_INFO_POS, SIZE };
public:
  TagVal *GetAbstractCode() {
    return TagVal::FromWordDirect(Sub(ABSTRACT_CODE));
  }
  word GetByneed() {
    return Sub(BYNEED_POS);
  }
  s_int GetNLocals() {
    return Store::DirectWordToInt(Sub(N_LOCALS_POS));
  }
  void SetNLocals(s_int nLocals) {
    Update(N_LOCALS_POS, nLocals);
  }
  void SetInlineInfo(InlineInfo *info) {
    TagVal *some = TagVal::New(Types::SOME,1);
    some->Init(0,info->ToWord());
    Update(INLINE_INFO_POS,some->ToWord());
  }
  TagVal *GetInlineInfoOpt() {
    return TagVal::FromWord(Sub(INLINE_INFO_POS));
  }
  static LazyByteCompileClosure *New(TagVal *abstractCode);
  static LazyByteCompileClosure *FromWordDirect(word wClosure) {
    return STATIC_CAST(LazyByteCompileClosure *, 
		       Closure::FromWordDirect(wClosure));
  }
};

class LazyByteCompileInterpreter : public Interpreter {
private:
  LazyByteCompileInterpreter(): Interpreter() {}
public:
  static LazyByteCompileInterpreter *self;
  static word concreteCode;

  static void Init();

  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Result Run(StackFrame *sFrame);
  virtual u_int GetInArity(ConcreteCode *concreteCode);
  virtual u_int GetOutArity(ConcreteCode *concreteCode);
  virtual void PushCall(Closure *closure);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

class AliceDll ByteConcreteCode : private ConcreteCode {
protected:
    enum {
    TRANSFORM_POS, BYTE_CODE_POS, IMMEDIATE_ENV_POS, 
    IN_ARITY_POS, OUT_ARITY_POS, NLOCALS_POS, 
    INLINE_INFO_POS,
    SIZE
  };
public:
  using Block::ToWord;
  using ConcreteCode::GetInterpreter;
 
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
