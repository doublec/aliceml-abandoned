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
#include "generic/Tuple.hh"
#include "alice/Data.hh"
#include "alice/NativeCodeInterpreter.hh"
#include "alice/LivenessInformation.hh"

class LazyCompileInterpreter : public Interpreter {
private:
  LazyCompileInterpreter(): Interpreter() {}
public:
  static LazyCompileInterpreter *self;
  static word concreteCode;

  static void Init();

  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Result Run(StackFrame *sFrame);
  virtual u_int GetInArity(ConcreteCode *concreteCode);
  virtual void PushCall(Closure *closure);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

class DllExport NativeConcreteCode : private ConcreteCode {
protected:
  enum {
    TRANSFORM_POS, NATIVE_CODE_POS, IMMEDIATE_ENV_POS, NLOCALS_POS,
    SKIP_CCC_PC_POS, SIZE
  };
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
  u_int GetSkipCCCPC() {
    return (u_int) Store::DirectWordToInt(Get(SKIP_CCC_PC_POS));
  }
  Transform *GetAbstractRepresentation() {
    return Transform::FromWordDirect(Get(TRANSFORM_POS));
  }
  void Disassemble(std::FILE *file);
  void UpdateCode(Chunk *chunk, word immediateEnv) {
    ConcreteCode::Init(NATIVE_CODE_POS, chunk->ToWord());
    ConcreteCode::Init(IMMEDIATE_ENV_POS, immediateEnv);
  }
  // NativeConcreteCode Constructor
  static word New(TagVal *abstractCode);
  static NativeConcreteCode *NewInternal(TagVal *abstractCode,
					 Chunk *code,
					 word immediateEnv,
					 word nbLocals,
					 word skipCCCPC);
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
