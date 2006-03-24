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

#include "alice/Data.hh"
#include "alice/NativeCodeInterpreter.hh"

#if HAVE_LIGHTNING

class LazyCompileClosure : public Closure {
protected:
  enum { ABSTRACT_CODE, BYNEED_POS, N_LOCALS_POS, ASSIGNMENT_POS, SIZE };
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
  Tuple *GetAssignment() {
    return Tuple::FromWordDirect(Sub(ASSIGNMENT_POS));
  }
  void SetAssignment(Tuple *assignment) {
    Update(ASSIGNMENT_POS, assignment->ToWord());
  }

  static LazyCompileClosure *New(TagVal *abstractCode);
  static LazyCompileClosure *FromWordDirect(word wClosure) {
    return STATIC_CAST(LazyCompileClosure *, Closure::FromWordDirect(wClosure));
  }
};

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
  virtual u_int GetOutArity(ConcreteCode *concreteCode);
  virtual void PushCall(Closure *closure);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

class AliceDll NativeConcreteCode : private ConcreteCode {
protected:
  enum {
    TRANSFORM_POS, NATIVE_CODE_POS, IMMEDIATE_ENV_POS, NLOCALS_POS,
    CCC_PC_POS, SKIP_CCC_PC_POS, SIZE
  };
public:
  using ConcreteCode::ToWord;
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
  u_int GetCCCPC() {
    return (u_int) Store::DirectWordToInt(Get(CCC_PC_POS));
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
					 word CCCPC,
					 word skipCCCPC);
  // NativeConcreteCode Untagging
  static NativeConcreteCode *FromWord(word code) {
    ConcreteCode *concreteCode = ConcreteCode::FromWord(code);
    Assert(concreteCode == INVALID_POINTER ||
	   concreteCode->GetInterpreter() == NativeCodeInterpreter::self);
    return STATIC_CAST(NativeConcreteCode *, concreteCode);
  }
  static NativeConcreteCode *FromWordDirect(word code) {
    ConcreteCode *concreteCode = ConcreteCode::FromWordDirect(code);
    Assert(concreteCode->GetInterpreter() == NativeCodeInterpreter::self);
    return STATIC_CAST(NativeConcreteCode *, concreteCode);
  }
};

#endif

#endif
