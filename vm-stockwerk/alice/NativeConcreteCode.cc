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

#if defined(INTERFACE)
#pragma implementation "alice/NativeConcreteCode.hh"
#endif

#include "generic/StackFrame.hh"
#include "generic/TaskStack.hh"
#include "generic/Closure.hh"
#include "generic/Transform.hh"
#include "generic/RootSet.hh"
#include "generic/Interpreter.hh"
#include "generic/Transients.hh"
#include "alice/NativeCodeJitter.hh"
#include "alice/AliceConcreteCode.hh"
#include "alice/AliceLanguageLayer.hh"

// LazyCompile Frame
class LazyCompileFrame : private StackFrame {
private:
  static const u_int ABSTRACT_CODE_POS = 0;
  static const u_int SIZE              = 1;
public:
  using Block::ToWord;
  using StackFrame::GetInterpreter;
  // LazyCompileFrame Accessors
  TagVal *GetAbstractCode() {
    return TagVal::FromWordDirect(StackFrame::GetArg(ABSTRACT_CODE_POS));
  }
  // LazyCompileFrame Constructor
  static LazyCompileFrame *New(Interpreter *interpreter, TagVal *abstractCode) {
    StackFrame *frame =
      StackFrame::New(LAZY_COMPILE_FRAME,interpreter, SIZE);
    frame->InitArg(ABSTRACT_CODE_POS, abstractCode->ToWord());
    return static_cast<LazyCompileFrame *>(frame);
  }
  // LazyCompileFrame Untagging
  static LazyCompileFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == LAZY_COMPILE_FRAME);
    return static_cast<LazyCompileFrame *>(p);
  }
};

//
// LazyCompileInterpreter
//
LazyCompileInterpreter *LazyCompileInterpreter::self;

void
LazyCompileInterpreter::PushFrame(TaskStack *taskStack, TagVal *abstractCode) {
  taskStack->PushFrame(LazyCompileFrame::New(self, abstractCode)->ToWord());
}

void LazyCompileInterpreter::PushCall(TaskStack *taskStack, Closure *closure) {
  PushFrame(taskStack, TagVal::FromWordDirect(closure->Sub(0)));
}

Interpreter::Result LazyCompileInterpreter::Run(TaskStack *taskStack) {
  LazyCompileFrame *frame =
    LazyCompileFrame::FromWordDirect(taskStack->GetFrame());
  TagVal *abstractCode = frame->GetAbstractCode();
  taskStack->PopFrame(); // Discard Frame
  Scheduler::nArgs          = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = NativeCodeJitter::Compile(abstractCode)->ToWord();
  return Interpreter::CONTINUE;
}

const char *LazyCompileInterpreter::Identify() {
  return "LazyCompileInterpreter";
}

void LazyCompileInterpreter::DumpFrame(word) {
  fprintf(stderr, "LazyCompile");
}

//
// LazyCompileClosure
//
class LazyCompileClosure : public Closure {
public:
  static LazyCompileClosure *New(TagVal *abstractCode);
};

LazyCompileClosure *LazyCompileClosure::New(TagVal *abstractCode) {
  ConcreteCode *concreteCode =
    ConcreteCode::New(LazyCompileInterpreter::self, 0);
  Closure *closure = Closure::New(concreteCode->ToWord(), 1);
  closure->Init(0, abstractCode->ToWord());
  return static_cast<LazyCompileClosure *>(closure);
}

//
// NativeConcreteCode
//
NativeConcreteCode *NativeConcreteCode::NewInternal(TagVal *abstractCode,
						    Chunk *code,
						    word immediateEnv,
#if defined(ALICE_IMPLICIT_KILL)
						    word livenessInfo,
#endif
						    word nbLocals) {
  ConcreteCode *concreteCode =
    (ConcreteCode *) ConcreteCode::New(NativeCodeInterpreter::self, SIZE);
  Chunk *name =
    Store::DirectWordToChunk(AliceLanguageLayer::TransformNames::function);
  Transform *transform = Transform::New(name, abstractCode->ToWord());
  concreteCode->Init(TRANSFORM_POS, transform->ToWord());
  concreteCode->Init(NATIVE_CODE_POS, code->ToWord());
  concreteCode->Init(IMMEDIATE_ENV_POS, immediateEnv);
  concreteCode->Init(NLOCALS_POS, nbLocals);
#if defined(ALICE_IMPLICIT_KILL)
  concreteCode->Init(LIVENESS_INFO_POS, livenessInfo);
#endif
  return (NativeConcreteCode *) concreteCode;
}

word NativeConcreteCode::New(TagVal *abstractCode) {
  Closure *closure = LazyCompileClosure::New(abstractCode);
  return Byneed::New(closure->ToWord())->ToWord();
}
