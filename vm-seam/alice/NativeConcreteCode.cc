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

#if LIGHTNING

#if defined(INTERFACE)
#pragma implementation "alice/NativeConcreteCode.hh"
#endif

#include "alice/AbstractCode.hh"
#include "alice/NativeCodeJitter.hh"
#include "alice/AliceLanguageLayer.hh"

// LazyCompile Frame
class LazyCompileFrame : private StackFrame {
private:
  enum { CLOSURE_POS, SIZE };
public:
  // LazyCompileFrame Accessors
  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  Closure *GetClosure() {
    return Closure::FromWordDirect(StackFrame::GetArg(CLOSURE_POS));
  }
  // LazyCompileFrame Constructor
  static LazyCompileFrame *New(Interpreter *interpreter,
			       Closure *closure) {
    NEW_STACK_FRAME(frame, interpreter, SIZE);
    frame->InitArg(CLOSURE_POS, closure->ToWord());
    return static_cast<LazyCompileFrame *>(frame);
  }
};

//
// LazyCompileInterpreter
//
LazyCompileInterpreter *LazyCompileInterpreter::self;
word LazyCompileInterpreter::concreteCode;

void LazyCompileInterpreter::Init() {
  self = new LazyCompileInterpreter();
  concreteCode = ConcreteCode::New(self, 0)->ToWord();
  RootSet::Add(concreteCode);
}

void LazyCompileInterpreter::PushCall(Closure *closure) {
  LazyCompileFrame::New(self, closure);
}

u_int LazyCompileInterpreter::GetFrameSize(StackFrame *sFrame) {
  LazyCompileFrame *frame = static_cast<LazyCompileFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result LazyCompileInterpreter::Run(StackFrame *sFrame) {
  LazyCompileFrame *frame = static_cast<LazyCompileFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  Closure *closure = frame->GetClosure();
  Scheduler::PopFrame(frame->GetSize());
  TagVal *abstractCode = TagVal::FromWordDirect(closure->Sub(0));
  NativeCodeJitter::currentConcreteCode = closure->Sub(1);
  Scheduler::nArgs          = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = NativeCodeJitter::Compile(abstractCode)->ToWord();
  return Worker::CONTINUE;
}

u_int LazyCompileInterpreter::GetInArity(ConcreteCode *) {
  return 0;
}

const char *LazyCompileInterpreter::Identify() {
  return "LazyCompileInterpreter";
}

void LazyCompileInterpreter::DumpFrame(StackFrame *) {
  std::fprintf(stderr, "LazyCompile");
}

//
// LazyCompileClosure
//
class LazyCompileClosure : public Closure {
public:
  static LazyCompileClosure *New(TagVal *abstractCode);
};

LazyCompileClosure *LazyCompileClosure::New(TagVal *abstractCode) {
  Closure *closure = Closure::New(LazyCompileInterpreter::concreteCode, 2);
  closure->Init(0, abstractCode->ToWord());
  // closure->Init(1, byneed) done in NativeConcreteCode::New
  return static_cast<LazyCompileClosure *>(closure);
}

//
// NativeConcreteCode
//
NativeConcreteCode *NativeConcreteCode::NewInternal(TagVal *abstractCode,
						    Chunk *code,
						    word immediateEnv,
						    word nbLocals,
						    word skipCCCPC) {
  ConcreteCode *concreteCode =
    ConcreteCode::New(NativeCodeInterpreter::self, SIZE);
  Chunk *name =
    Store::DirectWordToChunk(AliceLanguageLayer::TransformNames::function);
  Transform *transform = Transform::New(name, abstractCode->ToWord());
  concreteCode->Init(TRANSFORM_POS, transform->ToWord());
  concreteCode->Init(NATIVE_CODE_POS, code->ToWord());
  concreteCode->Init(IMMEDIATE_ENV_POS, immediateEnv);
  concreteCode->Init(NLOCALS_POS, nbLocals);
  concreteCode->Init(SKIP_CCC_PC_POS, skipCCCPC);
  return static_cast<NativeConcreteCode *>(concreteCode);
}

word NativeConcreteCode::New(TagVal *abstractCode) {
  Closure *closure = LazyCompileClosure::New(abstractCode);
  word byneed      = Byneed::New(closure->ToWord())->ToWord();
  closure->Init(1, byneed);
  return byneed;
}

void NativeConcreteCode::Disassemble(std::FILE *file) {
  Transform *transform = Transform::FromWordDirect(Get(TRANSFORM_POS));
  TagVal *abstractCode = TagVal::FromWordDirect(transform->GetArgument());
  Tuple *coord = Tuple::FromWordDirect(abstractCode->Sel(0));
  fprintf(file, "Disassembling function at %s:%d.%d\n\n",
	  String::FromWordDirect(coord->Sel(0))->ExportC(),
	  Store::DirectWordToInt(coord->Sel(1)),
	  Store::DirectWordToInt(coord->Sel(2)));
  TagVal *pc = TagVal::FromWordDirect(abstractCode->Sel(4));
  AbstractCode::Disassemble(file, pc);
}

#endif
