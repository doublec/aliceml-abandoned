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

#include "alice/AbstractCode.hh"
#include "alice/NativeCodeJitter.hh"
#include "alice/AliceLanguageLayer.hh"

#if HAVE_LIGHTNING

// LazyCompile Frame
class LazyCompileFrame : private StackFrame {
private:
  enum { CLOSURE_POS, SIZE };
public:
  // LazyCompileFrame Accessors
  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  LazyCompileClosure *GetClosure() {
    return LazyCompileClosure::FromWordDirect(StackFrame::GetArg(CLOSURE_POS));
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
  LazyCompileFrame *frame = reinterpret_cast<LazyCompileFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result LazyCompileInterpreter::Run(StackFrame *sFrame) {
  LazyCompileFrame *frame = reinterpret_cast<LazyCompileFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  LazyCompileClosure *closure = frame->GetClosure();
  Scheduler::PopFrame(frame->GetSize());
  Scheduler::SetNArgs(1);
  NativeCodeJitter jitter;
  Scheduler::SetCurrentArg(0, jitter.Compile(closure));
  return Worker::CONTINUE;
}

u_int LazyCompileInterpreter::GetInArity(ConcreteCode *) {
  return 0;
}

u_int LazyCompileInterpreter::GetOutArity(ConcreteCode *) {
  return 1;
}

const char *LazyCompileInterpreter::Identify() {
  return "LazyCompileInterpreter";
}

void LazyCompileInterpreter::DumpFrame(StackFrame *, std::ostream& out) {
  out << "[NativeConcreteCode::LazyCompile]" << std::endl;
}

//
// LazyCompileClosure
//
LazyCompileClosure *LazyCompileClosure::New(TagVal *abstractCode) {
  Closure *closure = Closure::New(LazyCompileInterpreter::concreteCode, SIZE);
  closure->Init(ABSTRACT_CODE, abstractCode->ToWord());
  // closure->Init(BYNEED_POS, byneed) done in NativeConcreteCode::New
  closure->Init(N_LOCALS_POS, Store::IntToWord(-1));
  closure->Init(ASSIGNMENT_POS, Store::IntToWord(0));
  return static_cast<LazyCompileClosure *>(closure);
}

//
// NativeConcreteCode
//
NativeConcreteCode *NativeConcreteCode::NewInternal(TagVal *abstractCode,
						    Chunk *code,
						    word immediateEnv,
						    word nbLocals,
						    word CCCPC,
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
  concreteCode->Init(CCC_PC_POS, CCCPC);
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
  TagVal *pc = TagVal::FromWordDirect(abstractCode->Sel(5));
  AbstractCode::Disassemble(file, pc);
}

#endif
