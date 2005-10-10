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

#if defined(INTERFACE)
#pragma implementation "alice/ByteConcreteCode.hh"
#endif

#include "alice/ByteConcreteCode.hh"
#include "alice/ByteCode.hh"
#include "alice/ByteCodeJitter.hh"
#include "alice/AbstractCode.hh"
#include "alice/AliceLanguageLayer.hh"

// LazyByteCompile Frame
class LazyByteCompileFrame : private StackFrame {
private:
  enum { CLOSURE_POS, SIZE };
public:
  // LazyByteCompileFrame Accessors
  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  LazyByteCompileClosure *GetClosure() {
    return LazyByteCompileClosure
      ::FromWordDirect(StackFrame::GetArg(CLOSURE_POS));
  }
  // LazyByteCompileFrame Constructor
  static LazyByteCompileFrame *New(Interpreter *interpreter,
				   Closure *closure) {
    NEW_STACK_FRAME(frame, interpreter, SIZE);
    frame->InitArg(CLOSURE_POS, closure->ToWord());
    return STATIC_CAST(LazyByteCompileFrame *, frame);
  }
};

//
// LazyCompileInterpreter
//
LazyByteCompileInterpreter *LazyByteCompileInterpreter::self;
word LazyByteCompileInterpreter::concreteCode;

void LazyByteCompileInterpreter::Init() {
  self = new LazyByteCompileInterpreter();
  concreteCode = ConcreteCode::New(self, 0)->ToWord();
  RootSet::Add(concreteCode);
}

void LazyByteCompileInterpreter::PushCall(Closure *closure) {
  LazyByteCompileFrame::New(self, closure);
}

u_int LazyByteCompileInterpreter::GetFrameSize(StackFrame *sFrame) {
  LazyByteCompileFrame *frame = STATIC_CAST(LazyByteCompileFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result LazyByteCompileInterpreter::Run(StackFrame *sFrame) {
  LazyByteCompileFrame *frame = STATIC_CAST(LazyByteCompileFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  LazyByteCompileClosure *closure = frame->GetClosure();
  Scheduler::PopFrame(frame->GetSize());
  Scheduler::SetNArgs(1);
  ByteCodeJitter jitter;
  //  fprintf(stderr,"invoke byte code jitter\n");
  word bcc = jitter.Compile(closure);
  Scheduler::SetCurrentArg(0, bcc);//jitter.Compile(closure));
  // fprintf(stderr,"got bcc %p\n",bcc);
  return Worker::CONTINUE;
}

u_int LazyByteCompileInterpreter::GetInArity(ConcreteCode *) {
  Error("LazyByteCompileInterpreter::GetInArity should not be called\n");
  return 0;
}

u_int LazyByteCompileInterpreter::GetOutArity(ConcreteCode *) {
  Error("LazyByteCompileInterpreter::GetOutArity should not be called\n");
  return 1;
}

const char *LazyByteCompileInterpreter::Identify() {
  return "LazyByteCompileInterpreter";
}

void LazyByteCompileInterpreter::DumpFrame(StackFrame *) {
  std::fprintf(stderr, "LazyCompile");
}

//
// LazyCompileClosure
//
LazyByteCompileClosure *LazyByteCompileClosure::New(TagVal *abstractCode) {
  Closure *closure = 
    Closure::New(LazyByteCompileInterpreter::concreteCode, SIZE);
  closure->Init(ABSTRACT_CODE, abstractCode->ToWord());
  // closure->Init(BYNEED_POS, byneed) done in ByteConcreteCode::New
  closure->Init(N_LOCALS_POS, Store::IntToWord(-1));
  closure->Init(INLINE_INFO_POS, Store::IntToWord(Types::NONE));
  return STATIC_CAST(LazyByteCompileClosure *, closure);
}

//
// ByteConcreteCode
//
ByteConcreteCode *ByteConcreteCode::NewInternal(TagVal *abstractCode,
						Chunk *code,
						word immediateEnv,
						word nbLocals,
						word inlineInfo) {
  ConcreteCode *concreteCode =
    ConcreteCode::New(ByteCodeInterpreter::self, SIZE);
  Chunk *name =
    Store::DirectWordToChunk(AliceLanguageLayer::TransformNames::function);
  Transform *transform = Transform::New(name, abstractCode->ToWord());
  concreteCode->Init(TRANSFORM_POS, transform->ToWord());
  concreteCode->Init(BYTE_CODE_POS, code->ToWord());
  concreteCode->Init(IMMEDIATE_ENV_POS, immediateEnv);
  concreteCode->Init(NLOCALS_POS, nbLocals);
  Vector *args = Vector::FromWordDirect(abstractCode->Sel(3));
  concreteCode->Init(IN_ARITY_POS, Store::IntToWord(args->GetLength()));
  TagVal *outArityOpt = TagVal::FromWord(abstractCode->Sel(4));
  word outArity = ((outArityOpt == INVALID_POINTER) 
		   ? Store::IntToWord(-1) : outArityOpt->Sel(0));
  concreteCode->Init(OUT_ARITY_POS, outArity);
  concreteCode->Init(INLINE_INFO_POS, inlineInfo);

  return STATIC_CAST(ByteConcreteCode *, concreteCode);
}

word ByteConcreteCode::New(TagVal *abstractCode) {
  Closure *closure = LazyByteCompileClosure::New(abstractCode);
  word byneed      = Byneed::New(closure->ToWord())->ToWord();
  closure->Init(1, byneed);
  return byneed;
}

void ByteConcreteCode::Disassemble(std::FILE *file) {
  Transform *transform = Transform::FromWordDirect(Get(TRANSFORM_POS));
  TagVal *abstractCode = TagVal::FromWordDirect(transform->GetArgument());
  Tuple *coord = Tuple::FromWordDirect(abstractCode->Sel(0));
  fprintf(file, "Disassembling byte code function at %s:%d.%d\n\n",
	  String::FromWordDirect(coord->Sel(0))->ExportC(),
	  Store::DirectWordToInt(coord->Sel(1)),
	  Store::DirectWordToInt(coord->Sel(2)));
  Chunk *code = GetByteCode();
  Tuple *imEnv = GetImmediateArgs();
#ifdef THREADED
  ByteCode::Disassemble(file,(u_int *)code->GetBase(),code,imEnv);
#else
  ByteCode::Disassemble(file,0,code,imEnv);
#endif
}
