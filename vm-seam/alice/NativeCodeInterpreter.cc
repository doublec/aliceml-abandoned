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
#pragma implementation "alice/NativeCodeInterpreter.hh"
#endif

#include <cstdio>
#include "alice/Data.hh"
#include "alice/AbstractCode.hh"
#include "alice/NativeConcreteCode.hh"
#include "alice/NativeCodeInterpreter.hh"
#include "alice/NativeCodeJitter.hh"

//
// Interpreter StackFrames
//
class NativeCodeFrame : public StackFrame {
protected:
  enum {
    PC_POS, CODE_POS, CLOSURE_POS, IMMEDIATE_ARGS_POS,
    CONTINUATION_POS, BASE_SIZE
  };
public:
  // NativeCodeFrame Accessors
  Tuple *GetImmediateArgs() {
    return Tuple::FromWord(StackFrame::GetArg(IMMEDIATE_ARGS_POS));
  }
  u_int GetSize() {
    return Store::DirectWordToInt(GetImmediateArgs()->Sel(0));
  }
  u_int GetPC() {
    return (u_int) Store::DirectWordToInt(StackFrame::GetArg(PC_POS));
  }
  void SetPC(u_int pc) {
    StackFrame::InitArg(PC_POS, Store::IntToWord(pc));
  }
  Chunk *GetCode() {
    return Store::DirectWordToChunk(StackFrame::GetArg(CODE_POS));
  }
  Closure *GetClosure() {
    return Closure::FromWord(StackFrame::GetArg(CLOSURE_POS));
  }
  void InitLocalEnv(u_int index, word value) {
    StackFrame::InitArg(BASE_SIZE + index, value);
  }
  // NativeCodeFrame Constructor
  static NativeCodeFrame *New(Interpreter *interpreter,
			      word pc,
			      Chunk *code,
			      Closure *closure,
			      Tuple *immediateArgs,
			      word continuation,
			      u_int nbLocals) {
    u_int frSize = BASE_SIZE + nbLocals;
    NEW_STACK_FRAME(frame, interpreter, frSize);
    frame->InitArg(PC_POS, pc);
    frame->InitArg(CODE_POS, code->ToWord());
    frame->InitArg(CLOSURE_POS, closure->ToWord());
    frame->InitArg(IMMEDIATE_ARGS_POS, immediateArgs->ToWord());
    frame->InitArg(CONTINUATION_POS, continuation);
    return static_cast<NativeCodeFrame *>(frame);
  }
};

//
// Interpreter Functions
//
NativeCodeInterpreter *NativeCodeInterpreter::self;

static inline StackFrame *MakeNativeFrame(NativeConcreteCode *concreteCode,
					  word continuation, Closure *closure) {
  Assert(concreteCode->GetInterpreter() == NativeCodeInterpreter::self);
  u_int nLocals        = concreteCode->GetNLocals();
  Chunk *code          = concreteCode->GetNativeCode();
  Tuple *immediateArgs = concreteCode->GetImmediateArgs();
  NativeCodeFrame *frame =
    NativeCodeFrame::New(NativeCodeInterpreter::self,
			 NativeCodeJitter::GetInitialPC(),
			 code, closure, immediateArgs,
			 continuation, nLocals);
  for (u_int i = nLocals; i--;)
    frame->InitLocalEnv(i, Store::IntToWord(0));
  return static_cast<StackFrame *>(frame);
}

StackFrame *NativeCodeInterpreter::FastPushCall(word continuation,
						Closure *closure) {
  NativeConcreteCode *concreteCode =
    NativeConcreteCode::FromWordDirect(closure->GetConcreteCode());
  return MakeNativeFrame(concreteCode, continuation, closure);
}

Transform *
NativeCodeInterpreter::GetAbstractRepresentation(ConcreteRepresentation *b) {
  return static_cast<NativeConcreteCode *>(b)->GetAbstractRepresentation();
}

u_int NativeCodeInterpreter::GetFrameSize(StackFrame *sFrame) {
  NativeCodeFrame *frame = static_cast<NativeCodeFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

void NativeCodeInterpreter::PushCall(Closure *closure) {
  NativeConcreteCode *concreteCode =
    NativeConcreteCode::FromWord(closure->GetConcreteCode());
  MakeNativeFrame(concreteCode, Store::IntToWord(0), closure);
}

Worker::Result NativeCodeInterpreter::Run(StackFrame *sFrame) {
  NativeCodeFrame *frame = static_cast<NativeCodeFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  Chunk *code        = frame->GetCode();
  native_fun execute = (native_fun) code->GetBase();
  return execute(frame);
}

Worker::Result NativeCodeInterpreter::Handle(word data) {
  StackFrame *sFrame = Scheduler::GetFrame();
  NativeCodeFrame *frame = static_cast<NativeCodeFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  frame->SetPC(Store::DirectWordToInt(data));
  Tuple *package = Tuple::New(2);
  word exn = Scheduler::currentData;
  package->Init(0, exn);
  package->Init(1, Scheduler::currentBacktrace->ToWord());
  Scheduler::nArgs = 2;
  Scheduler::currentArgs[0] = package->ToWord();
  Scheduler::currentArgs[1] = exn;
  return Worker::CONTINUE;
}

u_int NativeCodeInterpreter::GetInArity(ConcreteCode *concreteCode) {
  Assert(concreteCode->GetInterpreter() == NativeCodeInterpreter::self);
  NativeConcreteCode *nativeConcreteCode =
    static_cast<NativeConcreteCode *>(concreteCode);
  Transform *transform = nativeConcreteCode->GetAbstractRepresentation();
  TagVal *abstractCode = TagVal::FromWordDirect(transform->GetArgument());
  TagVal *args = TagVal::FromWordDirect(abstractCode->Sel(3));
  switch (AbstractCode::GetArgs(args)) {
  case AbstractCode::OneArg:
    return Scheduler::ONE_ARG;
  case AbstractCode::TupArgs:
    return Vector::FromWordDirect(args->Sel(0))->GetLength();
  default:
    Error("invalid args tag");
  }
}

const char *NativeCodeInterpreter::Identify() {
  return "NativeCodeInterpreter";
}

void NativeCodeInterpreter::DumpFrame(StackFrame *sFrame) {
  NativeCodeFrame *codeFrame = static_cast<NativeCodeFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  const char *frameType;
  frameType = "function";
  // to be done: frameType = "handler";
  // Print closure information
  Closure *closure = codeFrame->GetClosure();
  NativeConcreteCode *concreteCode =
    NativeConcreteCode::FromWord(closure->GetConcreteCode());
  Transform *transform =
    static_cast<Transform *>(concreteCode->GetAbstractRepresentation());
  TagVal *abstractCode = TagVal::FromWordDirect(transform->GetArgument());
  Tuple *coord         = Tuple::FromWord(abstractCode->Sel(0));
  String *name         = String::FromWord(coord->Sel(0));
  std::fprintf(stderr, "Alice native %s %.*s, line %d, column %d\n",
	       frameType, (int) name->GetSize(), name->GetValue(),
	       Store::WordToInt(coord->Sel(1)),
	       Store::WordToInt(coord->Sel(2)));
}

#if PROFILE
word NativeCodeInterpreter::GetProfileKey(StackFrame *frame) {
  Block *p = (Block *) frame;
  if (p->GetLabel() == (BlockLabel) NATIVE_CODE_FRAME) {
    NativeCodeFrame *f = (NativeCodeFrame *) frame;
    word concreteCode = f->GetClosure()->GetConcreteCode();
    return ConcreteCode::FromWord(concreteCode)->ToWord();
  }
  else {
    NativeCodeHandlerFrame *f = (NativeCodeHandlerFrame *) frame;
    word concreteCode = f->GetCodeFrame()->GetClosure()->GetConcreteCode();
    return ConcreteCode::FromWord(concreteCode)->ToWord();
  }
}

word NativeCodeInterpreter::GetProfileKey(ConcreteCode *concreteCode) {
  return concreteCode->ToWord();
}

static String *
MakeProfileName(NativeConcreteCode *concreteCode, const char *type) {
  Transform *transform =
    static_cast<Transform *>(concreteCode->GetAbstractRepresentation());
  TagVal *abstractCode = TagVal::FromWordDirect(transform->GetArgument());
  Tuple *coord         = Tuple::FromWord(abstractCode->Sel(0));
  String *name         = String::FromWord(coord->Sel(0));
  char buf[1024]; // to be done
  std::sprintf(buf, "Alice native %s %.*s, line %d, column %d",
	       type, (int) name->GetSize(), name->GetValue(),
	       Store::WordToInt(coord->Sel(1)),
	       Store::WordToInt(coord->Sel(2)));
  return String::New(buf);
}

String *NativeCodeInterpreter::GetProfileName(StackFrame *frame) {
  NativeCodeFrame *codeFrame = NativeCodeFrame::FromWordDirect(frame->ToWord());
  const char *frameType;
  frameType = "function";
  // to be done: frameType = "handler";
  Closure *closure = codeFrame->GetClosure();
  NativeConcreteCode *nativeConcreteCode =
    NativeConcreteCode::FromWord(closure->GetConcreteCode());
  return MakeProfileName(nativeConcreteCode, frameType);
}

String *NativeCodeInterpreter::GetProfileName(ConcreteCode *concreteCode) {
  NativeConcreteCode *nativeConcreteCode =
    static_cast<NativeConcreteCode *>(concreteCode);
  return MakeProfileName(nativeConcreteCode, "function");
}
#endif

void DisassembleNative(Closure *closure) {
  NativeConcreteCode *concreteCode =
    NativeConcreteCode::FromWord(closure->GetConcreteCode());
  concreteCode->Disassemble(stdout);
}

#endif
