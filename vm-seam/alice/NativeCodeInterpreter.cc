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
#pragma implementation "alice/NativeCodeInterpreter.hh"
#endif

#include <cstdio>
#include "alice/Data.hh"
#include "alice/AbstractCode.hh"
#include "alice/NativeConcreteCode.hh"
#include "alice/NativeCodeInterpreter.hh"
#include "alice/NativeCodeJitter.hh"

#if HAVE_LIGHTNING

//
// Interpreter StackFrames
//
class NativeCodeFrame : public StackFrame {
protected:
  enum {
    PC_POS, CODE_POS, CLOSURE_POS, IMMEDIATE_ARGS_POS, BASE_SIZE
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
  // NativeCodeFrame Constructor
  static NativeCodeFrame *New(Interpreter *interpreter,
			      u_int pc,
			      Chunk *code,
			      Closure *closure,
			      Tuple *immediateArgs,
			      u_int nbLocals) {
    u_int frSize = BASE_SIZE + nbLocals;
    NEW_STACK_FRAME(frame, interpreter, frSize);
    frame->InitArg(PC_POS, pc);
    frame->InitArg(CODE_POS, code->ToWord());
    frame->InitArg(CLOSURE_POS, closure->ToWord());
    frame->InitArg(IMMEDIATE_ARGS_POS, immediateArgs->ToWord());
    for (u_int i = nbLocals; i--;)
      frame->InitArg(BASE_SIZE + i, Store::IntToWord(0));
    return STATIC_CAST(NativeCodeFrame *, frame);
  }
};

//
// Interpreter Functions
//
NativeCodeInterpreter *NativeCodeInterpreter::self;

void NativeCodeInterpreter::Init() {
  self = new NativeCodeInterpreter();
}

u_int NativeCodeInterpreter::GetInArity(TagVal *abstractCode) {
  Vector *args = Vector::FromWordDirect(abstractCode->Sel(3));
  u_int nArgs = args->GetLength();
  return nArgs;
}

u_int NativeCodeInterpreter::GetOutArity(TagVal *abstractCode) {
  TagVal *outArityOpt = TagVal::FromWord(abstractCode->Sel(4));
  return ((outArityOpt == INVALID_POINTER) ? INVALID_INT :
	  Store::DirectWordToInt(outArityOpt->Sel(0)));
}

Transform *
NativeCodeInterpreter::GetAbstractRepresentation(ConcreteRepresentation *b) {
  return STATIC_CAST(NativeConcreteCode *, b)->GetAbstractRepresentation();
}

u_int NativeCodeInterpreter::GetFrameSize(StackFrame *sFrame) {
  NativeCodeFrame *frame = STATIC_CAST(NativeCodeFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

void NativeCodeInterpreter::PushCall(Closure *closure) {
  NativeConcreteCode *concreteCode =
    NativeConcreteCode::FromWord(closure->GetConcreteCode());
  Assert(concreteCode->GetInterpreter() == NativeCodeInterpreter::self);
  u_int nLocals        = concreteCode->GetNLocals();
  Chunk *code          = concreteCode->GetNativeCode();
  Tuple *immediateArgs = concreteCode->GetImmediateArgs();
  NativeCodeFrame::New(NativeCodeInterpreter::self,
		       concreteCode->GetCCCPC(),
		       code, closure, immediateArgs,
		       nLocals);
}

Worker::Result NativeCodeInterpreter::Run(StackFrame *sFrame) {
  NativeCodeFrame *frame = STATIC_CAST(NativeCodeFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  Chunk *code        = frame->GetCode();
  native_fun execute = (native_fun) code->GetBase();
  return execute();
}

Worker::Result NativeCodeInterpreter::Handle(word data) {
  StackFrame *sFrame = Scheduler::GetFrame();
  NativeCodeFrame *frame = STATIC_CAST(NativeCodeFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  frame->SetPC(Store::DirectWordToInt(data));
  Tuple *package = Tuple::New(2);
  word exn = Scheduler::GetCurrentData();
  package->Init(0, exn);
  package->Init(1, Scheduler::GetCurrentBacktrace()->ToWord());
  Scheduler::SetNArgs(2);
  Scheduler::SetCurrentArg(0, package->ToWord());
  Scheduler::SetCurrentArg(1, exn);
  return Worker::CONTINUE;
}

u_int NativeCodeInterpreter::GetInArity(ConcreteCode *concreteCode) {
  Assert(concreteCode->GetInterpreter() == NativeCodeInterpreter::self);
  NativeConcreteCode *nativeConcreteCode =
    STATIC_CAST(NativeConcreteCode *, concreteCode);
  Transform *transform = nativeConcreteCode->GetAbstractRepresentation();
  TagVal *abstractCode = TagVal::FromWordDirect(transform->GetArgument());
  return GetInArity(abstractCode);
}

u_int NativeCodeInterpreter::GetOutArity(ConcreteCode *concreteCode) {
  Assert(concreteCode->GetInterpreter() == NativeCodeInterpreter::self);
  NativeConcreteCode *nativeConcreteCode =
    STATIC_CAST(NativeConcreteCode *, concreteCode);
  Transform *transform = nativeConcreteCode->GetAbstractRepresentation();
  TagVal *abstractCode = TagVal::FromWordDirect(transform->GetArgument());
  return GetOutArity(abstractCode);
}

const char *NativeCodeInterpreter::Identify() {
  return "NativeCodeInterpreter";
}

void NativeCodeInterpreter::DumpFrame(StackFrame *sFrame) {
  NativeCodeFrame *codeFrame = STATIC_CAST(NativeCodeFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  const char *frameType;
  frameType = "function";
  // to be done: frameType = "handler";
  // Print closure information
  Closure *closure = codeFrame->GetClosure();
  NativeConcreteCode *concreteCode =
    NativeConcreteCode::FromWord(closure->GetConcreteCode());
  Transform *transform =
    STATIC_CAST(Transform *, concreteCode->GetAbstractRepresentation());
  TagVal *abstractCode = TagVal::FromWordDirect(transform->GetArgument());
  Tuple *coord         = Tuple::FromWord(abstractCode->Sel(0));
  String *name         = String::FromWord(coord->Sel(0));
  std::fprintf(stderr, //"Alice native %s %.*s, line %d, column %d\n",
	       "%.*s:%d.%d\n",
	       /*frameType,*/ (int) name->GetSize(), name->GetValue(),
	       Store::WordToInt(coord->Sel(1)),
	       Store::WordToInt(coord->Sel(2)));
}

#if PROFILE
word NativeCodeInterpreter::GetProfileKey(StackFrame *sFrame) {
  NativeCodeFrame *frame = STATIC_CAST(NativeCodeFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  word concreteCode = frame->GetClosure()->GetConcreteCode();
  return ConcreteCode::FromWord(concreteCode)->ToWord();
}

word NativeCodeInterpreter::GetProfileKey(ConcreteCode *concreteCode) {
  return concreteCode->ToWord();
}

static String *
MakeProfileName(NativeConcreteCode *concreteCode, const char *type) {
  Transform *transform =
    STATIC_CAST(Transform *, concreteCode->GetAbstractRepresentation());
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

String *NativeCodeInterpreter::GetProfileName(StackFrame *sFrame) {
  NativeCodeFrame *frame = STATIC_CAST(NativeCodeFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  const char *frameType;
  frameType = "function";
  // to be done: frameType = "handler";
  Closure *closure = frame->GetClosure();
  NativeConcreteCode *nativeConcreteCode =
    NativeConcreteCode::FromWord(closure->GetConcreteCode());
  return MakeProfileName(nativeConcreteCode, frameType);
}

String *NativeCodeInterpreter::GetProfileName(ConcreteCode *concreteCode) {
  NativeConcreteCode *nativeConcreteCode =
    STATIC_CAST(NativeConcreteCode *, concreteCode);
  return MakeProfileName(nativeConcreteCode, "function");
}
#endif

void DisassembleNative(Closure *closure) {
  NativeConcreteCode *concreteCode =
    NativeConcreteCode::FromWord(closure->GetConcreteCode());
  concreteCode->Disassemble(stdout);
}

#endif
