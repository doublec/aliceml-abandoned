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
#include "generic/Scheduler.hh"
#include "generic/Backtrace.hh"
#include "generic/Closure.hh"
#include "generic/ConcreteCode.hh"
#include "generic/Transients.hh"
#include "generic/Transform.hh"
#include "alice/Data.hh"
#include "alice/StackFrame.hh"
#include "alice/AbstractCode.hh"
#include "alice/NativeConcreteCode.hh"
#include "alice/NativeCodeInterpreter.hh"
#include "alice/NativeCodeJitter.hh"
#include "alice/LivenessInformation.hh"

//
// Interpreter StackFrames
//
class NativeCodeFrame : public StackFrame {
protected:
  enum {
    PC_POS, CODE_POS, CLOSURE_POS, IMMEDIATE_ARGS_POS, CONTINUATION_POS,
    BASE_SIZE
  };
public:
  using Block::ToWord;

  // NativeCodeFrame Accessors
  u_int GetPC() {
    return (u_int) Store::DirectWordToInt(StackFrame::GetArg(PC_POS));
  }
  void SetPC(u_int pc) {
    StackFrame::InitArg(PC_POS, Store::IntToWord(pc));
  }
  Chunk *GetCode() {
    return Store::DirectWordToChunk(StackFrame::GetArg(CODE_POS));
  }
  void SetCode(Chunk *code) {
    StackFrame::ReplaceArg(CODE_POS, code->ToWord());
  }
  Closure *GetClosure() {
    return Closure::FromWord(StackFrame::GetArg(CLOSURE_POS));
  }
  Tuple *GetImmediateArgs() {
    return Tuple::FromWord(StackFrame::GetArg(IMMEDIATE_ARGS_POS));
  }
  void SetImmediateArgs(Tuple *immediateArgs) {
    StackFrame::ReplaceArg(IMMEDIATE_ARGS_POS, immediateArgs->ToWord());
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
    u_int frSize      = BASE_SIZE + nbLocals;
    StackFrame *frame = StackFrame::New(NATIVE_CODE_FRAME, interpreter, frSize);
    frame->InitArg(PC_POS, pc);
    frame->InitArg(CODE_POS, code->ToWord());
    frame->InitArg(CLOSURE_POS, closure->ToWord());
    frame->InitArg(IMMEDIATE_ARGS_POS, immediateArgs->ToWord());
    frame->InitArg(CONTINUATION_POS, continuation);
    return static_cast<NativeCodeFrame *>(frame);
  }
  // NativeCodeFrame Untagging
  static NativeCodeFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == NATIVE_CODE_FRAME);
    return static_cast<NativeCodeFrame *>(p);
  }
};

class NativeCodeHandlerFrame : public StackFrame {
protected:
  enum { PC_POS, FRAME_POS, BASE_SIZE };
public:
  using Block::ToWord;

  // NativeCodeHandlerFrame Accessors
  u_int GetPC() {
    return (u_int) Store::DirectWordToInt(StackFrame::GetArg(PC_POS));
  }
  NativeCodeFrame *GetCodeFrame() {
    return NativeCodeFrame::FromWordDirect(StackFrame::GetArg(FRAME_POS));
  }
  // NativeCodeHandlerFrame Untagging
  static NativeCodeHandlerFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == NATIVE_CODE_HANDLER_FRAME);
    return static_cast<NativeCodeHandlerFrame *>(p);
  }
};

//
// Interpreter Functions
//
NativeCodeInterpreter *NativeCodeInterpreter::self;

static inline word MakeNativeFrame(word continuation, Closure *closure) {
  NativeConcreteCode *concreteCode =
    NativeConcreteCode::FromWord(closure->GetConcreteCode());
  Assert(concreteCode->GetInterpreter() == NativeCodeInterpreter::self);
  u_int nLocals        = concreteCode->GetNLocals();
  Chunk *code          = concreteCode->GetNativeCode();
  Tuple *immediateArgs = concreteCode->GetImmediateArgs();
  return NativeCodeFrame::New(NativeCodeInterpreter::self,
			      NativeCodeJitter::GetInitialPC(),
			      code, closure, immediateArgs,
			      continuation, nLocals)->ToWord();
}

word NativeCodeInterpreter::FastPushCall(word continuation, Closure *closure) {
  word frame = MakeNativeFrame(continuation, closure);
  Scheduler::PushFrame(frame);
  return frame;
}

word NativeCodeInterpreter::TailPushCall(Closure *closure) {
  word frame =
    MakeNativeFrame(NativeCodeJitter::GetDefaultContinuation(), closure);
  Scheduler::PopFrame();
  Scheduler::PushFrameNoCheck(frame);
  return frame;
}

Transform *
NativeCodeInterpreter::GetAbstractRepresentation(ConcreteRepresentation *b) {
  return static_cast<NativeConcreteCode *>(b)->GetAbstractRepresentation();
}

void NativeCodeInterpreter::PushCall(Closure *closure) {
  word frame =
    MakeNativeFrame(NativeCodeJitter::GetDefaultContinuation(), closure);
  Scheduler::PushFrame(frame);
}

Worker::Result NativeCodeInterpreter::Run() {
  NativeCodeFrame *frame =
    NativeCodeFrame::FromWordDirect(Scheduler::GetFrame());
#if 0
  Block *p = (Block *) frame;
  if (!HeaderOp::IsChildish(p)) {
    Store::AddToIntgenSet(p);
  }
#endif
  Assert(frame->GetWorker() == this);
  Chunk *code        = frame->GetCode();
  native_fun execute = (native_fun) code->GetBase();
  return execute(frame);
}

Worker::Result NativeCodeInterpreter::Handle(word data) {
  NativeCodeFrame *frame =
    NativeCodeFrame::FromWordDirect(Scheduler::GetFrame());
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

void NativeCodeInterpreter::DumpFrame(word frame) {
  Block *p = Store::DirectWordToBlock(frame);
  Assert(p->GetLabel() == (BlockLabel) NATIVE_CODE_FRAME ||
	 p->GetLabel() == (BlockLabel) NATIVE_CODE_HANDLER_FRAME);
  NativeCodeFrame *codeFrame;
  const char *frameType;
  if (p->GetLabel() == (BlockLabel) NATIVE_CODE_FRAME) {
    codeFrame = NativeCodeFrame::FromWordDirect(frame);
    frameType = "function";
  }
  else {
    NativeCodeHandlerFrame *handlerFrame =
      NativeCodeHandlerFrame::FromWordDirect(frame);
    codeFrame = handlerFrame->GetCodeFrame();
    frameType = "handler";
  }
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
  Block *p = (Block *) frame;
  NativeCodeFrame *codeFrame;
  const char *frameType;
  if (p->GetLabel() == (BlockLabel) NATIVE_CODE_FRAME) {
    codeFrame = NativeCodeFrame::FromWordDirect(frame->ToWord());
    frameType = "function";
  }
  else {
    NativeCodeHandlerFrame *handlerFrame =
      NativeCodeHandlerFrame::FromWordDirect(frame->ToWord());
    codeFrame = handlerFrame->GetCodeFrame();
    frameType = "handler";
  }
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
