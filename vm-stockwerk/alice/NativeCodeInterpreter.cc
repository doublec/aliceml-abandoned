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
#include "generic/TaskStack.hh"
#include "generic/Scheduler.hh"
#include "generic/Backtrace.hh"
#include "generic/Closure.hh"
#include "generic/ConcreteCode.hh"
#include "generic/Transients.hh"
#include "generic/Transform.hh"
#include "alice/Data.hh"
#include "alice/AbstractCode.hh"
#include "alice/NativeConcreteCode.hh"
#include "alice/NativeCodeInterpreter.hh"
#include "alice/NativeCodeJitter.hh"

#include "generic/Debug.hh"

//
// Interpreter StackFrames
//
class NativeCodeFrame : public StackFrame {
protected:
  static const u_int PC_POS             = 0;
  static const u_int CODE_POS           = 1;
  static const u_int CLOSURE_POS        = 2;
  static const u_int IMMEDIATE_ARGS_POS = 3;
  static const u_int NB_LOCAL_ARGS_POS  = 4;
  static const u_int TASK_STACK_POS     = 5;
  static const u_int BASE_SIZE          = 6;
public:
  using Block::ToWord;
  using StackFrame::GetInterpreter;
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
  u_int GetNbLocals() {
    return Store::DirectWordToInt(StackFrame::GetArg(NB_LOCAL_ARGS_POS));
  }
  TaskStack *GetTaskStack() {
    return TaskStack::FromWordDirect(StackFrame::GetArg(TASK_STACK_POS));
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
			      int nbLocals,
			      TaskStack *taskStack) {
    u_int frSize      = BASE_SIZE + nbLocals;
    StackFrame *frame = StackFrame::New(NATIVE_CODE_FRAME, interpreter, frSize);
    frame->InitArg(PC_POS, pc);
    frame->InitArg(CODE_POS, code->ToWord());
    frame->InitArg(CLOSURE_POS, closure->ToWord());
    frame->InitArg(IMMEDIATE_ARGS_POS, immediateArgs->ToWord());
    frame->InitArg(NB_LOCAL_ARGS_POS, Store::IntToWord(nbLocals));
    frame->InitArg(TASK_STACK_POS, taskStack->ToWord());
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
  static const u_int PC_POS    = 0;
  static const u_int FRAME_POS = 1;
  static const u_int BASE_SIZE = 2;
public:
  using Block::ToWord;
  using StackFrame::GetInterpreter;
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

Block *
NativeCodeInterpreter::GetAbstractRepresentation(Block *blockWithHandler) {
  NativeConcreteCode *concreteCode = (NativeConcreteCode *) blockWithHandler;
  return concreteCode->GetAbstractRepresentation();
}

void NativeCodeInterpreter::PushCall(TaskStack *taskStack,
				     Closure *closure) {
  NativeConcreteCode *concreteCode =
    NativeConcreteCode::FromWord(closure->GetConcreteCode());
  Assert(concreteCode->GetInterpreter() == this);
  NativeCodeFrame *frame;
  int nLocals = concreteCode->GetNLocals();
  Chunk *code          = concreteCode->GetNativeCode();
  Tuple *immediateArgs = concreteCode->GetImmediateArgs();
  frame = NativeCodeFrame::New(this, NativeCodeJitter::GetInitialPC(),
			       code, closure, immediateArgs, nLocals,
			       taskStack);
  taskStack->PushFrame(frame->ToWord());
}

Interpreter::Result NativeCodeInterpreter::Run(TaskStack *taskStack) {
  NativeCodeFrame *frame =
    NativeCodeFrame::FromWordDirect(taskStack->GetFrame());
  Assert(frame->GetInterpreter() == this);
  Assert(taskStack == frame->GetTaskStack());
  Chunk *code        = frame->GetCode();
  native_fun execute = (native_fun) code->GetBase();
  return execute(frame);
}

Interpreter::Result
NativeCodeInterpreter::Handle(word exn, Backtrace *trace,
			      TaskStack *taskStack) {
  StackFrame *frame = StackFrame::FromWordDirect(taskStack->GetFrame());
  //Store::VerifyGC(exn);
  if (frame->GetLabel() == NATIVE_CODE_HANDLER_FRAME) {
    NativeCodeHandlerFrame *handlerFrame =
      static_cast<NativeCodeHandlerFrame *>(frame);
    NativeCodeFrame *codeFrame = handlerFrame->GetCodeFrame();
    codeFrame->SetPC(handlerFrame->GetPC());
    Tuple *package = Tuple::New(2);
    package->Init(0, exn);
    package->Init(1, trace->ToWord());
    Scheduler::nArgs          = 2;
    Scheduler::currentArgs[0] = package->ToWord();
    Scheduler::currentArgs[1] = exn;
    taskStack->PopFrame();
    taskStack->PushFrame(codeFrame->ToWord());
    return Interpreter::CONTINUE;
  }
  else {
    taskStack->PopFrame();
    trace->Enqueue(frame->ToWord());
    Scheduler::currentBacktrace = trace;
    Scheduler::currentData      = exn;
    return Interpreter::RAISE;
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
    NativeConcreteCode::FromWordDirect(closure->GetConcreteCode());
  Transform *transform =
    static_cast<Transform *>(concreteCode->GetAbstractRepresentation());
  TagVal *abstractCode = TagVal::FromWordDirect(transform->GetArgument());
  Tuple *coord         = Tuple::FromWord(abstractCode->Sel(0));
  String *name         = String::FromWord(coord->Sel(0));
  std::fprintf(stderr, "Alice(native) %s %.*s, line %d\n",
	       frameType, (int) name->GetSize(), name->GetValue(),
	       Store::WordToInt(coord->Sel(1)));
}
