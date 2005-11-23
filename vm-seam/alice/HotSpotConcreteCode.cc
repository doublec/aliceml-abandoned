//
// Author:
//   Christian Mueller <cmueller@ps.uni-sb.de>
//
// Copyright:
//   Christian Mueller, 2005
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "alice/HotSpotConcreteCode.hh"
#endif

#include "alice/HotSpotConcreteCode.hh"
#include "alice/ByteCodeJitter.hh"
#include "alice/ByteCodeInterpreter.hh"
#include "alice/ByteConcreteCode.hh"
#include "alice/AbstractCodeInterpreter.hh"
#include "alice/AliceConcreteCode.hh"
#include "alice/AbstractCode.hh"

// initialization of the state machine
typedef enum state { ABSTRACT_CODE, BYTE_CODE, NSTATES };

#define ABSTRACT_CODE_COUNTER_INIT 5 // execute abstract code five times

word HotSpotConcreteCode::New(TagVal *abstractCode) {
  // reserve enough space for final state
  ConcreteCode *concreteCode = 
    ConcreteCode::New(HotSpotInterpreter::self, ByteConcreteCode::SIZE);
  // start with abstract code
  concreteCode->Init(CODE, AliceConcreteCode::New(abstractCode));
  concreteCode->Init(COUNTER, Store::IntToWord(ABSTRACT_CODE_COUNTER_INIT));
  // set all other arguments to 0
  word zero = Store::IntToWord(0);
  for(u_int i=SIZE; i<ByteConcreteCode::SIZE; i++)
    concreteCode->Init(i, zero);
  return concreteCode->ToWord();
}

Transform *HotSpotConcreteCode::GetAbstractRepresentation() {
  return AliceConcreteCode::FromWordDirect(GetCode())->GetAbstractRepresentation();
}

HotSpotInterpreter *HotSpotInterpreter::self;

void HotSpotInterpreter::Init() {
  self = new HotSpotInterpreter();
}

// this implements the transition from abstract code into byte code
void HotSpotInterpreter::PushCall(Closure *closure) {
  HotSpotConcreteCode *wrapper =
    HotSpotConcreteCode::FromWordDirect(closure->GetConcreteCode());
  if(wrapper->GetCounter() > 0) {
    wrapper->DecCounter();
    // temporary remove indirection to use the real push call method
    closure->SetConcreteCode(wrapper->GetCode());
    AbstractCodeInterpreter::self->PushCall(closure);
    closure->SetConcreteCode(wrapper->ToWord());
  } else {	
    // go to next state
    // reset the counter if the next state is not a final one
    // TODO: remove the indirection over the lazy compile closure
    AliceConcreteCode *acc = 
      AliceConcreteCode::FromWord(wrapper->GetCode());
    LazyByteCompileClosure *compileClosure = 
      LazyByteCompileClosure::New(acc->GetAbstractCode());
    compileClosure->Init(1, wrapper->ToWord()); // set self call pointer
    ByteCodeJitter jitter;
    word wByteConcreteCode = jitter.Compile(compileClosure);
    // convert the wrapper into byte concrete code
    Block *src = Store::DirectWordToBlock(wByteConcreteCode);
    Block *dst = Store::DirectWordToBlock(wrapper->ToWord());
    for(u_int i = src->GetSize(); i--; )
      dst->ReplaceArg(i, src->GetArg(i));
    // push the closure
    ByteCodeInterpreter::self->PushCall(closure);
  }
}

Worker::Result HotSpotInterpreter::Run(StackFrame *) {
  Error("HotSpotInterpreter should transfer control to real interpreters");
  // Design option:
  // Instead of cloning the closure in HotSpotInterpreter::PushCall we could
  // replace the concrete code handler by HotSpotInterpreter::self. So in the
  // Run method we can then simply call the interpreter that fits the 
  // current state.
}

Transform *
HotSpotInterpreter::GetAbstractRepresentation(ConcreteRepresentation *b) {
  return STATIC_CAST(HotSpotConcreteCode *, b)->GetAbstractRepresentation();
}

const char *HotSpotInterpreter::Identify() {
  return "HotSpotInterpreter";
}

// pass information of the corresponding state

u_int HotSpotInterpreter::GetFrameSize(StackFrame *sFrame) {
  Worker *worker = sFrame->GetWorker();
  if(worker == AbstractCodeInterpreter::self) {
    AbstractCodeInterpreter::self->GetFrameSize(sFrame);
  } else {
    Error("wrong code state");
  }
}

void HotSpotInterpreter::DumpFrame(StackFrame *sFrame) {
  Worker *worker = sFrame->GetWorker();
  if(worker == AbstractCodeInterpreter::self) {
    AbstractCodeInterpreter::self->DumpFrame(sFrame);
  } else {
    Error("wrong code state");
  }
}

u_int HotSpotInterpreter::GetInArity(ConcreteCode *concreteCode) {
  HotSpotConcreteCode *wrapper = 
    STATIC_CAST(HotSpotConcreteCode *, concreteCode);
  ConcreteCode *realConcreteCode = 
    ConcreteCode::FromWordDirect(wrapper->GetCode());
  return AbstractCodeInterpreter::self->GetInArity(realConcreteCode);
}

u_int HotSpotInterpreter::GetOutArity(ConcreteCode *concreteCode) {
  HotSpotConcreteCode *wrapper = 
    STATIC_CAST(HotSpotConcreteCode *, concreteCode);
  ConcreteCode *realConcreteCode = 
    ConcreteCode::FromWordDirect(wrapper->GetCode());
  return AbstractCodeInterpreter::self->GetOutArity(realConcreteCode);
}
