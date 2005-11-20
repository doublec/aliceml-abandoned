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
  ConcreteCode *concreteCode = 
    ConcreteCode::New(HotSpotInterpreter::self, SIZE);
  // start with abstract code
  concreteCode->Init(STATE, Store::IntToWord(ABSTRACT_CODE));
  concreteCode->Init(CODE, AliceConcreteCode::New(abstractCode));
  // execute abstract code only once
  concreteCode->Init(COUNTER, Store::IntToWord(ABSTRACT_CODE_COUNTER_INIT));
  return concreteCode->ToWord();
}


Transform *HotSpotConcreteCode::GetAbstractRepresentation() {
  switch(GetState()) {
  case ABSTRACT_CODE:
    return AliceConcreteCode::FromWordDirect(GetCode())->GetAbstractRepresentation();
  case BYTE_CODE:
    return ByteConcreteCode::FromWordDirect(GetCode())->GetAbstractRepresentation();
  default:
    Error("wrong state");
  }
}

HotSpotInterpreter *HotSpotInterpreter::self;

void HotSpotInterpreter::Init() {
  self = new HotSpotInterpreter();
}

void HotSpotInterpreter::PushCall(Closure *closure) {
  HotSpotConcreteCode *wrapper =
    HotSpotConcreteCode::FromWordDirect(closure->GetConcreteCode());
  switch(wrapper->GetState()) {
  case ABSTRACT_CODE:
    {
      if(wrapper->GetCounter() > 0) {
	wrapper->DecCounter();
	// create a copy of the closure with a real concrete code
	// TODO: store this new closure somewhere if counter threshold is greater 1
	u_int size = closure->GetSize();
	Closure *newClosure = Closure::New(wrapper->GetCode(), size);
	for(u_int i = size; i--; )
	  newClosure->Init(i, closure->Sub(i));
	AbstractCodeInterpreter::self->PushCall(newClosure);
      } else {	
	// go to next state
	wrapper->SetState(BYTE_CODE);
	// reset the counter if the next state is not a final one
	// TODO: remove the indirection over the lazy compile closure
	AliceConcreteCode *acc = 
	  AliceConcreteCode::FromWord(wrapper->GetCode());
	LazyByteCompileClosure *compileClosure = 
	  LazyByteCompileClosure::New(acc->GetAbstractCode());
	word byneed = Byneed::New(compileClosure->ToWord())->ToWord();
	compileClosure->Init(1, byneed);
	ByteCodeJitter jitter;
	word wByteConcreteCode = jitter.Compile(compileClosure);
	// remove the hot spot indirection from the current closure
	wrapper->SetCode(wByteConcreteCode);
	closure->SetConcreteCode(wByteConcreteCode);
	// push the updated closure
	ByteCodeInterpreter::self->PushCall(closure);
      }
    }
    break;
  case BYTE_CODE:
    {
      // remove indirection
      // Of course it would be better to adopt the mechanism of transients, i.e.
      // to transform the wrapper into a ref node onto the real concrete code.
      closure->SetConcreteCode(wrapper->GetCode());
      ByteCodeInterpreter::self->PushCall(closure);      
    }
    break;
  default:
    Error("internal consistancy error: wrong code state");
  };
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
  } else if(worker == ByteCodeInterpreter::self) {
    ByteCodeInterpreter::self->GetFrameSize(sFrame);
  }
  Error("wrong code state");
}

void HotSpotInterpreter::DumpFrame(StackFrame *sFrame) {
  Worker *worker = sFrame->GetWorker();
  if(worker == AbstractCodeInterpreter::self) {
    AbstractCodeInterpreter::self->DumpFrame(sFrame);
  } else if(worker == ByteCodeInterpreter::self) {
    ByteCodeInterpreter::self->DumpFrame(sFrame);
  }
  Error("wrong code state");
}

u_int HotSpotInterpreter::GetInArity(ConcreteCode *concreteCode) {
  HotSpotConcreteCode *wrapper = STATIC_CAST(HotSpotConcreteCode *, concreteCode);
  ConcreteCode *realConcreteCode = ConcreteCode::FromWord(wrapper->GetCode());
  switch(wrapper->GetState()) {
  case ABSTRACT_CODE:
    return AbstractCodeInterpreter::self->GetInArity(realConcreteCode);
  case BYTE_CODE:
    return ByteCodeInterpreter::self->GetInArity(realConcreteCode);
  default:
    Error("internal consistancy error: wrong code state");
  };
}

u_int HotSpotInterpreter::GetOutArity(ConcreteCode *concreteCode) {
  HotSpotConcreteCode *wrapper = STATIC_CAST(HotSpotConcreteCode *, concreteCode);
  ConcreteCode *realConcreteCode = ConcreteCode::FromWord(wrapper->GetCode());
  switch(wrapper->GetState()) {
  case ABSTRACT_CODE:
    return AbstractCodeInterpreter::self->GetOutArity(realConcreteCode);
  case BYTE_CODE:
    return ByteCodeInterpreter::self->GetOutArity(realConcreteCode);
  default:
    Error("internal consistancy error: wrong code state");
  };
}
