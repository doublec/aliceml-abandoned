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

#define ABSTRACT_CODE_COUNTER_INIT 5 // execute abstract code five times

word HotSpotConcreteCode::New(TagVal *abstractCode) {
  return HotSpot_StartState::New(HotSpotInterpreter::self, 
				 abstractCode,
				 ByteConcreteCode::SIZE,
				 AliceConcreteCode::New,
				 ABSTRACT_CODE_COUNTER_INIT);
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
    AliceConcreteCode *acc = 
      AliceConcreteCode::FromWordDirect(wrapper->GetCode());
    AbstractCodeInterpreter::self->PushCall_Internal(acc, closure);
  } else {
    ByteCodeJitter jitter;
    jitter.Compile(wrapper);
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
  return reinterpret_cast<HotSpotConcreteCode *>(b)->GetAbstractRepresentation();
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
    reinterpret_cast<HotSpotConcreteCode *>(concreteCode);
  ConcreteCode *realConcreteCode = 
    ConcreteCode::FromWordDirect(wrapper->GetCode());
  return AbstractCodeInterpreter::self->GetInArity(realConcreteCode);
}

u_int HotSpotInterpreter::GetOutArity(ConcreteCode *concreteCode) {
  HotSpotConcreteCode *wrapper = 
    reinterpret_cast<HotSpotConcreteCode *>(concreteCode);
  ConcreteCode *realConcreteCode = 
    ConcreteCode::FromWordDirect(wrapper->GetCode());
  return AbstractCodeInterpreter::self->GetOutArity(realConcreteCode);
}


#if PROFILE

word HotSpotInterpreter::GetProfileKey(StackFrame *frame) {
  Error("HotSpotInterpreter should transfer control to real interpreters");
}

String *HotSpotInterpreter::GetProfileName(StackFrame *frame) {
  Error("HotSpotInterpreter should transfer control to real interpreters");
}

word HotSpotInterpreter::GetProfileKey(ConcreteCode *cc) {
  return cc->ToWord();
}

String *HotSpotInterpreter::GetProfileName(ConcreteCode *cc) {
  Transform *tr = reinterpret_cast<HotSpotCode*>(cc)->GetAbstractRepresentation();
  return AbstractCodeInterpreter::MakeProfileName(TagVal::FromWord(tr->GetArgument()));
}

#endif