//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "alice/AbstractCodeInterpreter.hh"
#pragma implementation "alice/AbstractCodeFrame.hh"
#endif

#include <cstdio>
#include "alice/Data.hh"
#include "alice/Types.hh"
#include "alice/AbstractCode.hh"
#include "alice/AbstractCodeFrame.hh"
#include "alice/LazySelInterpreter.hh"
#include "alice/AliceConcreteCode.hh"
#include "alice/AliceLanguageLayer.hh"
#include "alice/AliceDebuggerEvent.hh"
#include "alice/DebugEnvironment.hh"

inline word GetIdRef(word idRef, Closure *globalEnv, 
		     AbstractCodeFrame::Environment *localEnv) {
  TagVal *tagVal = TagVal::FromWordDirect(idRef);
  switch (AbstractCode::GetIdRef(tagVal)) {
  case AbstractCode::Immediate:
    return tagVal->Sel(0);
  case AbstractCode::Local:
  case AbstractCode::LastUseLocal:
    return localEnv->Lookup(tagVal->Sel(0));
  case AbstractCode::Global:
    return globalEnv->Sub(Store::WordToInt(tagVal->Sel(0)));
  default:
    Error("AbstractCodeInterpreter::GetIdRef: invalid idRef tag");
  }
}

inline void KillIdRef(word idRef, TagVal *pc,
		      Closure *globalEnv, 
		      AbstractCodeFrame::Environment *localEnv) {
  TagVal *tagVal = TagVal::FromWordDirect(idRef);
  if (AbstractCode::GetIdRef(tagVal) == AbstractCode::LastUseLocal)
    localEnv->Kill(tagVal->Sel(0), pc, globalEnv);
}

#if DEBUGGER
//
// Debugging Support
//
// Debugger event generation

#define APP_LABEL     0
#define CON_APP_LABEL APP_LABEL
#define COND_LABEL    1
#define HANDLE_LABEL  2
#define RAISE_LABEL   3
#define SEL_LABEL     COND_LABEL
#define STRICT_LABEL  COND_LABEL
#define SPAWN_LABEL   4

#define SUSPEND() {                                     \
  PushState(pc, globalEnv, localEnv);                   \
  Scheduler::nArgs = 0;                                 \
  return Worker::SUSPEND;                               \
}

static word IdRefVecToValueVec(word wIdRef, 
			       AbstractCodeFrame::Environment *localEnv,
			       Closure *globalEnv) {
  Vector *idRefs = Vector::FromWordDirect(wIdRef);
  Vector *values = Vector::New(idRefs->GetLength());
  for (int index = idRefs->GetLength(); index--; ) {
    values->Init(index, GetIdRef(idRefs->Sub(index), globalEnv, localEnv));
  }
  return values->ToWord();
}

static word GenerateEntryEvent(word coord, word stepPoint, 
			       AbstractCodeFrame::Environment *localEnv, 
			       Closure *globalEnv) {
  TagVal *entryEvent;
  Thread *thread = Scheduler::GetCurrentThread();
  // Check wheter we have a breakpoint
  if (Debugger::IsBreakpoint(thread)) {
    entryEvent = TagVal::New(1,3);
  } else {
    entryEvent = TagVal::New(2,3);
  }
  // Entry Event generation
  entryEvent->Init(0, Scheduler::GetCurrentThread()->ToWord());
  entryEvent->Init(1, coord);
  TagVal *entryPoint = TagVal::FromWord(stepPoint);
  if(entryPoint == INVALID_POINTER) {
    // spawn 
    entryEvent->Init(2, Store::IntToWord(SPAWN_LABEL));
    AliceDebuggerEvent *event = AliceDebuggerEvent::New(entryEvent->ToWord());
    return event->ToWord();
  }
  TagVal *stepPointCon;
  switch (AbstractCode::GetEntryPoint(entryPoint)) {
  case AbstractCode::AppEntry:
    {
      stepPointCon = TagVal::New(APP_LABEL, 3);
      stepPointCon->Init(0, GetIdRef(entryPoint->Sel(1), globalEnv, localEnv));
      stepPointCon->Init(1, entryPoint->Sel(0));
      stepPointCon->Init(2, IdRefVecToValueVec(entryPoint->Sel(2), 
					       localEnv, globalEnv));
      break;
    }
  case AbstractCode::ConEntry:
    {
      stepPointCon = TagVal::New(CON_APP_LABEL, 3);
      stepPointCon->Init(0, GetIdRef(entryPoint->Sel(1), globalEnv, localEnv));
      stepPointCon->Init(1, entryPoint->Sel(0));
      stepPointCon->Init(2, IdRefVecToValueVec(entryPoint->Sel(2), 
						 localEnv, globalEnv));
      break;
    }
  case AbstractCode::CondEntry:
    {
      stepPointCon = TagVal::New(COND_LABEL, 2);
      stepPointCon->Init(0, GetIdRef(entryPoint->Sel(1), globalEnv, localEnv));
      stepPointCon->Init(1, entryPoint->Sel(0));
      break;
    }
  case AbstractCode::HandleEntry:
    {
      stepPointCon = TagVal::New(HANDLE_LABEL, 1);
      stepPointCon->Init(0, GetIdRef(entryPoint->Sel(0), globalEnv, localEnv));
      break;
    }
  case AbstractCode::RaiseEntry:
    {
      stepPointCon = TagVal::New(RAISE_LABEL, 1);
      stepPointCon->Init(0, GetIdRef(entryPoint->Sel(0), globalEnv, localEnv));
      break;
    }
  case AbstractCode::SelEntry:
    {
      stepPointCon = TagVal::New(SEL_LABEL, 2);
      stepPointCon->Init(0, GetIdRef(entryPoint->Sel(2), globalEnv, localEnv));
      stepPointCon->Init(1, entryPoint->Sel(1));
      break;
    }
  case AbstractCode::StrictEntry:
    {
      stepPointCon = TagVal::New(STRICT_LABEL, 2);
      stepPointCon->Init(0, GetIdRef(entryPoint->Sel(1), globalEnv, localEnv));
      stepPointCon->Init(1, entryPoint->Sel(0));
      break;
    }
  default:
    {
      Error("GenerateEntryEvent: illegal tag");
    }
  }
  entryEvent->Init(2, stepPointCon->ToWord());
  AliceDebuggerEvent *event = AliceDebuggerEvent::New(entryEvent->ToWord());
  return event->ToWord();
}

word GenerateExitEvent(word coord, word result, word stepPoint) {
  TagVal *exitPoint = TagVal::FromWord(stepPoint);
  TagVal *exitEvent = TagVal::New(3, 4);
  exitEvent->Init(0, Scheduler::GetCurrentThread()->ToWord());
  exitEvent->Init(1, coord);
  exitEvent->Init(2, result);
  if (exitPoint == INVALID_POINTER) {
    exitEvent->Init(3, Store::IntToWord(Types::NONE));
    AliceDebuggerEvent *event = AliceDebuggerEvent::New(exitEvent->ToWord());   
    return event->ToWord();
  }
  switch (AbstractCode::GetExitPoint(exitPoint)) {
  case AbstractCode::SelExit:
    {
      TagVal *some = TagVal::New(Types::SOME, 1);
      some->Init(0, exitPoint->Sel(0));
      exitEvent->Init(3, some->ToWord());
      break;
    }
  case AbstractCode::CondExit:
    {
      TagVal *some = TagVal::New(Types::SOME, 1);
      some->Init(0, exitPoint->Sel(0));
      exitEvent->Init(3, some->ToWord());
      break;
    }
  case AbstractCode::RaiseExit:
    {
      TagVal *some = TagVal::New(Types::SOME, 1);
      some->Init(0, exitPoint->Sel(0));
      exitEvent->Init(3, some->ToWord());
      break;
    }
  case AbstractCode::HandleExit:
    {
      TagVal *some = TagVal::New(Types::SOME, 1);
      some->Init(0, exitPoint->Sel(0));
      exitEvent->Init(3, some->ToWord());
      break;
    }
  case AbstractCode::SpawnExit:
    {
      TagVal *some = TagVal::New(Types::SOME, 1);
      some->Init(0, exitPoint->Sel(0));
      exitEvent->Init(3, some->ToWord());
      break;
    }
  default:
    {
      exitEvent->Init(3, Store::IntToWord(Types::NONE));
    }
  }
  AliceDebuggerEvent *event = AliceDebuggerEvent::New(exitEvent->ToWord());
  return event->ToWord();
}

//
// End of Debugging Support
//
#endif

// Interpreter Helper
inline void PushState(TagVal *pc, Closure *globalEnv, 
		      AbstractCodeFrame::Environment *localEnv,
		      Vector *formalArgs) {
  AbstractCodeFrame::New(AbstractCodeInterpreter::self, pc->ToWord(),
			 globalEnv, localEnv, formalArgs->ToWord());
}

inline void PushState(TagVal *pc, Closure *globalEnv, 
		      AbstractCodeFrame::Environment *localEnv) {
  AbstractCodeFrame::New(AbstractCodeInterpreter::self, pc->ToWord(),
			 globalEnv, localEnv, Store::IntToWord(0));
}

inline word
GetIdRefKill(word idRef, TagVal *pc,
	     Closure *globalEnv, AbstractCodeFrame::Environment *localEnv) {
  TagVal *tagVal = TagVal::FromWordDirect(idRef);
  switch (AbstractCode::GetIdRef(tagVal)) {
  case AbstractCode::Immediate:
    return tagVal->Sel(0);
  case AbstractCode::Local:
    return localEnv->Lookup(tagVal->Sel(0));
  case AbstractCode::LastUseLocal:
    {
      word id = tagVal->Sel(0);
      word value = localEnv->Lookup(id);
      localEnv->Kill(id, pc, globalEnv);
      return value;
    }
  case AbstractCode::Global:
    return globalEnv->Sub(Store::WordToInt(tagVal->Sel(0)));
  default:
    Error("AbstractCodeInterpreter::GetIdRef: invalid idRef tag");
  }
}

//
// Interpreter Functions
//
AbstractCodeInterpreter *AbstractCodeInterpreter::self;

void AbstractCodeInterpreter::Init() {
  self = new AbstractCodeInterpreter();
}

Transform *
AbstractCodeInterpreter::GetAbstractRepresentation(ConcreteRepresentation *b) {
  return STATIC_CAST(AliceConcreteCode *, b)->GetAbstractRepresentation();
}

void AbstractCodeInterpreter::PushCall(Closure *closure) {
  AliceConcreteCode *concreteCode =
    AliceConcreteCode::FromWord(closure->GetConcreteCode());
  // Function of coord * int * int * idDef vector *
  //   outArity option * instr * liveness
  TagVal *abstractCode = concreteCode->GetAbstractCode();
  switch (AbstractCode::GetAbstractCode(abstractCode)) {
  case AbstractCode::Function:
    {
      abstractCode->AssertWidth(AbstractCode::functionWidth);
      // get number of local variables from abstractCode
      TagVal *annotation = TagVal::FromWordDirect(abstractCode->Sel(2));
      u_int nLocals;
      switch (AbstractCode::GetAnnotation(annotation)) {
      case AbstractCode::Simple:
	{
	  nLocals = Store::DirectWordToInt(annotation->Sel(0));
	}
	break;
      case AbstractCode::Debug:
	{
	  Vector *localNamesTypOpt = 
	    Vector::FromWordDirect(annotation->Sel(0));
	  nLocals = localNamesTypOpt->GetLength();
	}
	break;
      default:
	Assert(false);
      }
      AbstractCodeFrame::New(AbstractCodeInterpreter::self,
			     abstractCode->Sel(5), closure,
			     AbstractCodeFrame::Environment::New(nLocals),
			     abstractCode->Sel(3));
    }
    break;
  default:
    Error("AbstractCodeInterpreter::PushCall: invalid abstractCode tag");
  }
}

#undef REQUEST
#define REQUEST(w) {				\
  Scheduler::PopFrame(frame->GetSize());        \
  PushState(pc, globalEnv, localEnv);		\
  Scheduler::currentData = w;			\
  Scheduler::nArgs = 0;				\
  return Worker::REQUEST;			\
}

#define CHECK_PREEMPT() {			\
  Scheduler::PopFrame(frame->GetSize());        \
  if (StatusWord::GetStatus() != 0)		\
    return Worker::PREEMPT;			\
  else						\
    return Worker::CONTINUE;			\
}

u_int AbstractCodeInterpreter::GetFrameSize(StackFrame *sFrame) {
  AbstractCodeFrame *frame = STATIC_CAST(AbstractCodeFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result AbstractCodeInterpreter::Run(StackFrame *sFrame) {
  AbstractCodeFrame *frame = STATIC_CAST(AbstractCodeFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  TagVal *pc = frame->GetPC();
  Closure *globalEnv = frame->GetClosure();
  AbstractCodeFrame::Environment *localEnv = frame->GetLocalEnv();
  Vector *formalArgs = frame->GetFormalArgs();
  if (formalArgs != INVALID_POINTER) {
    // Calling convention conversion
    u_int nArgs = formalArgs->GetLength();
    switch (nArgs) {
    case 0:
      break;
    case 1:
      {
	Construct();
	TagVal *idDef = TagVal::FromWord(formalArgs->Sub(0));
	if (idDef != INVALID_POINTER) // IdDef id
	  localEnv->Add(idDef->Sel(0), Scheduler::currentArgs[0]);
      }
      break;
    default:
      {
	Vector *formalIdDefs = formalArgs;
	if (nArgs == 0) {
	  if (Scheduler::nArgs != 0) {
	    Assert(Scheduler::nArgs == 1);
	    word requestWord = Scheduler::currentArgs[0];
	    if (Store::WordToInt(requestWord) == INVALID_INT)
	      REQUEST(requestWord);
	    Assert(Store::WordToInt(requestWord) == 0); // unit
	  }
	} else {
	  if (Worker::Deconstruct()) {
	    // Scheduler::currentData has been set by Worker::Deconstruct
	    return Worker::REQUEST;
	  }
	  Assert(Scheduler::nArgs == nArgs);
	  for (u_int i = nArgs; i--; ) {
	    TagVal *idDef = TagVal::FromWord(formalIdDefs->Sub(i));
	    if (idDef != INVALID_POINTER) // IdDef id
	      localEnv->Add(idDef->Sel(0), Scheduler::currentArgs[i]);
	  }
	}
      }
      break;
    }
  }
  // Execution
  while (true) {
  loop:
    switch (AbstractCode::GetInstr(pc)) {
    case AbstractCode::Entry: // of coord * entry_point * instr
      {
#if DEBUGGER
	// DebugFrame Generation
	word event =
	  GenerateEntryEvent(pc->Sel(0), pc->Sel(1), localEnv, globalEnv);
	// TODO: Introduce PushUnder on Stack
// 	Assert(frame == (StackFrame *) Scheduler::stackTop);
 	Scheduler::PopFrame(frame->GetSize());
	DebugWorker::PushFrame(event);
 	frame = AbstractCodeFrame::New(self, pc->ToWord(), globalEnv, 
 				       localEnv, formalArgs->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(2));
	if (Scheduler::GetCurrentThread()->GetDebugMode() == Thread::DEBUG) {
	  Debugger::SendEvent(event);
	  SUSPEND();
	}
#else
	pc = TagVal::FromWordDirect(pc->Sel(2));
#endif
      }
      break;
    case AbstractCode::Exit: // of coord * exit_point * idRef * instr
      {
#if DEBUGGER
	// Pop current Frame
	Scheduler::PopFrame(frame->GetSize());
	// Check for and pop DebugFrame
	StackFrame *dFrame = Scheduler::GetFrame();
	Assert(dFrame->GetWorker() == DebugWorker::self);
	DebugFrame *debugFrame = STATIC_CAST(DebugFrame *, dFrame);
	Scheduler::PopFrame(debugFrame->GetSize());
 	frame = AbstractCodeFrame::New(self, pc->ToWord(), globalEnv, 
 				       localEnv, formalArgs->ToWord());
	word coord = pc->Sel(0);
	word stepPoint = pc->Sel(1);
	word idRef = pc->Sel(2);
	pc = TagVal::FromWordDirect(pc->Sel(3));
	if (Scheduler::GetCurrentThread()->GetDebugMode() == Thread::DEBUG) {
	  word idRefRes = GetIdRef(idRef, globalEnv, localEnv);
	  Debugger::SendEvent(GenerateExitEvent(coord, idRefRes, stepPoint));
	  SUSPEND();
	}
#else 
	pc = TagVal::FromWordDirect(pc->Sel(3));
#endif
      }
      break;
    case AbstractCode::Kill: // of id vector * instr
      {
	Vector *kills = Vector::FromWordDirect(pc->Sel(0));
	for (u_int i = kills->GetLength(); i--; )
	  localEnv->Kill(kills->Sub(i), pc, globalEnv);
	pc = TagVal::FromWordDirect(pc->Sel(1));
      }
      break;
    case AbstractCode::PutVar: // of id * idRef  * instr
      {
	localEnv->Add(pc->Sel(0), GetIdRefKill(pc->Sel(1), pc,
					       globalEnv, localEnv));
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::PutNew: // of id * string * instr
      {
	Constructor *constructor =
	  Constructor::New(String::FromWordDirect(pc->Sel(1)));
	localEnv->Add(pc->Sel(0), constructor->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::PutTag: // of id * int * idRef vector * instr
      {
	Vector *idRefs = Vector::FromWordDirect(pc->Sel(2));
	u_int nargs = idRefs->GetLength();
	TagVal *tagVal =
	  TagVal::New(Store::DirectWordToInt(pc->Sel(1)), nargs);
	for (u_int i = nargs; i--; )
	  tagVal->Init(i, GetIdRefKill(idRefs->Sub(i), pc,
				       globalEnv, localEnv));
	localEnv->Add(pc->Sel(0), tagVal->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::PutCon: // of id * idRef * idRef vector * instr
      {
	Vector *idRefs = Vector::FromWordDirect(pc->Sel(2));
	u_int nargs = idRefs->GetLength();
	word requestWord = GetIdRef(pc->Sel(1), globalEnv, localEnv);
	Block *constructor = Store::WordToBlock(requestWord);
	if (constructor == INVALID_POINTER) REQUEST(requestWord);
	KillIdRef(pc->Sel(1), pc, globalEnv, localEnv);
	ConVal *conVal = ConVal::New(constructor, nargs);
	for (u_int i = nargs; i--; )
	  conVal->Init(i, GetIdRefKill(idRefs->Sub(i), pc,
				       globalEnv, localEnv));
	localEnv->Add(pc->Sel(0), conVal->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::PutRef: // of id * idRef * instr
      {
	word contents = GetIdRefKill(pc->Sel(1), pc, globalEnv, localEnv);
	localEnv->Add(pc->Sel(0), Cell::New(contents)->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::PutTup: // of id * idRef vector * instr
      {
	Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
	u_int nargs = idRefs->GetLength();
	if (nargs == 0) {
	  localEnv->Add(pc->Sel(0), Store::IntToWord(0)); // unit
	} else {
	  Tuple *tuple = Tuple::New(nargs);
	  for (u_int i = nargs; i--; )
	    tuple->Init(i, GetIdRefKill(idRefs->Sub(i), pc,
					globalEnv, localEnv));
	  localEnv->Add(pc->Sel(0), tuple->ToWord());
	}
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::PutPolyRec:
      // of id * label vector * idRef vector * instr
      {
	Vector *labels = Vector::FromWordDirect(pc->Sel(1));
	Record *record = Record::New(labels);
	Vector *idRefs = Vector::FromWordDirect(pc->Sel(2));
	Assert(labels->GetLength() == idRefs->GetLength());
	for (u_int i = labels->GetLength(); i--; ) {
	  record->AssertLabel(i, UniqueString::FromWordDirect(labels->Sub(i)));
	  record->Init(i, GetIdRefKill(idRefs->Sub(i), pc,
				       globalEnv, localEnv));
	}
	localEnv->Add(pc->Sel(0), record->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::PutVec: // of id * idRef vector * instr
      {
	Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
	u_int nargs = idRefs->GetLength();
	Vector *vector = Vector::New(nargs);
	for (u_int i = nargs; i--; )
	  vector->Init(i, GetIdRefKill(idRefs->Sub(i), pc,
				       globalEnv, localEnv));
	localEnv->Add(pc->Sel(0), vector->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::Close: // of id * idRef vector * template * instr
      {
	Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
	u_int nGlobals = idRefs->GetLength();
#if PROFILE
	Profiler::IncClosures(pc->Sel(2));
#endif
	// Instantiate the template into an abstract code:
	TagVal *abstractCode =
	  TagVal::New(AbstractCode::Function, AbstractCode::functionWidth);
	TagVal *template_ = TagVal::FromWordDirect(pc->Sel(2));
	template_->AssertWidth(AbstractCode::functionWidth);
	abstractCode->Init(0, template_->Sel(0));
	Assert(STATIC_CAST(u_int, Store::DirectWordToInt(template_->Sel(1))) ==
	       nGlobals);
	Vector *subst = Vector::New(nGlobals);
	for (u_int i = nGlobals; i--; )
	  subst->Init(0, Store::IntToWord(Types::NONE));
	abstractCode->Init(1, subst->ToWord());
	abstractCode->Init(2, template_->Sel(2));
	abstractCode->Init(3, template_->Sel(3));
	abstractCode->Init(4, template_->Sel(4));
	abstractCode->Init(5, template_->Sel(5));
	abstractCode->Init(6, template_->Sel(6));
	// Construct concrete code from abstract code:
	word wConcreteCode =
	  AliceLanguageLayer::concreteCodeConstructor(abstractCode);
#if DEBUGGER
	Closure *closure;
	// check wether abstractCode has debug annotation
	TagVal *annotation = TagVal::FromWordDirect(abstractCode->Sel(2));
	if (AbstractCode::GetAnnotation(annotation) == AbstractCode::Debug) {
	  DebugEnvironment *env = DebugEnvironment::New(localEnv, globalEnv);
	  closure = Closure::New(wConcreteCode, nGlobals + 1);
	  // add link to DebugEnvironment at the last position
	  closure->Init(nGlobals, env->ToWord());
	} else {
	  closure = Closure::New(wConcreteCode, nGlobals);
	} 
#else
	Closure *closure = Closure::New(wConcreteCode, nGlobals);
#endif
	for (u_int i = nGlobals; i--; )
	  closure->Init(i, GetIdRefKill(idRefs->Sub(i), pc,
					globalEnv, localEnv));
	localEnv->Add(pc->Sel(0), closure->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::Specialize: // of id * idRef vector * template * instr
      {
	// Construct new abstract code by instantiating template:
	TagVal *abstractCode =
	  TagVal::New(AbstractCode::Function, AbstractCode::functionWidth);
	TagVal *template_ = TagVal::FromWordDirect(pc->Sel(2));
	template_->AssertWidth(AbstractCode::functionWidth);
	// PROFILE: IncInstances here
	abstractCode->Init(0, template_->Sel(0));
	abstractCode->Init(2, template_->Sel(2));
	abstractCode->Init(3, template_->Sel(3));
	abstractCode->Init(4, template_->Sel(4));
	abstractCode->Init(5, template_->Sel(5));
	abstractCode->Init(6, template_->Sel(6));
	// Construct concrete code from abstract code:
	word wConcreteCode =
	  AliceLanguageLayer::concreteCodeConstructor(abstractCode);
	// Construct closure from concrete code:
	Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
	u_int nGlobals = idRefs->GetLength();
	Assert(STATIC_CAST(u_int, Store::DirectWordToInt(template_->Sel(1))) ==
	       nGlobals);
	Vector *subst = Vector::New(nGlobals);
#if DEBUGGER
	Closure *closure;
	// check wether abstractCode has debug annotation
	TagVal *annotation = TagVal::FromWordDirect(abstractCode->Sel(2));
	if (AbstractCode::GetAnnotation(annotation) == AbstractCode::Debug) {
	  DebugEnvironment *env = DebugEnvironment::New(localEnv, globalEnv);
	  closure = Closure::New(wConcreteCode, nGlobals + 1);
	  // add link to DebugEnvironment at the last position
	  closure->Init(nGlobals, env->ToWord());
	} else {
	  closure = Closure::New(wConcreteCode, nGlobals);
	} 
#else
	Closure *closure = Closure::New(wConcreteCode, nGlobals);
#endif
	for (u_int i = nGlobals; i--; ) {
	  word value = GetIdRefKill(idRefs->Sub(i), pc, globalEnv, localEnv);
	  TagVal *some = TagVal::New(Types::SOME, 1);
	  some->Init(0, value);
	  subst->Init(i, some->ToWord());
	  closure->Init(i, value);
	}
	abstractCode->Init(1, subst->ToWord());
	localEnv->Add(pc->Sel(0), closure->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::AppPrim:
      // of value * idRef vector * (idDef * instr) option
      {
	Scheduler::PopFrame(frame->GetSize());
	TagVal *idDefInstrOpt = TagVal::FromWord(pc->Sel(2));
	if (idDefInstrOpt != INVALID_POINTER) { // SOME (idDef * instr)
	  // Save our state for return
	  Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
	  Vector *formalArgs = Vector::New(1);
	  formalArgs->Init(0, idDefInstr->Sel(0));
	  PushState(TagVal::FromWordDirect(idDefInstr->Sel(1)),
		    globalEnv, localEnv, formalArgs);
	}
	// Push a call frame for the primitive
	Vector *actualIdRefs = Vector::FromWordDirect(pc->Sel(1));
	u_int nArgs = actualIdRefs->GetLength();
	Scheduler::nArgs = nArgs;
	for (u_int i = nArgs; i--; )
	  Scheduler::currentArgs[i] =
	    GetIdRefKill(actualIdRefs->Sub(i), pc, globalEnv, localEnv);
	return Scheduler::PushCall(pc->Sel(0));
      }
      break;
    case AbstractCode::AppVar:
      // of idRef * idRef vector * bool * (idDef vector * instr) option
      {
	Scheduler::PopFrame(frame->GetSize());
	TagVal *idDefsInstrOpt = TagVal::FromWord(pc->Sel(3));
	if (idDefsInstrOpt != INVALID_POINTER) { // SOME ...
	  // Save our state for return
	  Tuple *idDefsInstr = Tuple::FromWordDirect(idDefsInstrOpt->Sel(0));
	  Vector *idDefs = Vector::FromWordDirect(idDefsInstr->Sel(0));
	  PushState(TagVal::FromWordDirect(idDefsInstr->Sel(1)),
		    globalEnv, localEnv, idDefs);
	}
	Vector *actualIdRefs = Vector::FromWordDirect(pc->Sel(1));
	u_int nArgs = actualIdRefs->GetLength();
	Scheduler::nArgs = nArgs;
	for (u_int i = nArgs; i--; )
	  Scheduler::currentArgs[i] =
	    GetIdRefKill(actualIdRefs->Sub(i), pc, globalEnv, localEnv);
	if (StatusWord::GetStatus() != 0) {
	  Worker::Result res =
	    Scheduler::PushCall(GetIdRefKill(pc->Sel(0), pc,
					     globalEnv, localEnv));
	  return res == Worker::CONTINUE? Worker::PREEMPT: res;
	} else {
	  word closure = GetIdRefKill(pc->Sel(0), pc, globalEnv, localEnv);
	  return Scheduler::PushCall(closure);
	}
      }
      break;
    case AbstractCode::GetRef: // of id * idRef * instr
      {
	word requestWord = GetIdRef(pc->Sel(1), globalEnv, localEnv);
	Cell *cell = Cell::FromWord(requestWord);
	if (cell == INVALID_POINTER) REQUEST(requestWord);
	KillIdRef(pc->Sel(1), pc, globalEnv, localEnv);
	localEnv->Add(pc->Sel(0), cell->Access());
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::GetTup: // of idDef vector * idRef * instr
      {
	word requestWord = GetIdRef(pc->Sel(1), globalEnv, localEnv);
	Vector *idDefs = Vector::FromWordDirect(pc->Sel(0));
	u_int nargs = idDefs->GetLength();
	if (nargs == 0) {
	  if (Store::WordToInt(requestWord) == INVALID_INT)
	    REQUEST(requestWord);
	  KillIdRef(pc->Sel(1), pc, globalEnv, localEnv);
	} else {
	  Tuple *tuple = Tuple::FromWord(requestWord);
	  if (tuple == INVALID_POINTER) REQUEST(requestWord);
	  KillIdRef(pc->Sel(1), pc, globalEnv, localEnv);
	  tuple->AssertWidth(idDefs->GetLength());
	  for (u_int i = nargs; i--; ) {
	    TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
	    if (idDef != INVALID_POINTER) // IdDef id
	      localEnv->Add(idDef->Sel(0), tuple->Sel(i));
	  }
	}
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::Sel: // of id * idRef * int * instr
      {
	word requestWord = GetIdRef(pc->Sel(1), globalEnv, localEnv);
	Tuple *tuple = Tuple::FromWord(requestWord);
	if (tuple == INVALID_POINTER) REQUEST(requestWord);
	KillIdRef(pc->Sel(1), pc, globalEnv, localEnv);
	localEnv->Add(pc->Sel(0),
		      tuple->Sel(Store::DirectWordToInt(pc->Sel(2))));
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::LazyPolySel:
      // of id vector * idRef * label vector * instr
      {
	word wRecord = GetIdRefKill(pc->Sel(1), pc, globalEnv, localEnv);
	Record *record = Record::FromWord(wRecord);
	Vector *ids = Vector::FromWordDirect(pc->Sel(0));
	Vector *labels = Vector::FromWordDirect(pc->Sel(2));
	Assert(ids->GetLength() == labels->GetLength());
	if (record == INVALID_POINTER) {
	  u_int n = ids->GetLength();
	  for (u_int i = n; i--; ) {
	    UniqueString *label = UniqueString::FromWordDirect(labels->Sub(i));
	    LazySelClosure *closure = LazySelClosure::New(wRecord, label);
	    Byneed *byneed = Byneed::New(closure->ToWord());
	    localEnv->Add(ids->Sub(i), byneed->ToWord());
	  }
	} else
	  for (u_int i = ids->GetLength(); i--; ) {
	    UniqueString *label = UniqueString::FromWordDirect(labels->Sub(i));
	    localEnv->Add(ids->Sub(i), record->PolySel(label));
	  }
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::Raise: // of idRef
      {
	word requestWord = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	Transient *transient = Store::WordToTransient(requestWord);
	if (transient != INVALID_POINTER) REQUEST(transient->ToWord());
	KillIdRef(pc->Sel(0), pc, globalEnv, localEnv);
	Scheduler::currentData = requestWord;
	Scheduler::currentBacktrace = Backtrace::New(frame->Clone());
	return Worker::RAISE;
      }
      break;
    case AbstractCode::Reraise: // of idRef
      {
	Tuple *package =
	  Tuple::FromWordDirect(GetIdRefKill(pc->Sel(0), pc,
					     globalEnv, localEnv));
	package->AssertWidth(2);
	Scheduler::currentData = package->Sel(0);
	Scheduler::currentBacktrace =
	  Backtrace::FromWordDirect(package->Sel(1));
	return Worker::RAISE;
      }
    case AbstractCode::Try: // of instr * idDef * idDef * instr
      {
	// Push a handler stack frame:
	Vector *formalIdDefs = Vector::New(2);
	formalIdDefs->Init(0, pc->Sel(1));
	formalIdDefs->Init(1, pc->Sel(2));
	Tuple *exnData = Tuple::New(2);
	exnData->Init(0, pc->Sel(3));
	exnData->Init(1, formalIdDefs->ToWord());
	Scheduler::PushHandler(exnData->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(0));
      }
      break;
    case AbstractCode::EndTry: // of instr
      {
	Scheduler::PopHandler();
	pc = TagVal::FromWordDirect(pc->Sel(0));
      }
      break;
    case AbstractCode::EndHandle: // of instr
      {
	pc = TagVal::FromWordDirect(pc->Sel(0));
      }
      break;
    case AbstractCode::IntTest: // of idRef * (int * instr) vector * instr
      {
	word requestWord = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	s_int value = Store::WordToInt(requestWord);
	if (value == INVALID_INT) REQUEST(requestWord);
	KillIdRef(pc->Sel(0), pc, globalEnv, localEnv);
	Vector *tests = Vector::FromWordDirect(pc->Sel(1));
	u_int ntests = tests->GetLength();
	for (u_int i = 0; i < ntests; i++) {
	  Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
	  if (Store::DirectWordToInt(pair->Sel(0)) == value) {
	    pc = TagVal::FromWordDirect(pair->Sel(1));
	    goto loop;
	  }
	}
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::CompactIntTest: // of idRef * int * instrs * instr
      {
	word requestWord = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	s_int value = Store::WordToInt(requestWord);
	if (value == INVALID_INT) REQUEST(requestWord);
	KillIdRef(pc->Sel(0), pc, globalEnv, localEnv);
	s_int offset = Store::DirectWordToInt(pc->Sel(1));
	u_int index = STATIC_CAST(u_int, value - offset);
	Vector *tests = Vector::FromWordDirect(pc->Sel(2));
	if (index < tests->GetLength())
	  pc = TagVal::FromWordDirect(tests->Sub(index));
	else
	  pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::RealTest: // of idRef * (real * instr) vector * instr
      {
	word requestWord = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	Real *real = Real::FromWord(requestWord);
	if (real == INVALID_POINTER) REQUEST(requestWord);
	KillIdRef(pc->Sel(0), pc, globalEnv, localEnv);
	double value = real->GetValue();
	Vector *tests = Vector::FromWordDirect(pc->Sel(1));
	u_int ntests = tests->GetLength();
	for (u_int i = 0; i < ntests; i++) {
	  Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
	  if (Real::FromWordDirect(pair->Sel(0))->GetValue() == value) {
	    pc = TagVal::FromWordDirect(pair->Sel(1));
	    goto loop;
	  }
	}
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::StringTest:
      // of idRef * (string * instr) vector * instr
      {
	word requestWord = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	String *string = String::FromWord(requestWord);
	if (string == INVALID_POINTER) REQUEST(requestWord);
	KillIdRef(pc->Sel(0), pc, globalEnv, localEnv);
	const u_char *value = string->GetValue();
	u_int length = string->GetSize();
	Vector *tests = Vector::FromWordDirect(pc->Sel(1));
	u_int ntests = tests->GetLength();
	for (u_int i = 0; i < ntests; i++) {
	  Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
	  string = String::FromWordDirect(pair->Sel(0));
	  if (string->GetSize() == length &&
	      !std::memcmp(string->GetValue(), value, length)) {
	    pc = TagVal::FromWordDirect(pair->Sel(1));
	    goto loop;
	  }
	}
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::TagTest:
      // of idRef * (int * instr) vector
      //          * (int * idDef vector * instr) vector * instr
      {
	word requestWord = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	TagVal *tagVal = TagVal::FromWord(requestWord);
	if (tagVal == INVALID_POINTER) { // nullary constructor or transient
	  s_int tag = Store::WordToInt(requestWord);
	  if (tag == INVALID_INT) REQUEST(requestWord);
	  KillIdRef(pc->Sel(0), pc, globalEnv, localEnv);
	  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
	  u_int ntests = tests->GetLength();
	  for (u_int i = 0; i < ntests; i++) {
	    Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
	    if (Store::DirectWordToInt(pair->Sel(0)) == tag) {
	      pc = TagVal::FromWordDirect(pair->Sel(1));
	      goto loop;
	    }
	  }
	} else { // non-nullary constructor
	  KillIdRef(pc->Sel(0), pc, globalEnv, localEnv);
	  s_int tag = tagVal->GetTag();
	  Vector *tests = Vector::FromWordDirect(pc->Sel(2));
	  u_int ntests = tests->GetLength();
	  for (u_int i = 0; i < ntests; i++) {
	    Tuple *triple = Tuple::FromWordDirect(tests->Sub(i));
	    if (Store::DirectWordToInt(triple->Sel(0)) == tag) {
	      Vector *idDefs = Vector::FromWordDirect(triple->Sel(1));
	      tagVal->AssertWidth(idDefs->GetLength());
	      for (u_int i = idDefs->GetLength(); i--; ) {
		TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
		if (idDef != INVALID_POINTER) // IdDef id
		  localEnv->Add(idDef->Sel(0), tagVal->Sel(i));
	      }
	      pc = TagVal::FromWordDirect(triple->Sel(2));
	      goto loop;
	    }
	  }
	}
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::CompactTagTest: // of idRef * tagTests * instr option
      {
	word requestWord = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	TagVal *tagVal = TagVal::FromWord(requestWord);
	if (tagVal == INVALID_POINTER) { // nullary constructor or transient
	  int tag = Store::WordToInt(requestWord);
	  if (tag == INVALID_INT) REQUEST(requestWord);
	  KillIdRef(pc->Sel(0), pc, globalEnv, localEnv);
	  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
	  if (STATIC_CAST(u_int, tag) < tests->GetLength()) {
	    Tuple *tuple = Tuple::FromWordDirect(tests->Sub(tag));
	    Assert(tuple->Sel(0) == Store::IntToWord(Types::NONE));
	    pc = TagVal::FromWordDirect(tuple->Sel(1));
	    goto loop;
	  }
	} else { // non-nullary constructor
	  KillIdRef(pc->Sel(0), pc, globalEnv, localEnv);
	  int tag = tagVal->GetTag();
	  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
	  if (STATIC_CAST(u_int, tag) < tests->GetLength()) {
	    Tuple *tuple = Tuple::FromWordDirect(tests->Sub(tag));
	    TagVal *idDefsOpt = TagVal::FromWordDirect(tuple->Sel(0));
	    Vector *idDefs = Vector::FromWordDirect(idDefsOpt->Sel(0));
	    tagVal->AssertWidth(idDefs->GetLength());
	    for (u_int i = idDefs->GetLength(); i--; ) {
	      TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
	      if (idDef != INVALID_POINTER) // IdDef id
		localEnv->Add(idDef->Sel(0), tagVal->Sel(i));
	    }
	    pc = TagVal::FromWordDirect(tuple->Sel(1));
	    goto loop;
	  }
	}
	TagVal *someElseInstr = TagVal::FromWordDirect(pc->Sel(2));
	pc = TagVal::FromWordDirect(someElseInstr->Sel(0));
      }
      break;
    case AbstractCode::ConTest:
      // of idRef * (idRef * instr) vector
      //          * (idRef * idDef vector * instr) vector * instr
      {
	word requestWord = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	ConVal *conVal = ConVal::FromWord(requestWord);
	if (conVal == INVALID_POINTER) REQUEST(requestWord);
	if (conVal->IsConVal()) { // non-nullary constructor
	  Block *constructor = conVal->GetConstructor();
	  Vector *tests = Vector::FromWordDirect(pc->Sel(2));
	  u_int ntests = tests->GetLength();
	  for (u_int i = 0; i < ntests; i++) {
	    Tuple *triple = Tuple::FromWordDirect(tests->Sub(i));
	    requestWord = GetIdRef(triple->Sel(0), globalEnv, localEnv);
	    Block *testConstructor = Store::WordToBlock(requestWord);
	    if (testConstructor == INVALID_POINTER) REQUEST(requestWord);
	    if (testConstructor == constructor) {
	      Vector *idDefs = Vector::FromWordDirect(triple->Sel(1));
	      conVal->AssertWidth(idDefs->GetLength());
	      for (u_int i = idDefs->GetLength(); i--; ) {
		TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
		if (idDef != INVALID_POINTER) // IdDef id
		  localEnv->Add(idDef->Sel(0), conVal->Sel(i));
	      }
	      KillIdRef(pc->Sel(0), pc,
			globalEnv, localEnv); //--** some kills missing
	      pc = TagVal::FromWordDirect(triple->Sel(2));
	      goto loop;
	    }
	  }
	} else { // nullary constructor
	  Block *constructor = Store::DirectWordToBlock(conVal->ToWord());
	  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
	  u_int ntests = tests->GetLength();
	  for (u_int i = 0; i < ntests; i++) {
	    Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
	    requestWord = GetIdRef(pair->Sel(0), globalEnv, localEnv);
	    Block *testConstructor = Store::WordToBlock(requestWord);
	    if (testConstructor == INVALID_POINTER) REQUEST(requestWord);
	    if (testConstructor == constructor) {
	      KillIdRef(pc->Sel(0), pc,
			globalEnv, localEnv); //--** some kills missing
	      pc = TagVal::FromWordDirect(pair->Sel(1));
	      goto loop;
	    }
	  }
	}
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::VecTest:
      // of idRef * (idDef vector * instr) vector * instr
      {
	word requestWord = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	Vector *vector = Vector::FromWord(requestWord);
	if (vector == INVALID_POINTER) REQUEST(requestWord);
	KillIdRef(pc->Sel(0), pc, globalEnv, localEnv);
	u_int value = vector->GetLength();
	Vector *tests = Vector::FromWordDirect(pc->Sel(1));
	u_int ntests = tests->GetLength();
	for (u_int i = 0; i < ntests; i++) {
	  Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
	  Vector *idDefs = Vector::FromWordDirect(pair->Sel(0));
	  if (idDefs->GetLength() == value) {
	    for (u_int i = value; i--; ) {
	      TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
	      if (idDef != INVALID_POINTER) // IdDef id
		localEnv->Add(idDef->Sel(0), vector->Sub(i));
	    }
	    pc = TagVal::FromWordDirect(pair->Sel(1));
	    goto loop;
	  }
	}
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::Shared: // of stamp * instr
      {
	pc = TagVal::FromWordDirect(pc->Sel(1));
      }
      break;
    case AbstractCode::Return: // of idRef vector
      {
	Vector *returnIdRefs = Vector::FromWordDirect(pc->Sel(0));
	u_int nArgs = returnIdRefs->GetLength();
	Assert(nArgs <= Scheduler::maxArgs);
	Scheduler::nArgs = nArgs;
	for (u_int i = nArgs; i--; )
	  Scheduler::currentArgs[i] =
	    GetIdRefKill(returnIdRefs->Sub(i), pc, globalEnv, localEnv);
	CHECK_PREEMPT();
      }
      break;
    default:
      Error("AbstractCodeInterpreter::Run: unknown instr tag");
    }
  }
}

Worker::Result AbstractCodeInterpreter::Handle(word data) {
  StackFrame *sFrame = Scheduler::GetFrame();
  Assert(sFrame->GetWorker() == this);
  AbstractCodeFrame *frame = STATIC_CAST(AbstractCodeFrame *, sFrame);
  Tuple *package = Tuple::New(2);
  word exn = Scheduler::currentData;
  package->Init(0, exn);
  package->Init(1, Scheduler::currentBacktrace->ToWord());
  Scheduler::nArgs = 2;
  Scheduler::currentArgs[0] = package->ToWord();
  Scheduler::currentArgs[1] = exn;
  Tuple *exnData = Tuple::FromWordDirect(data);
  frame->SetPC(TagVal::FromWordDirect(exnData->Sel(0)));
  frame->SetFormalArgs(exnData->Sel(1));
  return Worker::CONTINUE;
}

u_int AbstractCodeInterpreter::GetInArity(ConcreteCode *concreteCode) {
  Assert(concreteCode->GetInterpreter() == AbstractCodeInterpreter::self);
  AliceConcreteCode *aliceConcreteCode =
    STATIC_CAST(AliceConcreteCode *, concreteCode);
  TagVal *abstractCode = aliceConcreteCode->GetAbstractCode();
  Vector *idDefs = Vector::FromWordDirect(abstractCode->Sel(3));
  u_int nArgs = idDefs->GetLength();
  return nArgs;
}

u_int AbstractCodeInterpreter::GetOutArity(ConcreteCode *concreteCode) {
  Assert(concreteCode->GetInterpreter() == AbstractCodeInterpreter::self);
  AliceConcreteCode *aliceConcreteCode =
    STATIC_CAST(AliceConcreteCode *, concreteCode);
  TagVal *abstractCode = aliceConcreteCode->GetAbstractCode();
  TagVal *outArityOpt = TagVal::FromWord(abstractCode->Sel(4));
  return ((outArityOpt == INVALID_POINTER) ? INVALID_INT :
	  Store::DirectWordToInt(outArityOpt->Sel(0)));
}

const char *AbstractCodeInterpreter::Identify() {
  return "AbstractCodeInterpreter";
}

void AbstractCodeInterpreter::DumpFrame(StackFrame *sFrame) {
  AbstractCodeFrame *frame = STATIC_CAST(AbstractCodeFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  Closure *closure = frame->GetClosure();
  AliceConcreteCode *concreteCode =
    AliceConcreteCode::FromWord(closure->GetConcreteCode());
  Assert(concreteCode != INVALID_POINTER);
  TagVal *abstractCode = concreteCode->GetAbstractCode();
  Tuple *coord = Tuple::FromWordDirect(abstractCode->Sel(0));
  String *name = String::FromWordDirect(coord->Sel(0));
  std::fprintf(stderr, "Alice %s %.*s, line %d\n",
	       frame->IsHandlerFrame()? "handler": "function",
	       (int) name->GetSize(), name->GetValue(),
	       Store::DirectWordToInt(coord->Sel(1)));
}

#if PROFILE
word AbstractCodeInterpreter::GetProfileKey(StackFrame *sFrame) {
  AbstractCodeFrame *frame = STATIC_CAST(AbstractCodeFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  ConcreteCode *concreteCode =
    ConcreteCode::FromWord(frame->GetClosure()->GetConcreteCode());
  return concreteCode->ToWord();
}

word AbstractCodeInterpreter::GetProfileKey(ConcreteCode *concreteCode) {
  return concreteCode->ToWord();
}

static String *
MakeProfileName(AliceConcreteCode *concreteCode, const char *type) {
  Assert(concreteCode != INVALID_POINTER);
  TagVal *abstractCode = concreteCode->GetAbstractCode();
  Tuple *coord = Tuple::FromWordDirect(abstractCode->Sel(0));
  String *name = String::FromWordDirect(coord->Sel(0));
  char buf[1024]; // to be done
  std::sprintf(buf, "Alice %s %.*s, line %d, column %d",
	       type, (int) name->GetSize(), name->GetValue(),
	       Store::DirectWordToInt(coord->Sel(1)),
	       Store::DirectWordToInt(coord->Sel(2)));
  return String::New(buf);
}

String *AbstractCodeInterpreter::GetProfileName(StackFrame *sFrame) {
  AbstractCodeFrame *frame = STATIC_CAST(AbstractCodeFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  Closure *closure = frame->GetClosure();
  AliceConcreteCode *concreteCode =
    AliceConcreteCode::FromWord(closure->GetConcreteCode());
  return MakeProfileName(concreteCode,
			 frame->IsHandlerFrame()? "handler" : "function");
}

String *AbstractCodeInterpreter::GetProfileName(ConcreteCode *concreteCode) {
  return MakeProfileName(STATIC_CAST(AliceConcreteCode *, concreteCode),
			 "function");
}
#endif
