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
#endif

#include <cstdio>
#include <sstream>
#include "Seam.hh"
#include "alice/Data.hh"
#include "alice/Types.hh"
#include "alice/AbstractCode.hh"
#include "alice/AbstractCodeFrame.hh"
#include "alice/LazySelInterpreter.hh"
#include "alice/AliceConcreteCode.hh"
#include "alice/AliceLanguageLayer.hh"
#include "alice/AliceDebuggerEvent.hh"
#include "alice/ByteConcreteCode.hh"
#include "alice/ByteCodeInterpreter.hh"
#include "alice/HotSpotConcreteCode.hh"
#include "alice/DebugEnvironment.hh"


static word GetIdRef(word idRef, AbstractCodeFrame *frame, Closure *globalEnv) {
  TagVal *tagVal = TagVal::FromWordDirect(idRef);
  switch (AbstractCode::GetIdRef(tagVal)) {
  case AbstractCode::Immediate:
    return tagVal->Sel(0);
  case AbstractCode::Local:
  case AbstractCode::LastUseLocal:
    return frame->GetLocal(tagVal->Sel(0));
  case AbstractCode::Global:
    return globalEnv->Sub(Store::WordToInt(tagVal->Sel(0)));
  default:
    Error("AbstractCodeInterpreter::GetIdRef: invalid idRef tag");
  }
}


static void KillIdRef(word idRef, AbstractCodeFrame *frame, TagVal *pc, Closure *globalEnv) {
  TagVal *tagVal = TagVal::FromWordDirect(idRef);
  if (AbstractCode::GetIdRef(tagVal) == AbstractCode::LastUseLocal)
    frame->KillLocal(tagVal->Sel(0), pc, globalEnv);
}


word AbstractCodeInterpreter::GetCloseConcreteCode(word parentConcreteCode, TagVal *closeInstr) {
  ConcreteCode *cc = ConcreteCode::FromWordDirect(parentConcreteCode);
  Interpreter *interpreter = cc->GetInterpreter();
  word wCloseInstr = closeInstr->ToWord();
  
  TagVal *abstractCode;
  Map *closeConcreteCodes;
  if (interpreter == AbstractCodeInterpreter::self) {
    AliceConcreteCode *acc = reinterpret_cast<AliceConcreteCode*>(cc);
    abstractCode = acc->GetAbstractCode();
    closeConcreteCodes = acc->GetCloseConcreteCodes();
  }
  else if (interpreter == ByteCodeInterpreter::self) {
    ByteConcreteCode *bcc = reinterpret_cast<ByteConcreteCode*>(cc);
    abstractCode = bcc->GetAbstractCode();
    closeConcreteCodes = bcc->GetCloseConcreteCodes();
  }
  else if (interpreter == HotSpotInterpreter::self) {
    HotSpotConcreteCode *hsc = reinterpret_cast<HotSpotConcreteCode*>(cc);
    AliceConcreteCode *acc = AliceConcreteCode::FromWordDirect(hsc->GetCode());
    abstractCode = acc->GetAbstractCode();
    closeConcreteCodes = acc->GetCloseConcreteCodes();
  }
  else {
    Error("invalid interpreter");
  }
  
  word wCCC = closeConcreteCodes->CondGet(wCloseInstr);
  if (wCCC != INVALID_POINTER) {
    return wCCC;
  }
  else {
    Vector *parentSubst = Vector::FromWordDirect(abstractCode->Sel(1));
    Vector *idRefs = Vector::FromWordDirect(closeInstr->Sel(1));
    u_int nGlobals = idRefs->GetLength();
  
    // Inherit substitution
    Vector *subst = Vector::New(nGlobals);
    for (u_int i = nGlobals; i--; ) {
      TagVal *idRef = TagVal::FromWord(idRefs->Sub(i));
      if (AbstractCode::GetIdRef(idRef) == AbstractCode::Global) {
	TagVal *s = TagVal::FromWord(parentSubst->Sub(Store::DirectWordToInt(idRef->Sel(0))));
	if (s != INVALID_POINTER) {
	  subst->Init(i, s->ToWord());
	  continue;
	}
      }
      subst->Init(i, Store::IntToWord(Types::NONE));
    }
    
    TagVal *template_ = TagVal::FromWordDirect(closeInstr->Sel(2));
    template_->AssertWidth(AbstractCode::functionWidth);
    TagVal *abstractCode =
      TagVal::New(AbstractCode::Function, AbstractCode::functionWidth);
    abstractCode->Init(0, template_->Sel(0));
    abstractCode->Init(1, subst->ToWord());
    abstractCode->Init(2, template_->Sel(2));
    abstractCode->Init(3, template_->Sel(3));
    abstractCode->Init(4, template_->Sel(4));
    abstractCode->Init(5, template_->Sel(5));
    abstractCode->Init(6, template_->Sel(6));
    
    word wConcreteCode = AliceLanguageLayer::concreteCodeConstructor(abstractCode);
    closeConcreteCodes->Put(wCloseInstr, wConcreteCode);
    return wConcreteCode;
  }
}


#if DEBUGGER
#define SUSPEND() {                                     \
  frame->SetPC(pc);					\
  frame->SetCoord(coord);				\
  frame->SetFormalArgs(Vector::New(0));			\
  Scheduler::SetNArgs(0);                               \
  return Worker::SUSPEND;                               \
}

//
// Debugging Support
//
// Debugger event generation
//

enum {APP_LABEL, COND_LABEL, CON_LABEL, HANDLE_LABEL, RAISE_LABEL,
      SEL_LABEL, SPAWN_LABEL, STRICT_LABEL};

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
    // spawn (0-ary Constructor) 
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
      stepPointCon = TagVal::New(CON_LABEL, 3);
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
      stepPointCon = TagVal::New(SEL_LABEL, 3);
      stepPointCon->Init(0, entryPoint->Sel(0));
      stepPointCon->Init(1, GetIdRef(entryPoint->Sel(2), globalEnv, localEnv));
      stepPointCon->Init(2, entryPoint->Sel(1));
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

static word GetIdRefKill(word idRef, AbstractCodeFrame *frame, TagVal *pc, Closure *globalEnv) {
  word value = GetIdRef(idRef, frame, globalEnv);
  KillIdRef(idRef, frame, pc, globalEnv);
  return value;
}

// Interpreter Functions
//
AbstractCodeInterpreter *AbstractCodeInterpreter::self;

void AbstractCodeInterpreter::Init() {
  self = new AbstractCodeInterpreter();
}


void AbstractCodeInterpreter::DumpAliceFrame(word funCoordW, bool handler, word coord, bool inlined, std::ostream& out) {

  Tuple *funCoord = Tuple::FromWord(funCoordW);
  String *file = String::FromWord(funCoord->Sel(0));
  String *name = String::FromWord(funCoord->Sel(1));
 
  if (handler) {
    out << "<handler> ";
  }
  if (name->GetSize() > 0) {
    out << name;
    out << " (";
  }
  out << file;
  
  //indicate if the line number shown is only as accurate as the function start
  if (coord == Store::IntToWord(0)) {
    s_int line = Store::WordToInt(funCoord->Sel(2));
    out << ", function starting at line " << line;
  }
  else {
    Tuple *posCoord = Tuple::FromWord(coord);
    out << ", line " << Store::WordToInt(posCoord->Sel(0));
  }
  
  if (name->GetSize() > 0) {
    out << ")";
  }
  if (inlined) {
    out << " (call was inlined)";
  }
  out << std::endl;
}


Transform *
AbstractCodeInterpreter::GetAbstractRepresentation(ConcreteRepresentation *b) {
  return reinterpret_cast<AliceConcreteCode *>(b)->GetAbstractRepresentation();
}

void AbstractCodeInterpreter::PushCall_Internal(AliceConcreteCode *acc,
						Closure *closure) {
  AbstractCodeFrame::New(acc, closure);
}

void AbstractCodeInterpreter::PushCall(Closure *closure) {
  AbstractCodeFrame::New(closure);
}

#undef REQUEST
#define REQUEST(w) {				\
  Assert(Store::WordToTransient(w) != INVALID_POINTER); \
  frame->SetPC(pc);				\
  frame->SetCoord(coord);			\
  frame->SetFormalArgs(Vector::New(0));		\
  Scheduler::SetCurrentData(w);			\
  Scheduler::SetNArgs(0);			\
  return Worker::REQUEST;			\
}

#define POP_CHECK_PREEMPT() {			\
  Scheduler::PopFrame(frame->GetSize());        \
  if (StatusWord::GetStatus() != 0)		\
    return Worker::PREEMPT;			\
  else						\
    return Worker::CONTINUE;			\
}

u_int AbstractCodeInterpreter::GetFrameSize(StackFrame *sFrame) {
  AbstractCodeFrame *frame = reinterpret_cast<AbstractCodeFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result AbstractCodeInterpreter::Run(StackFrame *sFrame) {
  AbstractCodeFrame *frame = reinterpret_cast<AbstractCodeFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  
  TagVal *pc = frame->GetPC();
  word coord = frame->GetCoord();
  Closure *globalEnv = frame->GetClosure();
  word formalArgs = frame->GetFormalArgs();
  
  // formal args calling convention conversion
  if (!PointerOp::IsInt(formalArgs)) { // not Wildcard
    Block *bArgs = Store::DirectWordToBlock(formalArgs);
    
    if (bArgs->GetLabel() == Alice::Vector) { // idDef Vector
      Vector *idDefs = reinterpret_cast<Vector*>(bArgs);
      u_int nArgs = idDefs->GetLength();
      switch (nArgs) {
	case 0:
	  break;
	case 1: {
	  Construct();
	  TagVal *idDef = TagVal::FromWord(idDefs->Sub(0));
	  if (idDef != INVALID_POINTER) // IdDef id
	    frame->SetLocal(idDef->Sel(0), Scheduler::GetCurrentArg(0));
	  break;
	}
	default: {
	  if (Deconstruct()) {
	    // Scheduler::currentData has been set by Worker::Deconstruct
	    return Worker::REQUEST;
	  }
	  Assert(Scheduler::GetNArgs() == nArgs);
	  for (u_int i = nArgs; i--; ) {
	    TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
	    if (idDef != INVALID_POINTER) // IdDef id
	      frame->SetLocal(idDef->Sel(0), Scheduler::GetCurrentArg(i));
	  }
	}
      }
    }
    else { // IdDef of id
      TagVal *idDef = reinterpret_cast<TagVal*>(bArgs);
      Construct();
      frame->SetLocal(idDef->Sel(0), Scheduler::GetCurrentArg(0));
    }
  }
  
  // Execution
  while (true) {
  loop:
    switch (AbstractCode::GetInstr(pc)) {
    case AbstractCode::Coord: { // of coord * instr
      coord = pc->Sel(0);
      pc = TagVal::FromWordDirect(pc->Sel(1));
      break;
    }
    case AbstractCode::Entry: // of coord * entry_point * instr
      {
      coord = pc->Sel(0);
#if DEBUGGER
	// DebugFrame Generation
	word event =
	  GenerateEntryEvent(pc->Sel(0), pc->Sel(1), localEnv, globalEnv);
	// TODO: Introduce PushUnder on Stack
 	Scheduler::PopFrame(frame->GetSize());
	DebugWorker::PushFrame(event);
	pc = TagVal::FromWordDirect(pc->Sel(2));
  	PushState(pc, coord, globalEnv, localEnv, formalArgs);
	frame = static_cast<AbstractCodeFrame *>(Scheduler::GetFrame());
	if (Scheduler::GetCurrentThread()->GetDebugMode() == Thread::DEBUG) {
	  Debugger::SendEvent(event);
	  return Worker::SUSPEND;
	}
#else
	pc = TagVal::FromWordDirect(pc->Sel(2));
#endif
      }
      break;
    case AbstractCode::Exit: // of coord * exit_point * idRef * instr
      {
      coord = pc->Sel(0);
#if DEBUGGER
	// Pop current Frame
	Worker *w = frame->GetWorker();
	u_int top = Scheduler::GetCurrentStackTop();
	Scheduler::PopFrame(frame->GetSize());
	// Check for and pop DebugFrame
	StackFrame *dFrame = Scheduler::GetFrame();
	Assert(dFrame->GetWorker() == DebugWorker::self);
	DebugFrame *debugFrame = static_cast<DebugFrame *>(dFrame);
	Scheduler::PopFrame(debugFrame->GetSize());
	word coord = pc->Sel(0);
	word stepPoint = pc->Sel(1);
	word idRef = pc->Sel(2);
	pc = TagVal::FromWordDirect(pc->Sel(3));
  	PushState(pc, coord, globalEnv, localEnv, formalArgs);
	frame = static_cast<AbstractCodeFrame *>(Scheduler::GetFrame());
	if (Scheduler::GetCurrentThread()->GetDebugMode() == Thread::DEBUG) {
	  word idRefRes = GetIdRef(idRef, globalEnv, localEnv);
	  Debugger::SendEvent(GenerateExitEvent(coord, idRefRes, stepPoint));
	  return Worker::SUSPEND;
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
	  frame->KillLocal(kills->Sub(i), pc, globalEnv);
	pc = TagVal::FromWordDirect(pc->Sel(1));
      }
      break;
    case AbstractCode::PutVar: // of id * idRef  * instr
      {
	frame->SetLocal(pc->Sel(0), GetIdRefKill(pc->Sel(1), frame, pc, globalEnv));
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::PutNew: // of id * string * instr
      {
	Constructor *constructor =
	  Constructor::New(String::FromWordDirect(pc->Sel(1)));
	frame->SetLocal(pc->Sel(0), constructor->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::PutTag: // of id * int * int * idRef vector * instr
      {
	Vector *idRefs = Vector::FromWordDirect(pc->Sel(3));
	u_int nargs = idRefs->GetLength();
	TagVal *tagVal;
	u_int offset;
	if (Alice::IsBigTagVal(Store::DirectWordToInt(pc->Sel(1)))) {
	  tagVal =
	    reinterpret_cast<TagVal *>(BigTagVal::New(Store::DirectWordToInt(pc->Sel(2)), nargs));
	  offset = BigTagVal::GetOffset();
	} else {
	  tagVal = TagVal::New(Store::DirectWordToInt(pc->Sel(2)), nargs);
	  offset = TagVal::GetOffset();
	}
	for (u_int i = nargs; i--; )
	  tagVal->Init(i + offset,
		       GetIdRefKill(idRefs->Sub(i), frame, pc, globalEnv));
	frame->SetLocal(pc->Sel(0), tagVal->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(4));
      }
      break;
    case AbstractCode::PutCon: // of id * idRef * idRef vector * instr
      {
	Vector *idRefs = Vector::FromWordDirect(pc->Sel(2));
	u_int nargs = idRefs->GetLength();
	word requestWord = GetIdRef(pc->Sel(1), frame, globalEnv);
	Block *constructor = Store::WordToBlock(requestWord);
	if (constructor == INVALID_POINTER) REQUEST(requestWord);
	KillIdRef(pc->Sel(1), frame, pc, globalEnv);
	ConVal *conVal = ConVal::New(constructor, nargs);
	for (u_int i = nargs; i--; )
	  conVal->Init(i, GetIdRefKill(idRefs->Sub(i), frame, pc, globalEnv));
	frame->SetLocal(pc->Sel(0), conVal->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::PutRef: // of id * idRef * instr
      {
	word contents = GetIdRefKill(pc->Sel(1), frame, pc, globalEnv);
	frame->SetLocal(pc->Sel(0), Cell::New(contents)->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::PutTup: // of id * idRef vector * instr
      {
	Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
	u_int nargs = idRefs->GetLength();
	if (nargs == 0) {
	  frame->SetLocal(pc->Sel(0), Store::IntToWord(0)); // unit
	} else {
	  Tuple *tuple = Tuple::New(nargs);
	  for (u_int i = nargs; i--; )
	    tuple->Init(i, GetIdRefKill(idRefs->Sub(i), frame, pc, globalEnv));
	  frame->SetLocal(pc->Sel(0), tuple->ToWord());
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
	  record->Init(i, GetIdRefKill(idRefs->Sub(i), frame, pc, globalEnv));
	}
	frame->SetLocal(pc->Sel(0), record->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::PutVec: // of id * idRef vector * instr
      {
	Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
	u_int nargs = idRefs->GetLength();
	Vector *vector = Vector::New(nargs);
	for (u_int i = nargs; i--; )
	  vector->Init(i, GetIdRefKill(idRefs->Sub(i), frame, pc, globalEnv));
	frame->SetLocal(pc->Sel(0), vector->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::Close: // of id * idRef vector * template * instr
      {
	Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
	u_int nGlobals = idRefs->GetLength();
	
	word wConcreteCode = GetCloseConcreteCode(globalEnv->GetConcreteCode(), pc);
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
	  closure->Init(i, GetIdRefKill(idRefs->Sub(i), frame, pc, globalEnv));
	frame->SetLocal(pc->Sel(0), closure->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::Specialize: // of id * idRef vector * template * instr
      {
	// Construct new abstract code by instantiating template:
	Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
	u_int nGlobals = idRefs->GetLength();
	TagVal *template_ = TagVal::FromWordDirect(pc->Sel(2));
	template_->AssertWidth(AbstractCode::functionWidth);
	Assert(static_cast<u_int>(Store::DirectWordToInt(template_->Sel(1))) ==
	       nGlobals);
	
	TagVal *abstractCode =
	  TagVal::New(AbstractCode::Function, AbstractCode::functionWidth);
	abstractCode->Init(0, template_->Sel(0));
	Vector *subst = Vector::New(nGlobals);
	for (u_int i = nGlobals; i--; ) {
	  word value = GetIdRefKill(idRefs->Sub(i), frame, pc, globalEnv);
	  TagVal *some = TagVal::New(Types::SOME, 1);
	  some->Init(0, value);
	  subst->Init(i, some->ToWord());
	}
	abstractCode->Init(1, subst->ToWord());
	abstractCode->Init(2, template_->Sel(2));
	abstractCode->Init(3, template_->Sel(3));
	abstractCode->Init(4, template_->Sel(4));
	abstractCode->Init(5, template_->Sel(5));
	abstractCode->Init(6, template_->Sel(6));
	
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
	for (u_int i = nGlobals; i--; ) {
	  closure->Init(i, TagVal::FromWordDirect(subst->Sub(i))->Sel(0));
	}
	frame->SetLocal(pc->Sel(0), closure->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::AppPrim:
      // of value * idRef vector * (idDef * instr) option
      {
	// setup arguments for the primitive
	Vector *actualIdRefs = Vector::FromWordDirect(pc->Sel(1));
	u_int nArgs = actualIdRefs->GetLength();
	Scheduler::SetNArgs(nArgs);
	for (u_int i = nArgs; i--; )
	  Scheduler::SetCurrentArg(i,
	    GetIdRefKill(actualIdRefs->Sub(i), frame, pc, globalEnv));
	  
	TagVal *idDefInstrOpt = TagVal::FromWord(pc->Sel(2));
	if (idDefInstrOpt != INVALID_POINTER) { // SOME (idDef * instr)
	  // Save our state for return
	  Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
	  frame->SetPC(idDefInstr->Sel(1));
          frame->SetCoord(coord);
	  frame->SetFormalArgs(idDefInstr->Sel(0));
	}
	else {
	  Scheduler::PopFrame(frame->GetSize());
	}
	
	return Scheduler::PushCall(pc->Sel(0));
      }
      break;
    case AbstractCode::AppVar:
      // of idRef * idRef vector * bool * (idDef vector * instr) option
      {
	// setup argument for the call
	Vector *actualIdRefs = Vector::FromWordDirect(pc->Sel(1));
	u_int nArgs = actualIdRefs->GetLength();
	Scheduler::SetNArgs(nArgs);
	for (u_int i = nArgs; i--; )
	  Scheduler::SetCurrentArg(i,
	    GetIdRefKill(actualIdRefs->Sub(i), frame, pc, globalEnv));
 
	word closure = GetIdRefKill(pc->Sel(0), frame, pc, globalEnv);
	
	TagVal *idDefsInstrOpt = TagVal::FromWord(pc->Sel(3));
	if (idDefsInstrOpt != INVALID_POINTER) { // SOME ...
	  // Save our state for return
	  Tuple *idDefsInstr = Tuple::FromWordDirect(idDefsInstrOpt->Sel(0));
	  frame->SetPC(idDefsInstr->Sel(1));
          frame->SetCoord(coord);
	  frame->SetFormalArgs(idDefsInstr->Sel(0));
	}
	else {
	  Scheduler::PopFrame(frame->GetSize());
	}

	if (StatusWord::GetStatus() != 0) {
	  Worker::Result res = Scheduler::PushCall(closure);
	  return res == Worker::CONTINUE? Worker::PREEMPT: res;
	} else {
	  return Scheduler::PushCall(closure);
	}
      }
      break;
    case AbstractCode::GetRef: // of id * idRef * instr
      {
	word requestWord = GetIdRef(pc->Sel(1), frame, globalEnv);
	Cell *cell = Cell::FromWord(requestWord);
	if (cell == INVALID_POINTER) REQUEST(requestWord);
	KillIdRef(pc->Sel(1), frame, pc, globalEnv);
	frame->SetLocal(pc->Sel(0), cell->Access());
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::GetTup: // of idDef vector * idRef * instr
      {
	word requestWord = GetIdRef(pc->Sel(1), frame, globalEnv);
	Vector *idDefs = Vector::FromWordDirect(pc->Sel(0));
	u_int nargs = idDefs->GetLength();
	if (nargs == 0) {
	  if (Store::WordToInt(requestWord) == INVALID_INT)
	    REQUEST(requestWord);
	  KillIdRef(pc->Sel(1), frame, pc, globalEnv);
	} else {
	  Tuple *tuple = Tuple::FromWord(requestWord);
	  if (tuple == INVALID_POINTER) REQUEST(requestWord);
	  KillIdRef(pc->Sel(1), frame, pc, globalEnv);
	  tuple->AssertWidth(idDefs->GetLength());
	  for (u_int i = nargs; i--; ) {
	    TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
	    if (idDef != INVALID_POINTER) // IdDef id
	      frame->SetLocal(idDef->Sel(0), tuple->Sel(i));
	  }
	}
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::Sel: // of id * idRef * int * instr
      {
	word requestWord = GetIdRef(pc->Sel(1), frame, globalEnv);
	Tuple *tuple = Tuple::FromWord(requestWord);
	if (tuple == INVALID_POINTER) REQUEST(requestWord);
	KillIdRef(pc->Sel(1), frame, pc, globalEnv);
	frame->SetLocal(pc->Sel(0),
		      tuple->Sel(Store::DirectWordToInt(pc->Sel(2))));
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::LazyPolySel:
      // of id vector * idRef * label vector * instr
      {
	word wRecord = GetIdRefKill(pc->Sel(1), frame, pc, globalEnv);
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
	    frame->SetLocal(ids->Sub(i), byneed->ToWord());
	  }
	} else
	  for (u_int i = ids->GetLength(); i--; ) {
	    UniqueString *label = UniqueString::FromWordDirect(labels->Sub(i));
	    frame->SetLocal(ids->Sub(i), record->PolySel(label));
	  }
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::Raise: // of idRef
      {
	word requestWord = GetIdRef(pc->Sel(0), frame, globalEnv);
	Transient *transient = Store::WordToTransient(requestWord);
	if (transient != INVALID_POINTER) REQUEST(transient->ToWord());
	KillIdRef(pc->Sel(0), frame, pc, globalEnv);
	Scheduler::SetCurrentData(requestWord);
	Scheduler::SetCurrentBacktrace(Backtrace::New(frame->Clone()));
	return Worker::RAISE;
      }
      break;
    case AbstractCode::Reraise: // of idRef
      {
	Tuple *package =
	  Tuple::FromWordDirect(GetIdRefKill(pc->Sel(0), frame, pc, globalEnv));
	package->AssertWidth(2);
	Scheduler::SetCurrentData(package->Sel(0));
	Scheduler::SetCurrentBacktrace
	  (Backtrace::FromWordDirect(package->Sel(1)));
	return Worker::RAISE;
      }
    case AbstractCode::Try: // of instr * idDef * idDef * instr
      {
	// Push a handler stack frame
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
	Scheduler::PopHandler();
	pc = TagVal::FromWordDirect(pc->Sel(0));
      }
      break;
    case AbstractCode::IntTest: // of idRef * (int * instr) vector * instr
      {
	word requestWord = GetIdRef(pc->Sel(0), frame, globalEnv);
	s_int value = Store::WordToInt(requestWord);
	if (value == INVALID_INT) REQUEST(requestWord);
	KillIdRef(pc->Sel(0), frame, pc, globalEnv);
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
	word requestWord = GetIdRef(pc->Sel(0), frame, globalEnv);
	s_int value = Store::WordToInt(requestWord);
	if (value == INVALID_INT) REQUEST(requestWord);
	KillIdRef(pc->Sel(0), frame, pc, globalEnv);
	s_int offset = Store::DirectWordToInt(pc->Sel(1));
	u_int index = static_cast<u_int>(value - offset);
	Vector *tests = Vector::FromWordDirect(pc->Sel(2));
	if (index < tests->GetLength())
	  pc = TagVal::FromWordDirect(tests->Sub(index));
	else
	  pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::RealTest: // of idRef * (real * instr) vector * instr
      {
	word requestWord = GetIdRef(pc->Sel(0), frame, globalEnv);
	Real *real = Real::FromWord(requestWord);
	if (real == INVALID_POINTER) REQUEST(requestWord);
	KillIdRef(pc->Sel(0), frame, pc, globalEnv);
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
	word requestWord = GetIdRef(pc->Sel(0), frame, globalEnv);
	String *string = String::FromWord(requestWord);
	if (string == INVALID_POINTER) REQUEST(requestWord);
	KillIdRef(pc->Sel(0), frame, pc, globalEnv);
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
      // of idRef * int * (int * instr) vector
      //          * (int * idDef vector * instr) vector * instr
      {
	word requestWord = GetIdRef(pc->Sel(0), frame, globalEnv);
	TagVal *tagVal;
	if (Alice::IsBigTagVal(Store::DirectWordToInt(pc->Sel(1))))
	  tagVal = reinterpret_cast<TagVal *>(BigTagVal::FromWord(requestWord));
	else
	  tagVal = TagVal::FromWord(requestWord);
	if (tagVal == INVALID_POINTER) { // nullary constructor or transient
	  s_int tag = Store::WordToInt(requestWord);
	  if (tag == INVALID_INT) REQUEST(requestWord);
	  KillIdRef(pc->Sel(0), frame, pc, globalEnv);
	  Vector *tests = Vector::FromWordDirect(pc->Sel(2));
	  u_int ntests = tests->GetLength();
	  for (u_int i = 0; i < ntests; i++) {
	    Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
	    if (Store::DirectWordToInt(pair->Sel(0)) == tag) {
	      pc = TagVal::FromWordDirect(pair->Sel(1));
	      goto loop;
	    }
	  }
	} else { // non-nullary constructor
	  KillIdRef(pc->Sel(0), frame, pc, globalEnv);
	  s_int tag;
	  u_int offset;
	  if (Alice::IsBigTagVal(Store::DirectWordToInt(pc->Sel(1)))) {
	    tag    = reinterpret_cast<BigTagVal *>(tagVal)->GetTag();
	    offset = BigTagVal::GetOffset();
	  } else {
	    tag    = tagVal->GetTag();
	    offset = TagVal::GetOffset();
	  }
	  Vector *tests = Vector::FromWordDirect(pc->Sel(3));
	  u_int ntests = tests->GetLength();
	  for (u_int i = 0; i < ntests; i++) {
	    Tuple *triple = Tuple::FromWordDirect(tests->Sub(i));
	    if (Store::DirectWordToInt(triple->Sel(0)) == tag) {
	      Vector *idDefs = Vector::FromWordDirect(triple->Sel(1));
	      tagVal->AssertWidth(idDefs->GetLength() + offset);
	      for (u_int i = idDefs->GetLength(); i--; ) {
		TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
		if (idDef != INVALID_POINTER) // IdDef id
		  frame->SetLocal(idDef->Sel(0), tagVal->Sel(i + offset));
	      }
	      pc = TagVal::FromWordDirect(triple->Sel(2));
	      goto loop;
	    }
	  }
	}
	pc = TagVal::FromWordDirect(pc->Sel(4));
      }
      break;
    case AbstractCode::CompactTagTest: //of idRef * int * tagTests * instr option
      {
	word requestWord = GetIdRef(pc->Sel(0), frame, globalEnv);
	TagVal *tagVal;
	if (Alice::IsBigTagVal(Store::DirectWordToInt(pc->Sel(1))))
	  tagVal = reinterpret_cast<TagVal *>(BigTagVal::FromWord(requestWord));
	else
	  tagVal = TagVal::FromWord(requestWord);
	if (tagVal == INVALID_POINTER) { // nullary constructor or transient
	  s_int tag = Store::WordToInt(requestWord);
	  if (tag == INVALID_INT) REQUEST(requestWord);
	  KillIdRef(pc->Sel(0), frame, pc, globalEnv);
	  Vector *tests = Vector::FromWordDirect(pc->Sel(2));
	  if (static_cast<u_int>(tag) < tests->GetLength()) {
	    Tuple *tuple = Tuple::FromWordDirect(tests->Sub(tag));
	    Assert(tuple->Sel(0) == Store::IntToWord(Types::NONE));
	    pc = TagVal::FromWordDirect(tuple->Sel(1));
	    goto loop;
	  }
	} else { // non-nullary constructor
	  KillIdRef(pc->Sel(0), frame, pc, globalEnv);
	  s_int tag;
	  u_int offset;
	  if (Alice::IsBigTagVal(Store::DirectWordToInt(pc->Sel(1)))) {
	    tag    = reinterpret_cast<BigTagVal *>(tagVal)->GetTag();
	    offset = BigTagVal::GetOffset();
	  } else {
	    tag    = tagVal->GetTag();
	    offset = TagVal::GetOffset();
	  }
	  Vector *tests = Vector::FromWordDirect(pc->Sel(2));
	  if (static_cast<u_int>(tag) < tests->GetLength()) {
	    Tuple *tuple = Tuple::FromWordDirect(tests->Sub(tag));
	    TagVal *idDefsOpt = TagVal::FromWordDirect(tuple->Sel(0));
	    Vector *idDefs = Vector::FromWordDirect(idDefsOpt->Sel(0));
	    tagVal->AssertWidth(idDefs->GetLength() + offset);
	    for (u_int i = idDefs->GetLength(); i--; ) {
	      TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
	      if (idDef != INVALID_POINTER) // IdDef id
		frame->SetLocal(idDef->Sel(0), tagVal->Sel(i + offset));
	    }
	    pc = TagVal::FromWordDirect(tuple->Sel(1));
	    goto loop;
	  }
	}
	TagVal *someElseInstr = TagVal::FromWordDirect(pc->Sel(3));
	pc = TagVal::FromWordDirect(someElseInstr->Sel(0));
      }
      break;
    case AbstractCode::ConTest:
      // of idRef * (idRef * instr) vector
      //          * (idRef * idDef vector * instr) vector * instr
      {
	word requestWord = GetIdRef(pc->Sel(0), frame, globalEnv);
	ConVal *conVal = ConVal::FromWord(requestWord);
	if (conVal == INVALID_POINTER) REQUEST(requestWord);
	if (conVal->IsConVal()) { // non-nullary constructor
	  Block *constructor = conVal->GetConstructor();
	  Vector *tests = Vector::FromWordDirect(pc->Sel(2));
	  u_int ntests = tests->GetLength();
	  for (u_int i = 0; i < ntests; i++) {
	    Tuple *triple = Tuple::FromWordDirect(tests->Sub(i));
	    requestWord = GetIdRef(triple->Sel(0), frame, globalEnv);
	    Block *testConstructor = Store::WordToBlock(requestWord);
	    if (testConstructor == INVALID_POINTER) REQUEST(requestWord);
	    if (testConstructor == constructor) {
	      Vector *idDefs = Vector::FromWordDirect(triple->Sel(1));
	      conVal->AssertWidth(idDefs->GetLength());
	      for (u_int i = idDefs->GetLength(); i--; ) {
		TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
		if (idDef != INVALID_POINTER) // IdDef id
		  frame->SetLocal(idDef->Sel(0), conVal->Sel(i));
	      }
	      KillIdRef(pc->Sel(0), frame, pc, globalEnv); //--** some kills missing
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
	    requestWord = GetIdRef(pair->Sel(0), frame, globalEnv);
	    Block *testConstructor = Store::WordToBlock(requestWord);
	    if (testConstructor == INVALID_POINTER) REQUEST(requestWord);
	    if (testConstructor == constructor) {
	      KillIdRef(pc->Sel(0), frame, pc, globalEnv); //--** some kills missing
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
	word requestWord = GetIdRef(pc->Sel(0), frame, globalEnv);
	Vector *vector = Vector::FromWord(requestWord);
	if (vector == INVALID_POINTER) REQUEST(requestWord);
	KillIdRef(pc->Sel(0), frame, pc, globalEnv);
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
		frame->SetLocal(idDef->Sel(0), vector->Sub(i));
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
	Scheduler::SetNArgs(nArgs);
	for (u_int i = nArgs; i--; )
	  Scheduler::SetCurrentArg(i,
	    GetIdRefKill(returnIdRefs->Sub(i), frame, pc, globalEnv));
	POP_CHECK_PREEMPT();
      }
      break;
    default:
      Error("AbstractCodeInterpreter::Run: unknown instr tag");
    }
  }
}

Worker::Result AbstractCodeInterpreter::Handle(word data, Tuple *package) {
  StackFrame *sFrame = Scheduler::GetFrame();
  Assert(sFrame->GetWorker() == this);
  AbstractCodeFrame *frame = reinterpret_cast<AbstractCodeFrame *>(sFrame);
  
  Scheduler::SetNArgs(2);
  Scheduler::SetCurrentArg(0, package->ToWord());
  Scheduler::SetCurrentArg(1, package->Sel(0));
  Tuple *exnData = Tuple::FromWordDirect(data);
  frame->SetPC(TagVal::FromWordDirect(exnData->Sel(0)));
  frame->SetFormalArgs(exnData->Sel(1));
  return Worker::CONTINUE;
}

u_int AbstractCodeInterpreter::GetInArity(ConcreteCode *concreteCode) {
  Assert(concreteCode->GetInterpreter() == AbstractCodeInterpreter::self);
  AliceConcreteCode *aliceConcreteCode =
    reinterpret_cast<AliceConcreteCode *>(concreteCode);
  TagVal *abstractCode = aliceConcreteCode->GetAbstractCode();
  Vector *idDefs = Vector::FromWordDirect(abstractCode->Sel(3));
  u_int nArgs = idDefs->GetLength();
  return nArgs;
}

u_int AbstractCodeInterpreter::GetOutArity(ConcreteCode *concreteCode) {
  Assert(concreteCode->GetInterpreter() == AbstractCodeInterpreter::self);
  AliceConcreteCode *aliceConcreteCode =
    reinterpret_cast<AliceConcreteCode *>(concreteCode);
  TagVal *abstractCode = aliceConcreteCode->GetAbstractCode();
  TagVal *outArityOpt = TagVal::FromWord(abstractCode->Sel(4));
  return ((outArityOpt == INVALID_POINTER) ? INVALID_INT :
	  Store::DirectWordToInt(outArityOpt->Sel(0)));
}

const char *AbstractCodeInterpreter::Identify() {
  return "AbstractCodeInterpreter";
}

void AbstractCodeInterpreter::DumpFrame(StackFrame *sFrame, std::ostream& out) {
  
  AbstractCodeFrame *frame = reinterpret_cast<AbstractCodeFrame *>(sFrame);
  Closure *closure = frame->GetClosure();
  // might be {Alice,HotSpot,Byte}ConcreteCode
  ConcreteCode *cc = ConcreteCode::FromWord(closure->GetConcreteCode());
  
  Transform *tr = cc->GetInterpreter()->GetAbstractRepresentation(
    reinterpret_cast<ConcreteRepresentation*>(cc));
  TagVal *abstractCode = TagVal::FromWord(tr->GetArgument());
  
  AbstractCodeInterpreter::DumpAliceFrame(abstractCode->Sel(0), frame->IsHandlerFrame(), frame->GetCoord(), false, out);
}

String *AbstractCodeInterpreter::MakeProfileName(TagVal *abstractCode) {
  
  Tuple *funCoord = Tuple::FromWordDirect(abstractCode->Sel(0));
  String *file = String::FromWordDirect(funCoord->Sel(0));
  String *name = String::FromWordDirect(funCoord->Sel(1));
  s_int line   = Store::DirectWordToInt(funCoord->Sel(2));
  s_int column = Store::DirectWordToInt(funCoord->Sel(3));
  
  Vector *subst = Vector::FromWordDirect(abstractCode->Sel(1));
  u_int nSubst = 0;
  for (u_int i = subst->GetLength(); i--; ) {
    if (TagVal::FromWord(subst->Sub(i)) != INVALID_POINTER) {
      nSubst++;
    }
  }
  
  std::stringstream ss;
  if (name->GetSize() > 0) {
    ss << name << " at ";
  }
  ss << file << ", line " << line << ", column " << column <<
    " (" << subst->GetLength() << " globals, " << nSubst << " specialised)";
  return String::New(ss.str());
}

#if PROFILE

word AbstractCodeInterpreter::GetProfileKey(StackFrame *sFrame) {
  return reinterpret_cast<AbstractCodeFrame *>(sFrame)->GetClosure()->GetConcreteCode();
}

word AbstractCodeInterpreter::GetProfileKey(ConcreteCode *concreteCode) {
  return concreteCode->ToWord();
}

String *AbstractCodeInterpreter::GetProfileName(StackFrame *sFrame) {
  AbstractCodeFrame *frame = reinterpret_cast<AbstractCodeFrame *>(sFrame);
  // might not be an AliceConcreteCode
  ConcreteCode *cc = ConcreteCode::FromWord(frame->GetClosure()->GetConcreteCode());
  return cc->GetInterpreter()->GetProfileName(cc);
}

String *AbstractCodeInterpreter::GetProfileName(ConcreteCode *cc) {
  Assert(cc->GetInterpreter() == this);
  return MakeProfileName(
    reinterpret_cast<AliceConcreteCode*>(cc)->GetAbstractCode());
}

#endif
