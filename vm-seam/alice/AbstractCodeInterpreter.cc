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
#include "generic/RootSet.hh"
#include "generic/Scheduler.hh"
#include "generic/Backtrace.hh"
#include "generic/Closure.hh"
#include "generic/Transients.hh"
#include "alice/Data.hh"
#include "alice/AbstractCode.hh"
#include "alice/LazySelInterpreter.hh"
#include "alice/AliceConcreteCode.hh"
#include "alice/AliceLanguageLayer.hh"

#if PROFILE
#include "generic/Profiler.hh"
#endif

#if defined(DEBUG_CHECK)
static word dead;
#endif

void DisassembleAlice(Closure *closure) {
  AliceConcreteCode *concreteCode =
    AliceConcreteCode::FromWord(closure->GetConcreteCode());
  concreteCode->Disassemble(stdout);
}

// Local Environment
class Environment: private Array {
public:
  using Array::ToWord;
  // Environment Accessors
  void Add(word id, word value) {
    Update(Store::WordToInt(id), value);
  }
  word Lookup(word id) {
    word value = Sub(Store::WordToInt(id));
    Assert(value != dead);
    return value;
  }
  void Kill(word id) {
#ifdef DEBUG_CHECK
    Update(Store::WordToInt(id), dead);
#else
    Update(Store::WordToInt(id), Store::IntToWord(0));
#endif
  }
  // Environment Constructor
  static Environment *New(u_int size) {
    return static_cast<Environment *>(Array::New(size));
  }
  // Environment Untagging
  static Environment *FromWordDirect(word x) {
    return static_cast<Environment *>(Array::FromWordDirect(x));
  }
};

// AbstractCodeInterpreter StackFrames
class AbstractCodeFrame: public StackFrame {
protected:
  static const u_int PC_POS          = 0;
  static const u_int CLOSURE_POS     = 1;
  static const u_int LOCAL_ENV_POS   = 2;
  static const u_int FORMAL_ARGS_POS = 3;
  static const u_int SIZE            = 4;
public:
  using Block::ToWord;
  using StackFrame::GetInterpreter;
  // AbstractCodeFrame Accessors
  bool IsHandlerFrame() {
    if (GetLabel() == ABSTRACT_CODE_HANDLER_FRAME) {
      return true;
    } else {
      Assert(GetLabel() == ABSTRACT_CODE_FRAME);
      return false;
    }
  }
  TagVal *GetPC() {
    return TagVal::FromWordDirect(StackFrame::GetArg(PC_POS));
  }
  Closure *GetClosure() {
    return Closure::FromWordDirect(StackFrame::GetArg(CLOSURE_POS));
  }
  Environment *GetLocalEnv() {
    return Environment::FromWordDirect(StackFrame::GetArg(LOCAL_ENV_POS));
  }
  TagVal *GetFormalArgs() {
    return TagVal::FromWord(StackFrame::GetArg(FORMAL_ARGS_POS));
  }
  // AbstractCodeFrame Constructor
  static AbstractCodeFrame *New(Interpreter *interpreter,
				word pc,
				Closure *closure,
				Environment *env,
				word formalArgs) {
    StackFrame *frame =
      StackFrame::New(ABSTRACT_CODE_FRAME, interpreter, SIZE);
    frame->InitArg(PC_POS, pc);
    frame->InitArg(CLOSURE_POS, closure->ToWord());
    frame->InitArg(LOCAL_ENV_POS, env->ToWord());
    frame->InitArg(FORMAL_ARGS_POS, formalArgs);
    return static_cast<AbstractCodeFrame *>(frame);
  }
  // AbstractCodeFrame Untagging
  static AbstractCodeFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == ABSTRACT_CODE_FRAME ||
	   p->GetLabel() == ABSTRACT_CODE_HANDLER_FRAME);
    return static_cast<AbstractCodeFrame *>(p);
  }
};

class AbstractCodeHandlerFrame: public AbstractCodeFrame {
public:
  // AbstractCodeHandlerFrame Constructor
  static AbstractCodeHandlerFrame *New(Interpreter *interpreter,
				       word pc,
				       Closure *closure,
				       Environment *env,
				       word args) {
    StackFrame *frame =
      StackFrame::New(ABSTRACT_CODE_HANDLER_FRAME, interpreter, SIZE);
    frame->InitArg(PC_POS, pc);
    frame->InitArg(CLOSURE_POS, closure->ToWord());
    frame->InitArg(LOCAL_ENV_POS, env->ToWord());
    frame->InitArg(FORMAL_ARGS_POS, args);
    return static_cast<AbstractCodeHandlerFrame *>(frame);
  }
};

// Interpreter Helper
inline void PushState(TagVal *pc, Closure *globalEnv, Environment *localEnv,
		      TagVal *formalArgs) {
  AbstractCodeFrame *frame =
    AbstractCodeFrame::New(AbstractCodeInterpreter::self, pc->ToWord(),
			   globalEnv, localEnv, formalArgs->ToWord());
  Scheduler::PushFrame(frame->ToWord());
}

inline void PushState(TagVal *pc, Closure *globalEnv, Environment *localEnv) {
  AbstractCodeFrame *frame =
    AbstractCodeFrame::New(AbstractCodeInterpreter::self, pc->ToWord(),
			   globalEnv, localEnv, Store::IntToWord(0));
  Scheduler::PushFrame(frame->ToWord());
}

inline word
GetIdRefKill(word idRef, Closure *globalEnv, Environment *localEnv) {
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
      localEnv->Kill(id);
      return value;
    }
  case AbstractCode::Global:
    return globalEnv->Sub(Store::WordToInt(tagVal->Sel(0)));
  default:
    Error("AbstractCodeInterpreter::GetIdRef: invalid idRef tag");
  }
}

inline word GetIdRef(word idRef, Closure *globalEnv, Environment *localEnv) {
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

inline void KillIdRef(word idRef, Environment *localEnv) {
  TagVal *tagVal = TagVal::FromWordDirect(idRef);
  if (AbstractCode::GetIdRef(tagVal) == AbstractCode::LastUseLocal)
    localEnv->Kill(tagVal->Sel(0));
}

//
// Interpreter Functions
//
AbstractCodeInterpreter *AbstractCodeInterpreter::self;

void AbstractCodeInterpreter::Init() {
  self = new AbstractCodeInterpreter();
#ifdef DEBUG_CHECK
  dead = String::New("DEAD VALUE")->ToWord();
  RootSet::Add(dead);
#endif
}

Block *
AbstractCodeInterpreter::GetAbstractRepresentation(Block *blockWithHandler) {
  AliceConcreteCode *concreteCode =
    static_cast<AliceConcreteCode *>(blockWithHandler);
  return concreteCode->GetAbstractRepresentation();
}

void AbstractCodeInterpreter::PushCall(Closure *closure) {
  AliceConcreteCode *concreteCode =
    AliceConcreteCode::FromWord(closure->GetConcreteCode());
  // Function of coord * int * int * idDef args * instr * liveness
  TagVal *abstractCode = concreteCode->GetAbstractCode();
  switch (AbstractCode::GetAbstractCode(abstractCode)) {
  case AbstractCode::Function:
    {
      abstractCode->AssertWidth(AbstractCode::functionWidth);
      Vector *localNames = Vector::FromWordDirect(abstractCode->Sel(2));
      u_int nLocals = localNames->GetLength();
      AbstractCodeFrame *frame =
	AbstractCodeFrame::New(AbstractCodeInterpreter::self,
			       abstractCode->Sel(4), closure,
			       Environment::New(nLocals),
			       abstractCode->Sel(3));
      Scheduler::PushFrame(frame->ToWord());
    }
    break;
  default:
    Error("AbstractCodeInterpreter::PushCall: invalid abstractCode tag");
  }
}

#define REQUEST(w) {				\
  PushState(pc, globalEnv, localEnv);		\
  Scheduler::currentData = w;			\
  Scheduler::nArgs = 0;				\
  return Interpreter::REQUEST;			\
}

#define CHECK_PREEMPT() {			\
  if (StatusWord::GetStatus() != 0)		\
    return Interpreter::PREEMPT;		\
  else						\
    return Interpreter::CONTINUE;		\
}

Interpreter::Result AbstractCodeInterpreter::Run() {
  AbstractCodeFrame *frame =
    AbstractCodeFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  Assert(frame->GetInterpreter() == this);
  Assert(!frame->IsHandlerFrame());
  TagVal *pc = frame->GetPC();
  Closure *globalEnv = frame->GetClosure();
  Environment *localEnv = frame->GetLocalEnv();
  TagVal *formalArgs = frame->GetFormalArgs();
  if (formalArgs != INVALID_POINTER) {
    // Calling convention conversion
    switch (AbstractCode::GetArgs(formalArgs)) {
    case AbstractCode::OneArg:
      {
	Construct();
	TagVal *idDef = TagVal::FromWord(formalArgs->Sel(0));
	if (idDef != INVALID_POINTER) // IdDef id
	  localEnv->Add(idDef->Sel(0), Scheduler::currentArgs[0]);
      }
      break;
    case AbstractCode::TupArgs:
      {
	Vector *formalIdDefs = Vector::FromWordDirect(formalArgs->Sel(0));
	u_int nArgs = formalIdDefs->GetLength();
	if (nArgs == 0) {
	  if (Scheduler::nArgs != 0) {
	    Assert(Scheduler::nArgs == Scheduler::ONE_ARG);
	    word requestWord = Scheduler::currentArgs[0];
	    if (Store::WordToInt(requestWord) == INVALID_INT)
	      REQUEST(requestWord);
	    Assert(Store::WordToInt(requestWord) == 0); // unit
	  }
	} else {
	  if (Interpreter::Deconstruct()) {
	    // Scheduler::currentData has been set by Interpreter::Deconstruct
	    Scheduler::PushFrameNoCheck(frame->ToWord());
	    return Interpreter::REQUEST;
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
    default:
      Error("AbstractCodeInterpreter::Run: invalid formalArgs tag");
    }
  }
  // Execution
  while (true) {
  loop:
    switch (AbstractCode::GetInstr(pc)) {
    case AbstractCode::Kill: // of id vector * instr
      {
	Vector *kills = Vector::FromWordDirect(pc->Sel(0));
	for (u_int i = kills->GetLength(); i--; )
	  localEnv->Kill(kills->Sub(i));
	pc = TagVal::FromWordDirect(pc->Sel(1));
      }
      break;
    case AbstractCode::PutVar: // of id * idRef  * instr
      {
	localEnv->Add(pc->Sel(0),
		      GetIdRefKill(pc->Sel(1), globalEnv, localEnv));
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
	  tagVal->Init(i, GetIdRefKill(idRefs->Sub(i), globalEnv, localEnv));
	localEnv->Add(pc->Sel(0), tagVal->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::PutCon: // of id * idRef * idRef vector * instr
      {
	Vector *idRefs = Vector::FromWordDirect(pc->Sel(2));
	u_int nargs = idRefs->GetLength();
	word requestWord = GetIdRef(pc->Sel(1), globalEnv, localEnv);
	Constructor *constructor = Constructor::FromWord(requestWord);
	if (constructor == INVALID_POINTER) REQUEST(requestWord);
	KillIdRef(pc->Sel(1), localEnv);
	ConVal *conVal = ConVal::New(constructor, nargs);
	for (u_int i = nargs; i--; )
	  conVal->Init(i, GetIdRefKill(idRefs->Sub(i), globalEnv, localEnv));
	localEnv->Add(pc->Sel(0), conVal->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::PutRef: // of id * idRef * instr
      {
	word contents = GetIdRefKill(pc->Sel(1), globalEnv, localEnv);
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
	    tuple->Init(i, GetIdRefKill(idRefs->Sub(i), globalEnv, localEnv));
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
	  record->Init(i, GetIdRefKill(idRefs->Sub(i), globalEnv, localEnv));
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
	  vector->Init(i, GetIdRefKill(idRefs->Sub(i), globalEnv, localEnv));
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
	Assert(static_cast<u_int>(Store::DirectWordToInt(template_->Sel(1))) ==
	       nGlobals);
	Vector *subst = Vector::New(nGlobals);
	for (u_int i = nGlobals; i--; )
	  subst->Init(0, Store::IntToWord(0)); // NONE
	abstractCode->Init(1, subst->ToWord());
	abstractCode->Init(2, template_->Sel(2));
	abstractCode->Init(3, template_->Sel(3));
	abstractCode->Init(4, template_->Sel(4));
	abstractCode->Init(5, template_->Sel(5));
	// Construct concrete code from abstract code:
	word wConcreteCode =
	  AliceLanguageLayer::concreteCodeConstructor(abstractCode);
	Closure *closure = Closure::New(wConcreteCode, nGlobals);
	for (u_int i = nGlobals; i--; )
	  closure->Init(i, GetIdRefKill(idRefs->Sub(i), globalEnv, localEnv));
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
#if PROFILE
	Profiler::IncInstances(template_);
#endif
	abstractCode->Init(0, template_->Sel(0));
	abstractCode->Init(2, template_->Sel(2));
	abstractCode->Init(3, template_->Sel(3));
	abstractCode->Init(4, template_->Sel(4));
	abstractCode->Init(5, template_->Sel(5));
	// Construct concrete code from abstract code:
	word wConcreteCode =
	  AliceLanguageLayer::concreteCodeConstructor(abstractCode);
	// Construct closure from concrete code:
	Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
	u_int nGlobals = idRefs->GetLength();
	Assert(static_cast<u_int>(Store::DirectWordToInt(template_->Sel(1))) ==
	       nGlobals);
	Vector *subst = Vector::New(nGlobals);
	Closure *closure = Closure::New(wConcreteCode, nGlobals);
	for (u_int i = nGlobals; i--; ) {
	  word value = GetIdRefKill(idRefs->Sub(i), globalEnv, localEnv);
	  TagVal *some = TagVal::New(1, 1); // SOME ...
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
	TagVal *idDefInstrOpt = TagVal::FromWord(pc->Sel(2));
	if (idDefInstrOpt != INVALID_POINTER) { // SOME (idDef * instr)
	  // Save our state for return
	  Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
	  TagVal *formalArgs = TagVal::New(AbstractCode::OneArg, 1);
	  formalArgs->Init(0, idDefInstr->Sel(0));
	  PushState(TagVal::FromWordDirect(idDefInstr->Sel(1)),
		    globalEnv, localEnv, formalArgs);
	}
	// Push a call frame for the primitive
	Vector *actualIdRefs = Vector::FromWordDirect(pc->Sel(1));
	u_int nArgs = actualIdRefs->GetLength();
	Scheduler::nArgs = nArgs == 1? Scheduler::ONE_ARG: nArgs;
	for (u_int i = nArgs; i--; )
	  Scheduler::currentArgs[i] =
	    GetIdRefKill(actualIdRefs->Sub(i), globalEnv, localEnv);
	return Scheduler::PushCall(pc->Sel(0));
      }
      break;
    case AbstractCode::AppVar:
    case AbstractCode::DirectAppVar:
      // of idRef * idRef args * (idDef args * instr) option
      {
	TagVal *idDefArgsInstrOpt = TagVal::FromWord(pc->Sel(2));
	if (idDefArgsInstrOpt != INVALID_POINTER) { // SOME ...
	  // Save our state for return
	  Tuple *idDefArgsInstr =
	    Tuple::FromWordDirect(idDefArgsInstrOpt->Sel(0));
	  PushState(TagVal::FromWordDirect(idDefArgsInstr->Sel(1)),
		    globalEnv, localEnv,
		    TagVal::FromWordDirect(idDefArgsInstr->Sel(0)));
	}
	TagVal *actualArgs = TagVal::FromWordDirect(pc->Sel(1));
	switch (AbstractCode::GetArgs(actualArgs)) {
	case AbstractCode::OneArg:
	  Scheduler::nArgs = Scheduler::ONE_ARG;
	  Scheduler::currentArgs[0] =
	    GetIdRefKill(actualArgs->Sel(0), globalEnv, localEnv);
	  break;
	case AbstractCode::TupArgs:
	  {
	    Vector *actualIdRefs = Vector::FromWordDirect(actualArgs->Sel(0));
	    u_int nArgs = actualIdRefs->GetLength();
	    Scheduler::nArgs = nArgs;
	    for (u_int i = nArgs; i--; )
	      Scheduler::currentArgs[i] =
		GetIdRefKill(actualIdRefs->Sub(i), globalEnv, localEnv);
	  }
	  break;
	}
	if (StatusWord::GetStatus() != 0) {
	  Interpreter::Result res =
	    Scheduler::PushCall(GetIdRefKill(pc->Sel(0), globalEnv, localEnv));
	  return res == Interpreter::CONTINUE? Interpreter::PREEMPT: res;
	} else {
	  word closure = GetIdRefKill(pc->Sel(0), globalEnv, localEnv);
	  return Scheduler::PushCall(closure);
	}
      }
      break;
    case AbstractCode::GetRef: // of id * idRef * instr
      {
	word requestWord = GetIdRef(pc->Sel(1), globalEnv, localEnv);
	Cell *cell = Cell::FromWord(requestWord);
	if (cell == INVALID_POINTER) REQUEST(requestWord);
	KillIdRef(pc->Sel(1), localEnv);
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
	  KillIdRef(pc->Sel(1), localEnv);
	} else {
	  Tuple *tuple = Tuple::FromWord(requestWord);
	  if (tuple == INVALID_POINTER) REQUEST(requestWord);
	  KillIdRef(pc->Sel(1), localEnv);
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
	KillIdRef(pc->Sel(1), localEnv);
	localEnv->Add(pc->Sel(0),
		      tuple->Sel(Store::DirectWordToInt(pc->Sel(2))));
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::LazyPolySel:
      // of id vector * idRef * label vector * instr
      {
	word wRecord = GetIdRefKill(pc->Sel(1), globalEnv, localEnv);
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
	Scheduler::currentData = GetIdRefKill(pc->Sel(0), globalEnv, localEnv);
	Scheduler::currentBacktrace = Backtrace::New(frame->ToWord());
	return Interpreter::RAISE;
      }
      break;
    case AbstractCode::Reraise: // of idRef
      {
	Tuple *package =
	  Tuple::FromWordDirect(GetIdRefKill(pc->Sel(0), globalEnv, localEnv));
	package->AssertWidth(2);
	Scheduler::currentData = package->Sel(0);
	Scheduler::currentBacktrace =
	  Backtrace::FromWordDirect(package->Sel(1));
	return Interpreter::RAISE;
      }
    case AbstractCode::Try: // of instr * idDef * idDef * instr
      {
	// Push a handler stack frame:
	Vector *formalIdDefs = Vector::New(2);
	formalIdDefs->Init(0, pc->Sel(1));
	formalIdDefs->Init(1, pc->Sel(2));
	TagVal *formalArgs = TagVal::New(AbstractCode::TupArgs, 1);
	formalArgs->Init(0, formalIdDefs->ToWord());
	AbstractCodeHandlerFrame *frame =
	  AbstractCodeHandlerFrame::New(this, pc->Sel(3), globalEnv, localEnv,
					formalArgs->ToWord());
	Scheduler::PushFrame(frame->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(0));
      }
      break;
    case AbstractCode::EndTry: // of instr
      {
	Assert(StackFrame::FromWordDirect(Scheduler::GetFrame())->GetLabel() ==
	       ABSTRACT_CODE_HANDLER_FRAME);
	Scheduler::PopFrame();
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
	int value = Store::WordToInt(requestWord);
	if (value == INVALID_INT) REQUEST(requestWord);
	KillIdRef(pc->Sel(0), localEnv);
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
	int value = Store::WordToInt(requestWord);
	if (value == INVALID_INT) REQUEST(requestWord);
	KillIdRef(pc->Sel(0), localEnv);
	int offset = Store::DirectWordToInt(pc->Sel(1));
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
	word requestWord = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	Real *real = Real::FromWord(requestWord);
	if (real == INVALID_POINTER) REQUEST(requestWord);
	KillIdRef(pc->Sel(0), localEnv);
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
	KillIdRef(pc->Sel(0), localEnv);
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
	  int tag = Store::WordToInt(requestWord);
	  if (tag == INVALID_INT) REQUEST(requestWord);
	  KillIdRef(pc->Sel(0), localEnv);
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
	  KillIdRef(pc->Sel(0), localEnv);
	  int tag = tagVal->GetTag();
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
    case AbstractCode::CompactTagTest: // of idRef * tagTests * instr
      {
	word requestWord = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	TagVal *tagVal = TagVal::FromWord(requestWord);
	if (tagVal == INVALID_POINTER) { // nullary constructor or transient
	  int tag = Store::WordToInt(requestWord);
	  if (tag == INVALID_INT) REQUEST(requestWord);
	  KillIdRef(pc->Sel(0), localEnv);
	  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
	  if (static_cast<u_int>(tag) < tests->GetLength()) {
	    Tuple *tuple = Tuple::FromWordDirect(tests->Sub(tag));
	    Assert(tuple->Sel(0) == Store::IntToWord(0)); // NONE
	    pc = TagVal::FromWordDirect(tuple->Sel(1));
	    goto loop;
	  }
	} else { // non-nullary constructor
	  KillIdRef(pc->Sel(0), localEnv);
	  int tag = tagVal->GetTag();
	  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
	  if (static_cast<u_int>(tag) < tests->GetLength()) {
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
	pc = TagVal::FromWordDirect(pc->Sel(2));
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
	  Constructor *constructor = conVal->GetConstructor();
	  Vector *tests = Vector::FromWordDirect(pc->Sel(2));
	  u_int ntests = tests->GetLength();
	  for (u_int i = 0; i < ntests; i++) {
	    Tuple *triple = Tuple::FromWordDirect(tests->Sub(i));
	    requestWord = GetIdRef(triple->Sel(0), globalEnv, localEnv);
	    Constructor *testConstructor = Constructor::FromWord(requestWord);
	    if (testConstructor == INVALID_POINTER) REQUEST(requestWord);
	    if (testConstructor == constructor) {
	      Vector *idDefs = Vector::FromWordDirect(triple->Sel(1));
	      conVal->AssertWidth(idDefs->GetLength());
	      for (u_int i = idDefs->GetLength(); i--; ) {
		TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
		if (idDef != INVALID_POINTER) // IdDef id
		  localEnv->Add(idDef->Sel(0), conVal->Sel(i));
	      }
	      KillIdRef(pc->Sel(0), localEnv); //--** some kills missing
	      pc = TagVal::FromWordDirect(triple->Sel(2));
	      goto loop;
	    }
	  }
	} else { // nullary constructor
	  Constructor *constructor =
	    static_cast<Constructor *>(static_cast<Block *>(conVal));
	  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
	  u_int ntests = tests->GetLength();
	  for (u_int i = 0; i < ntests; i++) {
	    Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
	    requestWord = GetIdRef(pair->Sel(0), globalEnv, localEnv);
	    Constructor *testConstructor = Constructor::FromWord(requestWord);
	    if (testConstructor == INVALID_POINTER) REQUEST(requestWord);
	    if (testConstructor == constructor) {
	      KillIdRef(pc->Sel(0), localEnv); //--** some kills missing
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
	KillIdRef(pc->Sel(0), localEnv);
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
    case AbstractCode::Return: // of idRef args
      {
	TagVal *returnArgs = TagVal::FromWordDirect(pc->Sel(0));
	switch (AbstractCode::GetArgs(returnArgs)) {
	case AbstractCode::OneArg:
	  Scheduler::nArgs = Scheduler::ONE_ARG;
	  Scheduler::currentArgs[0] =
	    GetIdRefKill(returnArgs->Sel(0), globalEnv, localEnv);
	  break;
	case AbstractCode::TupArgs:
	  {
	    Vector *returnIdRefs = Vector::FromWordDirect(returnArgs->Sel(0));
	    u_int nArgs = returnIdRefs->GetLength();
	    Assert(nArgs <= Scheduler::maxArgs);
	    Scheduler::nArgs = nArgs;
	    for (u_int i = nArgs; i--; )
	      Scheduler::currentArgs[i] =
		GetIdRefKill(returnIdRefs->Sub(i), globalEnv, localEnv);
	  }
	  break;
	}
	CHECK_PREEMPT();
      }
      break;
    default:
      Error("AbstractCodeInterpreter::Run: unknown instr tag");
    }
  }
}

Interpreter::Result AbstractCodeInterpreter::Handle() {
  AbstractCodeFrame *frame =
    AbstractCodeFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  if (frame->IsHandlerFrame()) {
    Tuple *package = Tuple::New(2);
    word exn = Scheduler::currentData;
    package->Init(0, exn);
    package->Init(1, Scheduler::currentBacktrace->ToWord());
    Scheduler::nArgs = 2;
    Scheduler::currentArgs[0] = package->ToWord();
    Scheduler::currentArgs[1] = exn;
    AbstractCodeFrame *newFrame =
      AbstractCodeFrame::New(self, frame->GetPC()->ToWord(),
			     frame->GetClosure(), frame->GetLocalEnv(),
			     frame->GetFormalArgs()->ToWord());
    Scheduler::PushFrameNoCheck(newFrame->ToWord());
    return Interpreter::CONTINUE;
  } else {
    Scheduler::currentBacktrace->Enqueue(frame->ToWord());
    return Interpreter::RAISE;
  }
}

const char *AbstractCodeInterpreter::Identify() {
  return "AbstractCodeInterpreter";
}

void AbstractCodeInterpreter::DumpFrame(word frameWord) {
  AbstractCodeFrame *frame = AbstractCodeFrame::FromWordDirect(frameWord);
  Closure *closure = frame->GetClosure();
  AliceConcreteCode *concreteCode =
    AliceConcreteCode::FromWord(closure->GetConcreteCode());
  Assert(concreteCode != INVALID_POINTER);
  TagVal *abstractCode = concreteCode->GetAbstractCode();
  Tuple *coord = Tuple::FromWordDirect(abstractCode->Sel(0));
  String *name = String::FromWordDirect(coord->Sel(0));
  fprintf(stderr, "Alice %s %.*s, line %d\n",
	  frame->IsHandlerFrame()? "handler": "function",
	  (int) name->GetSize(), name->GetValue(),
	  Store::DirectWordToInt(coord->Sel(1)));
}

#if PROFILE
word AbstractCodeInterpreter::GetProfileKey(StackFrame *frame) {
  AbstractCodeFrame *f = AbstractCodeFrame::FromWordDirect(frame->ToWord());
  ConcreteCode *concreteCode =
    ConcreteCode::FromWord(f->GetClosure()->GetConcreteCode());
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

String *AbstractCodeInterpreter::GetProfileName(StackFrame *frame) {
  AbstractCodeFrame *f = AbstractCodeFrame::FromWordDirect(frame->ToWord());
  Closure *closure = f->GetClosure();
  AliceConcreteCode *concreteCode =
    AliceConcreteCode::FromWord(closure->GetConcreteCode());
  return MakeProfileName(concreteCode,
			 f->IsHandlerFrame()? "handler": "function");
}

String *AbstractCodeInterpreter::GetProfileName(ConcreteCode *concreteCode) {
  return MakeProfileName(static_cast<AliceConcreteCode *>(concreteCode),
			 "function");
}
#endif
