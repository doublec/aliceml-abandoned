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
#define implementation "emulator/AbstractCodeInterpreter.hh"
#endif

#include "emulator/AbstractCodeInterpreter.hh"
#include "emulator/TaskStack.hh"
#include "emulator/Scheduler.hh"
#include "emulator/Closure.hh"
#include "emulator/ConcreteCode.hh"
#include "emulator/Alice.hh"
#include "emulator/Pickle.hh"
#include "emulator/LazySelectionInterpreter.hh"

// Local Environment
class Environment : private Array {
public:
  using Array::ToWord;
  // Environment Accessors
  void Add(word id, word value) {
    Update(Store::WordToInt(id), value);
  }
  word Lookup(word id) {
    return Sub(Store::WordToInt(id));
  }
  void Kill(word id) {
    Update(Store::WordToInt(id), Store::IntToWord(0));
  }
  // Environment Constructor
  static Environment *New(u_int size) {
    return (Environment *) Array::New(size);
  }
  // Environment Untagging
  static Environment *FromWord(word x) {
    return (Environment *) Array::FromWord(x);
  }
  static Environment *FromWordDirect(word x) {
    return (Environment *) Array::FromWordDirect(x);
  }
};

// AbstractCodeInterpreter StackFrames
class AbstractCodeFrame : public StackFrame {
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
    if (((Block *) this)->GetLabel() ==
	(BlockLabel) ABSTRACT_CODE_HANDLER_FRAME)
      return true;
    else {
      Assert(((Block *) this)->GetLabel() == (BlockLabel) ABSTRACT_CODE_FRAME);
      return false;
    }
  }
  TagVal *GetPC() {
    return TagVal::FromWord(StackFrame::GetArg(PC_POS));
  }
  Closure *GetClosure() {
    return Closure::FromWord(StackFrame::GetArg(CLOSURE_POS));
  }
  Environment *GetLocalEnv() {
    return Environment::FromWord(StackFrame::GetArg(LOCAL_ENV_POS));
  }
  TagVal *GetFormalArgs() {
    return TagVal::FromWord(StackFrame::GetArg(FORMAL_ARGS_POS));
  }
  // AbstractCodeFrame Constructor
  static AbstractCodeFrame *New(Interpreter *interpreter,
				word pc,
				Closure *closure,
				Environment *env,
				word args) {
    StackFrame *frame =
      StackFrame::New(ABSTRACT_CODE_FRAME, interpreter, SIZE);
    frame->ReplaceArg(PC_POS, pc);
    frame->ReplaceArg(CLOSURE_POS, closure->ToWord());
    frame->ReplaceArg(LOCAL_ENV_POS, env->ToWord());
    frame->ReplaceArg(FORMAL_ARGS_POS, args);
    return (AbstractCodeFrame *) frame;
  }
  // AbstractCodeFrame Untagging
  static AbstractCodeFrame *FromWord(word frame) {
    Block *p = Store::WordToBlock(frame);
    Assert(p == INVALID_POINTER ||
	   p->GetLabel() == (BlockLabel) ABSTRACT_CODE_FRAME ||
	   p->GetLabel() == (BlockLabel) ABSTRACT_CODE_HANDLER_FRAME);
    return (AbstractCodeFrame *) p;
  }
  static AbstractCodeFrame *FromWordDirect(word frame) {
    Block *p = Store::DirectWordToBlock(frame);
    Assert(p == INVALID_POINTER ||
	   p->GetLabel() == (BlockLabel) ABSTRACT_CODE_FRAME ||
	   p->GetLabel() == (BlockLabel) ABSTRACT_CODE_HANDLER_FRAME);
    return (AbstractCodeFrame *) p;
  }
};

class AbstractCodeHandlerFrame : public AbstractCodeFrame {
public:
  // AbstractCodeHandlerFrame Constructor
  static AbstractCodeHandlerFrame *New(Interpreter *interpreter,
				       word pc,
				       Closure *closure,
				       Environment *env,
				       word args) {
    StackFrame *frame =
      StackFrame::New(ABSTRACT_CODE_HANDLER_FRAME, interpreter, SIZE);
    frame->ReplaceArg(PC_POS, pc);
    frame->ReplaceArg(CLOSURE_POS, closure->ToWord());
    frame->ReplaceArg(LOCAL_ENV_POS, env->ToWord());
    frame->ReplaceArg(FORMAL_ARGS_POS, args);
    return (AbstractCodeHandlerFrame *) frame;
  }
  // AbstractCodeHandlerFrame Untagging
  static AbstractCodeHandlerFrame *FromWord(word frame) {
    Block *p = Store::WordToBlock(frame);
    Assert(p == INVALID_POINTER ||
	   p->GetLabel() == (BlockLabel) ABSTRACT_CODE_HANDLER_FRAME);
    return (AbstractCodeHandlerFrame *) p;
  }
  static AbstractCodeHandlerFrame *FromWordDirect(word frame) {
    Block *p = Store::DirectWordToBlock(frame);
    Assert(p == INVALID_POINTER ||
	   p->GetLabel() == (BlockLabel) ABSTRACT_CODE_HANDLER_FRAME);
    return (AbstractCodeHandlerFrame *) p;
  }
};

// Interpreter Helper
inline void PushState(TaskStack *taskStack,
		      Interpreter *interpreter,
		      TagVal *pc,
		      Closure *closure,
		      Environment *localEnv,
		      TagVal *formalArgs) {
  taskStack->PushFrame(
		       AbstractCodeFrame::New(interpreter,
					      pc->ToWord(),
					      closure,
					      localEnv,
					      formalArgs->ToWord())->ToWord());
}

inline void PushState(TaskStack *taskStack,
		      Interpreter *interpreter,
		      TagVal *pc,
		      Closure *globalEnv,
		      Environment *localEnv) {
  //--** formalArgs should only be constructed once
  TagVal *formalArgs = TagVal::New(Pickle::TupArgs, 1);
  formalArgs->Init(0, Vector::New(0)->ToWord());
  PushState(taskStack, interpreter, pc, globalEnv, localEnv, formalArgs);
}
#define SUSPEND(w) {						\
  PushState(taskStack, this, pc, globalEnv, localEnv);		\
  Scheduler::currentData = w;                                   \
  return Interpreter::REQUEST;		       		\
}

inline word GetIdRef(word idRef, Closure *globalEnv, Environment *localEnv) {
  TagVal *tagVal = TagVal::FromWord(idRef);
  switch (Pickle::GetIdRef(tagVal)) {
  case Pickle::Local:
    return localEnv->Lookup(tagVal->Sel(0));
  case Pickle::Global:
    return globalEnv->Sub(Store::WordToInt(tagVal->Sel(0)));
  default:
    Error("AbstractCodeInterpreter::GetIdRef: invalid idRef tag");
  }
}

//
// Interpreter Functions
//
AbstractCodeInterpreter *AbstractCodeInterpreter::self;

void AbstractCodeInterpreter::PushCall(TaskStack *taskStack,
				       Closure *closure) {
  ConcreteCode *concreteCode =
    ConcreteCode::FromWord(closure->GetConcreteCode());
  Assert(concreteCode->GetInterpreter() == this);
  // datatype function = Function of coord * int * int * idDef args * instr
  TagVal *function = TagVal::FromWord(concreteCode->Get(0));
  int nlocals = Store::WordToInt(function->Sel(2));
  AbstractCodeFrame *frame =
    AbstractCodeFrame::New(this,
			   function->Sel(4),
			   closure,
			   Environment::New(nlocals),
			   function->Sel(3));
  taskStack->PushFrame(frame->ToWord());
}

void AbstractCodeInterpreter::PurgeFrame(TaskStack *) {
  return; // trivial, since we interpret Kill statements
}

Interpreter::Result
AbstractCodeInterpreter::Run(word args, TaskStack *taskStack) {
  AbstractCodeFrame *frame =
    AbstractCodeFrame::FromWord(taskStack->GetFrame());
  Assert(frame != INVALID_POINTER && frame->GetInterpreter() == this);
  TagVal *pc            = frame->GetPC();
  Closure *globalEnv    = frame->GetClosure();
  Environment *localEnv = frame->GetLocalEnv();
  TagVal *formalArgs    = frame->GetFormalArgs();
  // Calling convention conversion
  switch (Pickle::GetArgs(formalArgs)) {
  case Pickle::OneArg:
    {
      TagVal *idDef = TagVal::FromWord(formalArgs->Sel(0));
      if (idDef != INVALID_POINTER) // IdDef id
	localEnv->Add(idDef->Sel(0), Interpreter::Construct(args));
    }
    break;
  case Pickle::TupArgs:
    {
      Vector *formalIdDefs = Vector::FromWord(formalArgs->Sel(0));
      word deconstructed_args = Interpreter::Deconstruct(args);
      Block *p = Store::WordToBlock(deconstructed_args);
      if (p == INVALID_POINTER) {
	// Scheduler::currentData has been set by Interpreter::Deconstruct
	return Interpreter::REQUEST;
      }
      u_int nargs = p->GetSize(); //--** not exact
      // Internal Assertion
      formalArgs->AssertWidth(nargs);
      for (u_int i = nargs; i--; ) {
	TagVal *idDef = TagVal::FromWord(formalIdDefs->Sub(i));
	if (idDef != INVALID_POINTER) // IdDef id
	  localEnv->Add(idDef->Sel(0), p->GetArg(i));
      }
    }
    break;
  default:
    Error("AbstractCodeInterpreter::Run: invalid formalArgs");
  }
  taskStack->PopFrame(); // Discard Frame
  // Execution
  while (!(Scheduler::TestPreempt() || Store::NeedGC())) {
  loop:
    switch (Pickle::GetInstr(pc)) {
    case Pickle::Kill: // of id vector * instr
      {
	Vector *kills = Vector::FromWord(pc->Sel(0));
	for (u_int i = kills->GetLength(); i--; )
	  localEnv->Kill(kills->Sub(i));
	pc = TagVal::FromWord(pc->Sel(1));
      }
      break;
    case Pickle::PutConst: // of id * value * instr
      {
	localEnv->Add(pc->Sel(0), pc->Sel(1));
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::PutVar: // of id * idRef  * instr
      {
	localEnv->Add(pc->Sel(0), GetIdRef(pc->Sel(1), globalEnv, localEnv));
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::PutNew: // of id * instr
      {
	localEnv->Add(pc->Sel(0), Constructor::New()->ToWord());
	pc = TagVal::FromWord(pc->Sel(1));
      }
      break;
    case Pickle::PutTag: // of id * int * idRef vector * instr
      {
	Vector *idRefs = Vector::FromWord(pc->Sel(2));
	u_int nargs = idRefs->GetLength();
	TagVal *tagVal = TagVal::New(Store::WordToInt(pc->Sel(1)), nargs);
	for (u_int i = nargs; i--; )
	  tagVal->Init(i, GetIdRef(idRefs->Sub(i), globalEnv, localEnv));
	localEnv->Add(pc->Sel(0), tagVal->ToWord());
	pc = TagVal::FromWord(pc->Sel(3));
      }
      break;
    case Pickle::PutCon: // of id * idRef * idRef vector * instr
      {
	Vector *idRefs = Vector::FromWord(pc->Sel(2));
	u_int nargs = idRefs->GetLength();
	word suspendWord = GetIdRef(pc->Sel(1), globalEnv, localEnv);
	Constructor *constructor = Constructor::FromWord(suspendWord);
	if (constructor == INVALID_POINTER) SUSPEND(suspendWord);
	ConVal *conVal = ConVal::New(constructor, nargs);
	for (u_int i = nargs; i--; )
	  conVal->Init(i, GetIdRef(idRefs->Sub(i), globalEnv, localEnv));
	localEnv->Add(pc->Sel(0), conVal->ToWord());
	pc = TagVal::FromWord(pc->Sel(3));
      }
      break;
    case Pickle::PutRef: // of id * idRef * instr
      {
	word contents = GetIdRef(pc->Sel(1), globalEnv, localEnv);
	localEnv->Add(pc->Sel(0), Cell::New(contents)->ToWord());
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::PutTup: // of id * idRef vector * instr
      {
	Vector *idRefs = Vector::FromWord(pc->Sel(1));
	u_int nargs = idRefs->GetLength();
	if (nargs == 0) {
	  localEnv->Add(pc->Sel(0), Store::IntToWord(0)); // unit
	} else {
	  Tuple *tuple = Tuple::New(nargs);
	  for (u_int i = nargs; i--; )
	    tuple->Init(i, GetIdRef(idRefs->Sub(i), globalEnv, localEnv));
	  localEnv->Add(pc->Sel(0), tuple->ToWord());
	}
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::PutVec: // of id * idRef vector * instr
      {
	Vector *idRefs = Vector::FromWord(pc->Sel(1));
	u_int nargs = idRefs->GetLength();
	Vector *vector = Vector::New(nargs);
	for (u_int i = nargs; i--; )
	  vector->Init(i, GetIdRef(idRefs->Sub(i), globalEnv, localEnv));
	localEnv->Add(pc->Sel(0), vector->ToWord());
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::PutFun: // of id * idRef vector * function * instr
      {
	Vector *idRefs = Vector::FromWord(pc->Sel(1));
	u_int nglobals = idRefs->GetLength();
	Closure *closure = Closure::New(pc->Sel(2), nglobals);
	for (u_int i = nglobals; i--; )
	  closure->Init(i, GetIdRef(idRefs->Sub(i), globalEnv, localEnv));
	localEnv->Add(pc->Sel(0), closure->ToWord());
	pc = TagVal::FromWord(pc->Sel(3));
      }
      break;
    case Pickle::AppPrim: // of value * idRef vector * (idDef * instr) option
      {
	TagVal *idDefInstrOpt = TagVal::FromWord(pc->Sel(2));
	if (idDefInstrOpt != INVALID_POINTER) { // SOME (idDef * instr)
	  // Save our state for return
	  Tuple *idDefInstr = Tuple::FromWord(idDefInstrOpt->Sel(0));
	  Vector *formalIdDefs = Vector::New(1);
	  formalIdDefs->Init(0, idDefInstr->Sel(0));
	  TagVal *formalArgs = TagVal::New(Pickle::TupArgs, 1);
	  formalArgs->Init(0, formalIdDefs->ToWord());
	  PushState(taskStack, this, TagVal::FromWord(idDefInstr->Sel(1)),
		    globalEnv, localEnv, formalArgs);
	}
	// Push a call frame for the primitive
	taskStack->PushCall(pc->Sel(0));
	Vector *actualIdRefs = Vector::FromWord(pc->Sel(1));
	u_int nargs  = actualIdRefs->GetLength();
	Block *pargs = Interpreter::TupArgs(nargs);
	for (u_int i = nargs; i--; ) {
	  word arg = GetIdRef(actualIdRefs->Sub(i), globalEnv, localEnv);
	  pargs->InitArg(i, arg);
	}
	Scheduler::currentArgs = pargs->ToWord();
	return Interpreter::CONTINUE;
      }
      break;
    case Pickle::AppVar: // of idRef * idRef args * (idDef args * instr) option
      {
	TagVal *idDefArgsInstrOpt = TagVal::FromWord(pc->Sel(2));
	if (idDefArgsInstrOpt != INVALID_POINTER) { // SOME ...
	  // Save our state for return
	  Tuple *idDefArgsInstr = Tuple::FromWord(idDefArgsInstrOpt->Sel(0));
	  PushState(taskStack, this,
		    TagVal::FromWord(idDefArgsInstr->Sel(1)),
		    globalEnv, localEnv,
		    TagVal::FromWord(idDefArgsInstr->Sel(0)));
	}
	taskStack->PushCall(GetIdRef(pc->Sel(0), globalEnv, localEnv));
	TagVal *actualArgs = TagVal::FromWord(pc->Sel(1));
	switch (Pickle::GetArgs(actualArgs)) {
	case Pickle::OneArg:
	  Scheduler::currentArgs =
	    Interpreter::OneArg(GetIdRef(actualArgs->Sel(0),
					 globalEnv, localEnv));
	  return Interpreter::CONTINUE;
	case Pickle::TupArgs:
	  {
	    Vector *actualIdRefs = Vector::FromWord(actualArgs->Sel(0));
	    u_int nargs  = actualIdRefs->GetLength();
	    Block *pargs = Interpreter::TupArgs(nargs);
	    for (u_int i = nargs; i--; ) {
	      pargs->InitArg(i, GetIdRef(actualIdRefs->Sub(i),
					 globalEnv, localEnv));
	    }
	    Scheduler::currentArgs = pargs->ToWord();
	    return Interpreter::CONTINUE;
	  }
	}
      }
      break;
    case Pickle::AppConst:
      // of value * idRef args * (idDef args * instr) option
      //--** avoid code duplication with AppVar
      {
	TagVal *idDefArgsInstrOpt = TagVal::FromWord(pc->Sel(2));
	if (idDefArgsInstrOpt != INVALID_POINTER) { // SOME ...
	  // Save our state for return
	  Tuple *idDefArgsInstr = Tuple::FromWord(idDefArgsInstrOpt->Sel(0));
	  PushState(taskStack, this,
		    TagVal::FromWord(idDefArgsInstr->Sel(1)),
		    globalEnv, localEnv,
		    TagVal::FromWord(idDefArgsInstr->Sel(0)));
	}
	taskStack->PushCall(pc->Sel(0)); //--** only difference to AppVar
	TagVal *actualArgs = TagVal::FromWord(pc->Sel(1));
	switch (Pickle::GetArgs(actualArgs)) {
	case Pickle::OneArg:
	  Scheduler::currentArgs =
	    Interpreter::OneArg(GetIdRef(actualArgs->Sel(0),
					 globalEnv, localEnv));
	  return Interpreter::CONTINUE;
	case Pickle::TupArgs:
	  {
	    Vector *actualIdRefs = Vector::FromWord(actualArgs->Sel(0));
	    u_int nargs  = actualIdRefs->GetLength();
	    Block *pargs = Interpreter::TupArgs(nargs);
	    for (u_int i = nargs; i--; ) {
	      pargs->InitArg(i, GetIdRef(actualIdRefs->Sub(i),
					 globalEnv, localEnv));
	    }
	    Scheduler::currentArgs = pargs->ToWord();
	    return Interpreter::CONTINUE;
	  }
	}
      }
      break;
    case Pickle::GetRef: // of id * idRef * instr
      {
	word suspendWord = GetIdRef(pc->Sel(1), globalEnv, localEnv);
	Cell *cell = Cell::FromWord(suspendWord);
	if (cell == INVALID_POINTER) SUSPEND(suspendWord);
	localEnv->Add(pc->Sel(0), cell->Access());
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::GetTup: // of idDef vector * idRef * instr
      {
	word suspendWord = GetIdRef(pc->Sel(1), globalEnv, localEnv);
	Vector *idDefs = Vector::FromWord(pc->Sel(0));
	u_int nargs = idDefs->GetLength();
	if (nargs == 0) {
	  if (Store::WordToInt(suspendWord) == INVALID_INT)
	    SUSPEND(suspendWord);
	} else {
	  Tuple *tuple = Tuple::FromWord(suspendWord);
	  if (tuple == INVALID_POINTER) SUSPEND(suspendWord);
	  tuple->AssertWidth(idDefs->GetLength());
	  for (u_int i = nargs; i--; ) {
	    TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
	    if (idDef != INVALID_POINTER) // IdDef id
	      localEnv->Add(idDef->Sel(0), tuple->Sel(i));
	  }
	}
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::Sel: // of id * idRef * int * instr
      {
	word suspendWord = GetIdRef(pc->Sel(1), globalEnv, localEnv);
	Tuple *tuple = Tuple::FromWord(suspendWord);
	if (tuple == INVALID_POINTER) SUSPEND(suspendWord);
	localEnv->Add(pc->Sel(0), tuple->Sel(Store::WordToInt(pc->Sel(2))));
	pc = TagVal::FromWord(pc->Sel(3));
      }
      break;
    case Pickle::LazySel: // of id * idRef * int * instr
      {
	word tuple = GetIdRef(pc->Sel(1), globalEnv, localEnv);
	int index = Store::WordToInt(pc->Sel(2));
	localEnv->Add(pc->Sel(0),
		      LazySelectionClosure::New(tuple, index)->ToWord());
	pc = TagVal::FromWord(pc->Sel(3));
      }
      break;
    case Pickle::Raise: // of idRef
      {
	Scheduler::currentData = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	return Interpreter::RAISE;
      }
      break;
    case Pickle::Reraise: // of idRef
      {
	//--** separate the exception from the exception package
	Scheduler::currentData = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	return Interpreter::RAISE;
      }
    case Pickle::Try: // of instr * idDef * idDef * instr
      {
	// Push a handler stack frame:
	TagVal *formalArgs = TagVal::New(Pickle::TupArgs, 1);
	Vector *formalIdDefs = Vector::New(2);
	formalIdDefs->Init(0, pc->Sel(1));
	formalIdDefs->Init(1, pc->Sel(2));
	formalArgs->Init(0, formalIdDefs->ToWord());
	AbstractCodeHandlerFrame *frame =
	  AbstractCodeHandlerFrame::New(this,
					pc->Sel(3),
					globalEnv,
					localEnv,
					formalArgs->ToWord());
	taskStack->PushFrame(frame->ToWord());
	pc = TagVal::FromWord(pc->Sel(0));
      }
      break;
    case Pickle::EndTry: // of instr
      {
	Assert(Store::WordToBlock(taskStack->GetFrame()) != INVALID_POINTER);
	Assert(Store::WordToBlock(taskStack->GetFrame())->GetLabel() ==
	       (BlockLabel) ABSTRACT_CODE_HANDLER_FRAME);
	taskStack->PopFrame();
	pc = TagVal::FromWord(pc->Sel(0));
      }
      break;
    case Pickle::EndHandle: // of instr
      {
	pc = TagVal::FromWord(pc->Sel(0));
      }
      break;
    case Pickle::IntTest: // of idRef * (int * instr) vector * instr
      {
	word suspendWord = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	int value = Store::WordToInt(suspendWord);
	if (value == INVALID_INT) SUSPEND(suspendWord);
	Vector *tests = Vector::FromWord(pc->Sel(1));
	u_int ntests = tests->GetLength();
	for (u_int i = 0; i < ntests; i++) {
	  Tuple *pair = Tuple::FromWord(tests->Sub(i));
	  if (Store::WordToInt(pair->Sel(0)) == value) {
	    pc = TagVal::FromWord(pair->Sel(1));
	    goto loop;
	  }
	}
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::RealTest: // of idRef * (real * instr) vector * instr
      {
	word suspendWord = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	Real *real = Real::FromWord(suspendWord);
	if (real == INVALID_POINTER) SUSPEND(suspendWord);
	double value = real->GetValue();
	Vector *tests = Vector::FromWord(pc->Sel(1));
	u_int ntests = tests->GetLength();
	for (u_int i = 0; i < ntests; i++) {
	  Tuple *pair = Tuple::FromWord(tests->Sub(i));
	  if (Real::FromWord(pair->Sel(0))->GetValue() == value) {
	    pc = TagVal::FromWord(pair->Sel(1));
	    goto loop;
	  }
	}
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::StringTest: // of idRef * (string * instr) vector * instr
      {
	word suspendWord = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	String *string = String::FromWord(suspendWord);
	if (string == INVALID_POINTER) SUSPEND(suspendWord);
	const char *value = string->GetValue();
	u_int length = string->GetSize();
	Vector *tests = Vector::FromWord(pc->Sel(1));
	u_int ntests = tests->GetLength();
	for (u_int i = 0; i < ntests; i++) {
	  Tuple *pair = Tuple::FromWord(tests->Sub(i));
	  string = String::FromWord(pair->Sel(0));
	  if (string->GetSize() == length &&
	      !memcmp(string->GetValue(), value, length)) {
	    pc = TagVal::FromWord(pair->Sel(1));
	    goto loop;
	  }
	}
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    //--** WideStringTest
    case Pickle::TagTest:
      // of idRef * (int * instr) vector
      //          * (int * idDef vector * instr) vector * instr
      {
	word suspendWord = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	TagVal *tagVal = TagVal::FromWord(suspendWord);
	if (tagVal == INVALID_POINTER) { // nullary constructor or transient
	  int tag = Store::WordToInt(suspendWord);
	  if (tag == INVALID_INT) SUSPEND(suspendWord);
	  Vector *tests = Vector::FromWord(pc->Sel(1));
	  u_int ntests = tests->GetLength();
	  for (u_int i = 0; i < ntests; i++) {
	    Tuple *pair = Tuple::FromWord(tests->Sub(i));
	    if (Store::WordToInt(pair->Sel(0)) == tag) {
	      pc = TagVal::FromWord(pair->Sel(1));
	      goto loop;
	    }
	  }
	} else { // non-nullary constructor
	  int tag = tagVal->GetTag();
	  Vector *tests = Vector::FromWord(pc->Sel(2));
	  u_int ntests = tests->GetLength();
	  for (u_int i = 0; i < ntests; i++) {
	    Tuple *triple = Tuple::FromWord(tests->Sub(i));
	    if (Store::WordToInt(triple->Sel(0)) == tag) {
	      Vector *idDefs = Vector::FromWord(triple->Sel(1));
	      // Internal Assertion
	      tagVal->AssertWidth(idDefs->GetLength());
	      for (u_int i = idDefs->GetLength(); i--; ) {
		TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
		if (idDef != INVALID_POINTER) // IdDef id
		  localEnv->Add(idDef->Sel(0), tagVal->Sel(i));
	      }
	      pc = TagVal::FromWord(triple->Sel(2));
	      goto loop;
	    }
	  }
	}
	pc = TagVal::FromWord(pc->Sel(3));
      }
      break;
    case Pickle::ConTest:
      // of idRef * (idRef * instr) vector
      //          * (idRef * idDef vector * instr) vector * instr
      {
	word suspendWord = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	ConVal *conVal = ConVal::FromWord(suspendWord);
	if (conVal == INVALID_POINTER) SUSPEND(suspendWord);
	if (conVal->IsConVal()) { // non-nullary constructor
	  Constructor *constructor = conVal->GetConstructor();
	  Vector *tests = Vector::FromWord(pc->Sel(2));
	  u_int ntests = tests->GetLength();
	  for (u_int i = 0; i < ntests; i++) {
	    Tuple *triple = Tuple::FromWord(tests->Sub(i));
	    suspendWord = GetIdRef(triple->Sel(0), globalEnv, localEnv);
	    Constructor *testConstructor = Constructor::FromWord(suspendWord);
	    if (testConstructor == INVALID_POINTER) SUSPEND(suspendWord);
	    if (testConstructor == constructor) {
	      Vector *idDefs = Vector::FromWord(triple->Sel(1));
	      // Internal Assertion
	      conVal->AssertWidth(idDefs->GetLength());
	      for (u_int i = idDefs->GetLength(); i--; ) {
		TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
		if (idDef != INVALID_POINTER) // IdDef id
		  localEnv->Add(idDef->Sel(0), conVal->Sel(i));
	      }
	      pc = TagVal::FromWord(triple->Sel(2));
	      goto loop;
	    }
	  }
	} else { // nullary constructor
	  Constructor *constructor = reinterpret_cast<Constructor *>(conVal);
	  Vector *tests = Vector::FromWord(pc->Sel(1));
	  u_int ntests = tests->GetLength();
	  for (u_int i = 0; i < ntests; i++) {
	    Tuple *pair = Tuple::FromWord(tests->Sub(i));
	    suspendWord = GetIdRef(pair->Sel(0), globalEnv, localEnv);
	    Constructor *testConstructor = Constructor::FromWord(suspendWord);
	    if (testConstructor == INVALID_POINTER) SUSPEND(suspendWord);
	    if (testConstructor == constructor) {
	      pc = TagVal::FromWord(pair->Sel(1));
	      goto loop;
	    }
	  }
	}
	pc = TagVal::FromWord(pc->Sel(3));
      }
      break;
    case Pickle::VecTest: // of idRef * (idDef vector * instr) vector * instr
      {
	word suspendWord = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	Vector *vector = Vector::FromWord(suspendWord);
	if (vector == INVALID_POINTER) SUSPEND(suspendWord);
	u_int value = vector->GetLength();
	Vector *tests = Vector::FromWord(pc->Sel(1));
	u_int ntests = tests->GetLength();
	for (u_int i = 0; i < ntests; i++) {
	  Tuple *pair = Tuple::FromWord(tests->Sub(i));
	  Vector *idDefs = Vector::FromWord(pair->Sel(0));
	  if (idDefs->GetLength() == value) {
	    for (u_int i = value; i--; ) {
	      TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
	      if (idDef != INVALID_POINTER) // IdDef id
		localEnv->Add(idDef->Sel(0), vector->Sub(i));
	    }
	    pc = TagVal::FromWord(pair->Sel(1));
	    goto loop;
	  }
	}
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::Shared: // of stamp * instr
      {
	pc = TagVal::FromWord(pc->Sel(1));
      }
      break;
    case Pickle::Return: // of idRef args
      {
	TagVal *returnArgs = TagVal::FromWord(pc->Sel(0));
	switch (Pickle::GetArgs(returnArgs)) {
	case Pickle::OneArg:
	  Scheduler::currentArgs =
	    Interpreter::OneArg(GetIdRef(returnArgs->Sel(0),
					 globalEnv, localEnv));
	  return Interpreter::CONTINUE;
	case Pickle::TupArgs:
	  {
	    Vector *returnIdRefs = Vector::FromWord(returnArgs->Sel(0));
	    u_int nargs  = returnIdRefs->GetLength();
	    Block *pargs = Interpreter::TupArgs(nargs);
	    for (u_int i = nargs; i--; ) {
	      pargs->InitArg(i, GetIdRef(returnIdRefs->Sub(i),
					 globalEnv, localEnv));
	    }
	    Scheduler::currentArgs = pargs->ToWord();
	    return Interpreter::CONTINUE;
	  }
	}
      }
      break;
    default:
      Assert(0);
      return Interpreter::CONTINUE;
    }
  }
  Assert(0);
  return Interpreter::CONTINUE;
}

Interpreter::Result
AbstractCodeInterpreter::Handle(word exn, word /*debug*/,
				TaskStack *taskStack) {
  AbstractCodeFrame *frame =
    AbstractCodeFrame::FromWord(taskStack->GetFrame());
  if (frame->IsHandlerFrame()) {
    Block *args = Interpreter::TupArgs(2);
    args->InitArg(0, exn);
    args->InitArg(1, exn); //--** support debug
    return Run(args->ToWord(), taskStack);
  } else {
    taskStack->PopFrame();
    Scheduler::currentData = exn;
    return Interpreter::RAISE;
  }
}

const char *AbstractCodeInterpreter::Identify() {
  return "AbstractCodeInterpreter";
}

const char *AbstractCodeInterpreter::ToString(word args, TaskStack *taskStack) {
  return "AbstractCodeInterpreter::ToString";
}
