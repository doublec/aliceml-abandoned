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
#include "generic/TaskStack.hh"
#include "generic/Scheduler.hh"
#include "generic/Backtrace.hh"
#include "generic/Closure.hh"
#include "generic/ConcreteCode.hh"
#include "generic/Transients.hh"
#include "alice/Data.hh"
#include "alice/AbstractCode.hh"
#include "alice/LazySelInterpreter.hh"
#include "alice/AbstractCodeInterpreter.hh"

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
    return static_cast<Environment *>(Array::New(size));
  }
  // Environment Untagging
  static Environment *FromWordDirect(word x) {
    return static_cast<Environment *>(Array::FromWordDirect(x));
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
    return TagVal::FromWordDirect(StackFrame::GetArg(FORMAL_ARGS_POS));
  }
  // AbstractCodeFrame Constructor
  static AbstractCodeFrame *New(Interpreter *interpreter,
				word pc,
				Closure *closure,
				Environment *env,
				word args) {
    StackFrame *frame =
      StackFrame::New(ABSTRACT_CODE_FRAME, interpreter, SIZE);
    frame->InitArg(PC_POS, pc);
    frame->InitArg(CLOSURE_POS, closure->ToWord());
    frame->InitArg(LOCAL_ENV_POS, env->ToWord());
    frame->InitArg(FORMAL_ARGS_POS, args);
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
    frame->InitArg(PC_POS, pc);
    frame->InitArg(CLOSURE_POS, closure->ToWord());
    frame->InitArg(LOCAL_ENV_POS, env->ToWord());
    frame->InitArg(FORMAL_ARGS_POS, args);
    return static_cast<AbstractCodeHandlerFrame *>(frame);
  }
};

// Interpreter Helper
inline void PushState(TaskStack *taskStack,
		      TagVal *pc,
		      Closure *closure,
		      Environment *localEnv,
		      TagVal *formalArgs) {
  AbstractCodeFrame *frame =
    AbstractCodeFrame::New(AbstractCodeInterpreter::self, pc->ToWord(),
			   closure, localEnv, formalArgs->ToWord());
  taskStack->PushFrame(frame->ToWord());
}

inline void PushState(TaskStack *taskStack,
		      TagVal *pc,
		      Closure *globalEnv,
		      Environment *localEnv) {
  //--** formalArgs should only be constructed once
  TagVal *formalArgs = TagVal::New(AbstractCode::TupArgs, 1);
  formalArgs->Init(0, Vector::New(0)->ToWord());
  PushState(taskStack, pc, globalEnv, localEnv, formalArgs);
}

inline word GetIdRef(word idRef, Closure *globalEnv, Environment *localEnv) {
  TagVal *tagVal = TagVal::FromWordDirect(idRef);
  switch (AbstractCode::GetIdRef(tagVal)) {
  case AbstractCode::Local:
    return localEnv->Lookup(tagVal->Sel(0));
  case AbstractCode::Global:
    return globalEnv->Sub(Store::WordToInt(tagVal->Sel(0)));
  case AbstractCode::Immediate:
    return tagVal->Sel(0);
  default:
    Error("AbstractCodeInterpreter::GetIdRef: invalid idRef tag");
  }
}

//
// Interpreter Functions
//
AbstractCodeInterpreter *AbstractCodeInterpreter::self;

Block *
AbstractCodeInterpreter::GetAbstractRepresentation(Block *blockWithHandler) {
  ConcreteCode *concreteCode = static_cast<ConcreteCode *>(blockWithHandler);
  return Store::DirectWordToBlock(concreteCode->Get(1));
}

void AbstractCodeInterpreter::PushCall(TaskStack *taskStack,
				       Closure *closure) {
  ConcreteCode *concreteCode =
    ConcreteCode::FromWord(closure->GetConcreteCode());
  Assert(concreteCode->GetInterpreter() == this);
  // Function of coord * int * int * idDef args * instr
  TagVal *function = TagVal::FromWord(concreteCode->Get(0));
  Assert(function->GetTag() == 0);
  function->AssertWidth(5);
  int nlocals = Store::WordToInt(function->Sel(2));
  AbstractCodeFrame *frame =
    AbstractCodeFrame::New(this,
			   function->Sel(4),
			   closure,
			   Environment::New(nlocals),
			   function->Sel(3));
  taskStack->PushFrame(frame->ToWord());
}

#define REQUEST(w) {					\
  PushState(taskStack, pc, globalEnv, localEnv);	\
  Scheduler::currentData = w;				\
  Scheduler::nArgs = 0;					\
  return Interpreter::REQUEST;				\
}

#define CHECK_PREEMPT() {				\
  if (Scheduler::TestPreempt() || Store::NeedGC())	\
    return Interpreter::PREEMPT;			\
  else							\
    return Interpreter::CONTINUE;			\
}

Interpreter::Result AbstractCodeInterpreter::Run(TaskStack *taskStack) {
  AbstractCodeFrame *frame =
    AbstractCodeFrame::FromWordDirect(taskStack->GetFrame());
  Assert(!frame->IsHandlerFrame());
  Assert(frame->GetInterpreter() == this);
  TagVal *pc            = frame->GetPC();
  Closure *globalEnv    = frame->GetClosure();
  Environment *localEnv = frame->GetLocalEnv();
  TagVal *formalArgs    = frame->GetFormalArgs();
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
      if (nArgs != 0) {
	if (Interpreter::Deconstruct()) {
	  // Scheduler::currentData has been set by Interpreter::Deconstruct
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
    Error("AbstractCodeInterpreter::Run: invalid formalArgs");
  }
  taskStack->PopFrame(); // Discard Frame
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
	localEnv->Add(pc->Sel(0), GetIdRef(pc->Sel(1), globalEnv, localEnv));
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::PutNew: // of id * string * instr
      {
	Constructor *constructor = Constructor::New(pc->Sel(1));
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
	  tagVal->Init(i, GetIdRef(idRefs->Sub(i), globalEnv, localEnv));
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
	ConVal *conVal = ConVal::New(constructor, nargs);
	for (u_int i = nargs; i--; )
	  conVal->Init(i, GetIdRef(idRefs->Sub(i), globalEnv, localEnv));
	localEnv->Add(pc->Sel(0), conVal->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::PutRef: // of id * idRef * instr
      {
	word contents = GetIdRef(pc->Sel(1), globalEnv, localEnv);
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
	    tuple->Init(i, GetIdRef(idRefs->Sub(i), globalEnv, localEnv));
	  localEnv->Add(pc->Sel(0), tuple->ToWord());
	}
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::PutVec: // of id * idRef vector * instr
      {
	Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
	u_int nargs = idRefs->GetLength();
	Vector *vector = Vector::New(nargs);
	for (u_int i = nargs; i--; )
	  vector->Init(i, GetIdRef(idRefs->Sub(i), globalEnv, localEnv));
	localEnv->Add(pc->Sel(0), vector->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(2));
      }
      break;
    case AbstractCode::PutFun: // of id * idRef vector * function * instr
      {
	Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
	u_int nglobals = idRefs->GetLength();
	Closure *closure = Closure::New(pc->Sel(2), nglobals);
	for (u_int i = nglobals; i--; )
	  closure->Init(i, GetIdRef(idRefs->Sub(i), globalEnv, localEnv));
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
	  PushState(taskStack, TagVal::FromWordDirect(idDefInstr->Sel(1)),
		    globalEnv, localEnv, formalArgs);
	}
	// Push a call frame for the primitive
	Vector *actualIdRefs = Vector::FromWordDirect(pc->Sel(1));
	u_int nArgs = actualIdRefs->GetLength();
	Scheduler::nArgs = nArgs == 1? Scheduler::ONE_ARG: nArgs;
	for (u_int i = nArgs; i--; )
	  Scheduler::currentArgs[i] =
	    GetIdRef(actualIdRefs->Sub(i), globalEnv, localEnv);
	return taskStack->PushCall(pc->Sel(0));
      }
      break;
    case AbstractCode::AppVar:
      // of idRef * idRef args * (idDef args * instr) option
    case AbstractCode::AppConst:
      // of value * idRef args * (idDef args * instr) option
      {
	TagVal *idDefArgsInstrOpt = TagVal::FromWord(pc->Sel(2));
	if (idDefArgsInstrOpt != INVALID_POINTER) { // SOME ...
	  // Save our state for return
	  Tuple *idDefArgsInstr =
	    Tuple::FromWordDirect(idDefArgsInstrOpt->Sel(0));
	  PushState(taskStack, TagVal::FromWordDirect(idDefArgsInstr->Sel(1)),
		    globalEnv, localEnv,
		    TagVal::FromWordDirect(idDefArgsInstr->Sel(0)));
	}
	TagVal *actualArgs = TagVal::FromWordDirect(pc->Sel(1));
	switch (AbstractCode::GetArgs(actualArgs)) {
	case AbstractCode::OneArg:
	  Scheduler::nArgs = Scheduler::ONE_ARG;
	  Scheduler::currentArgs[0] =
	    GetIdRef(actualArgs->Sel(0), globalEnv, localEnv);
	  break;
	case AbstractCode::TupArgs:
	  {
	    Vector *actualIdRefs = Vector::FromWordDirect(actualArgs->Sel(0));
	    u_int nArgs  = actualIdRefs->GetLength();
	    Scheduler::nArgs = nArgs;
	    for (u_int i = nArgs; i--; )
	      Scheduler::currentArgs[i] =
		GetIdRef(actualIdRefs->Sub(i), globalEnv, localEnv);
	  }
	  break;
	}
	switch (AbstractCode::GetInstr(pc)) {
	case AbstractCode::AppVar:
	  if (Scheduler::TestPreempt() || Store::NeedGC()) {
	    Interpreter::Result res =
	      taskStack->PushCall(GetIdRef(pc->Sel(0), globalEnv, localEnv));
	    return res == Interpreter::CONTINUE? Interpreter::PREEMPT: res;
	  } else {
	    word closure = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	    return taskStack->PushCall(closure);
	  }
	case AbstractCode::AppConst:
	  if (Scheduler::TestPreempt() || Store::NeedGC()) {
	    Interpreter::Result res = taskStack->PushCall(pc->Sel(0));
	    return res == Interpreter::CONTINUE? Interpreter::PREEMPT: res;
	  } else
	    return taskStack->PushCall(pc->Sel(0));
	default:
	  Error("AbstractCodeInterpreter: inconsistent (AppVar/AppConst)")
	}
      }
      break;
    case AbstractCode::GetRef: // of id * idRef * instr
      {
	word requestWord = GetIdRef(pc->Sel(1), globalEnv, localEnv);
	Cell *cell = Cell::FromWord(requestWord);
	if (cell == INVALID_POINTER) REQUEST(requestWord);
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
	} else {
	  Tuple *tuple = Tuple::FromWord(requestWord);
	  if (tuple == INVALID_POINTER) REQUEST(requestWord);
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
	localEnv->Add(pc->Sel(0),
		      tuple->Sel(Store::DirectWordToInt(pc->Sel(2))));
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::LazySel: // of id * idRef * int * instr
      {
	word tuple = GetIdRef(pc->Sel(1), globalEnv, localEnv);
	int index = Store::DirectWordToInt(pc->Sel(2));
	Closure *closure = LazySelClosure::New(tuple, index);
	localEnv->Add(pc->Sel(0), Byneed::New(closure->ToWord())->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(3));
      }
      break;
    case AbstractCode::Raise: // of idRef
      {
	Scheduler::currentData = GetIdRef(pc->Sel(0), globalEnv, localEnv);
	Scheduler::currentBacktrace = Backtrace::New(frame->ToWord());
	return Interpreter::RAISE;
      }
      break;
    case AbstractCode::Reraise: // of idRef
      {
	Tuple *package =
	  Tuple::FromWordDirect(GetIdRef(pc->Sel(0), globalEnv, localEnv));
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
	  AbstractCodeHandlerFrame::New(this,
					pc->Sel(3),
					globalEnv,
					localEnv,
					formalArgs->ToWord());
	taskStack->PushFrame(frame->ToWord());
	pc = TagVal::FromWordDirect(pc->Sel(0));
      }
      break;
    case AbstractCode::EndTry: // of instr
      {
	Assert(StackFrame::FromWordDirect(taskStack->GetFrame())->GetLabel() ==
	       ABSTRACT_CODE_HANDLER_FRAME);
	taskStack->PopFrame();
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
	  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
	  if (static_cast<u_int>(tag) < tests->GetLength()) {
	    Tuple *tuple = Tuple::FromWordDirect(tests->Sub(tag));
	    Assert(tuple->Sel(0) == Store::IntToWord(0)); // NONE
	    pc = TagVal::FromWordDirect(tuple->Sel(1));
	    goto loop;
	  }
	} else { // non-nullary constructor
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
	    GetIdRef(returnArgs->Sel(0), globalEnv, localEnv);
	  break;
	case AbstractCode::TupArgs:
	  {
	    Vector *returnIdRefs = Vector::FromWordDirect(returnArgs->Sel(0));
	    u_int nArgs = returnIdRefs->GetLength();
	    if (nArgs < Scheduler::maxArgs) {
	      Scheduler::nArgs = nArgs;
	      for (u_int i = nArgs; i--; )
		Scheduler::currentArgs[i] =
		  GetIdRef(returnIdRefs->Sub(i), globalEnv, localEnv);
	    } else {
	      Tuple *tuple = Tuple::New(nArgs);
	      for (u_int i = nArgs; i--; )
		tuple->Init(i, GetIdRef(returnIdRefs->Sub(i),
					globalEnv, localEnv));
	      Scheduler::nArgs = Scheduler::ONE_ARG;
	      Scheduler::currentArgs[0] = tuple->ToWord();
	    }
	  }
	  break;
	}
	CHECK_PREEMPT();
      }
      break;
    default:
      Error("AbstractCodeInterpreter: unknown instruction");
    }
  }
}

Interpreter::Result
AbstractCodeInterpreter::Handle(word exn, Backtrace *trace,
				TaskStack *taskStack) {
  AbstractCodeFrame *frame =
    AbstractCodeFrame::FromWordDirect(taskStack->GetFrame());
  if (frame->IsHandlerFrame()) {
    Tuple *package = Tuple::New(2);
    package->Init(0, exn);
    package->Init(1, trace->ToWord());
    Scheduler::nArgs = 2;
    Scheduler::currentArgs[0] = package->ToWord();
    Scheduler::currentArgs[1] = exn;
    taskStack->PopFrame();
    AbstractCodeFrame *newFrame =
      AbstractCodeFrame::New(self, frame->GetPC()->ToWord(),
			     frame->GetClosure(), frame->GetLocalEnv(),
			     frame->GetFormalArgs()->ToWord());
    taskStack->PushFrame(newFrame->ToWord());
    return Interpreter::CONTINUE;
  } else {
    taskStack->PopFrame();
    trace->Enqueue(frame->ToWord());
    Scheduler::currentBacktrace = trace;
    Scheduler::currentData = exn;
    return Interpreter::RAISE;
  }
}

const char *AbstractCodeInterpreter::Identify() {
  return "AbstractCodeInterpreter";
}

void AbstractCodeInterpreter::DumpFrame(word frameWord) {
  AbstractCodeFrame *frame = AbstractCodeFrame::FromWordDirect(frameWord);
  Closure *closure = frame->GetClosure();
  ConcreteCode *concreteCode =
    ConcreteCode::FromWord(closure->GetConcreteCode());
  Assert(concreteCode != INVALID_POINTER);
  TagVal *function = TagVal::FromWord(concreteCode->Get(0));
  Tuple *coord = Tuple::FromWordDirect(function->Sel(0));
  String *name = String::FromWordDirect(coord->Sel(0));
  fprintf(stderr, "Alice %s %.*s, line %d\n",
	  frame->IsHandlerFrame()? "handler": "function",
	  (int) name->GetSize(), name->GetValue(),
	  Store::DirectWordToInt(coord->Sel(1)));
}
