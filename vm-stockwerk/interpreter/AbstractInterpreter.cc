//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "interpreter/AbstractInterpreter.hh"
#endif

#include "scheduler/Closure.hh"
#include "scheduler/Scheduler.hh"
#include "builtins/Primitive.hh"
#include "builtins/GlobalPrimitives.hh"
#include "interpreter/Pickle.hh"
#include "interpreter/AbstractInterpreter.hh"

//
// This interpreter's concrete code representation
//

ConcreteCode *AbstractInterpreter::Prepare(word abstractCode) {
  Assert(TagVal::FromWord(abstractCode)->GetTag() == Pickle::Function &&
	 TagVal::FromWord(abstractCode)->GetWidth() == 3);
  //--** block on all transients
  return ConcreteCode::New(abstractCode, this, 0);
}

//
// Local environment representation
//

class Environment: private Array {
public:
  using Array::ToWord;

  static Environment *New(u_int size) {
    return static_cast<Environment *>(Array::New(size));
  }
  static Environment *FromWord(word w) {
    return static_cast<Environment *>(Array::FromWord(w));
  }
  static Environment *FromWordDirect(word w) {
    return static_cast<Environment *>(Array::FromWordDirect(w));
  }

  void Add(word id, word value) {
    Update(Store::WordToInt(id), value);
  }
  word Lookup(word id) {
    return Sub(Store::WordToInt(id));
  }
  void Kill(word id) {
    Update(Store::WordToInt(id), Store::IntToWord(0));
  }
};

//
// This interpreter uses the following stack frame layout:
//
//    0 to nargs - 1: arguments
//    #args + INTERPRETER_POS: Interpreter *interpreter
//    #args + PC_POS: TagVal *pc
//    #args + CLOSURE_POS: Closure *closure
//    #args + LOCAL_ENV_POS: Environment *localEnv
//    #args + FORMAL_ARGS_POS: id args (to receive arguments)
//

static const u_int FRAME_SIZE = 5;
static const u_int INTERPRETER_POS = 0;
static const u_int PC_POS = 1;
static const u_int CLOSURE_POS = 2;
static const u_int LOCAL_ENV_POS = 3;
static const u_int FORMAL_ARGS_POS = 4;

void AbstractInterpreter::PushCall(TaskStack *taskStack, Closure *closure) {
  ConcreteCode *concreteCode = closure->GetConcreteCode();
  Assert(concreteCode->GetInterpreter() == this);
  TagVal *function = TagVal::FromWordDirect(concreteCode->GetAbstractCode());
  // datatype function = Function of int * int * idDef args * instr
  Assert(Store::WordToInt(function->Sel(0)) == closure->GetSize());
  taskStack->PushFrame(FRAME_SIZE);
  taskStack->PutUnmanagedPointer(INTERPRETER_POS, this);
  taskStack->PutWord(PC_POS, function->Sel(3));
  taskStack->PutWord(CLOSURE_POS, closure->ToWord());
  u_int nlocals = Store::WordToInt(function->Sel(1));
  taskStack->PutWord(LOCAL_ENV_POS, Environment::New(nlocals)->ToWord());
  taskStack->PutWord(FORMAL_ARGS_POS, function->Sel(2));
}

void AbstractInterpreter::PopFrame(TaskStack *taskStack) {
  taskStack->PopFrame(FRAME_SIZE);
}

inline void PushState(TaskStack *taskStack,
		      Interpreter *interpreter, TagVal *pc, Closure *closure,
		      Environment *localEnv, TagVal *formalArgs) {
  taskStack->PushFrame(FRAME_SIZE);
  taskStack->PutUnmanagedPointer(INTERPRETER_POS, interpreter);
  taskStack->PutWord(PC_POS, pc->ToWord());
  taskStack->PutWord(CLOSURE_POS, closure->ToWord());
  taskStack->PutWord(LOCAL_ENV_POS, localEnv->ToWord());
  taskStack->PutWord(FORMAL_ARGS_POS, formalArgs->ToWord());
}

inline void PushState(TaskStack *taskStack,
		      Interpreter *interpreter, TagVal *pc, Closure *globalEnv,
		      Environment *localEnv) {
  TagVal *formalArgs = TagVal::New(Pickle::TupArgs, 1);
  formalArgs->Init(0, Vector::New(0)->ToWord());
  PushState(taskStack, interpreter, pc, globalEnv, localEnv, formalArgs);
}

#define SUSPEND(w) {						\
  PushState(taskStack, this, pc, globalEnv, localEnv);		\
  taskStack->PushFrame(1);					\
  taskStack->PutWord(0, w);					\
  return Result(Result::REQUEST, 1);				\
}

inline word GetIdRef(word idRef, Closure *globalEnv, Environment *localEnv) {
  TagVal *tagVal = TagVal::FromWord(idRef);
  switch (Pickle::GetIdRef(tagVal)) {
  case Pickle::Local:
    return localEnv->Lookup(tagVal->Sel(0));
  case Pickle::Global:
    return globalEnv->Sub(Store::WordToInt(tagVal->Sel(0)));
  default:
    Error("AbstractInterpreter::GetIdRef: invalid idRef tag");
  }
}

//
// Execution
//

Interpreter::Result
AbstractInterpreter::Run(TaskStack *taskStack, int nargs) {
  u_int nslots = nargs == -1? 1: nargs;
  Assert(Store::WordToUnmanagedPointer(taskStack->GetWord(nslots + INTERPRETER_POS)) == this);
  TagVal *pc = TagVal::FromWordDirect(taskStack->GetWord(nslots + PC_POS));
  Closure *globalEnv =
    Closure::FromWordDirect(taskStack->GetWord(nslots + CLOSURE_POS));
  Environment *localEnv =
    Environment::FromWordDirect(taskStack->GetWord(nslots + LOCAL_ENV_POS));
  TagVal *formalArgs =
    TagVal::FromWordDirect(taskStack->GetWord(nslots + FORMAL_ARGS_POS));
  // Calling convention conversion:
  switch (Pickle::GetArgs(formalArgs)) {
  case Pickle::OneArg:
    {
      word formalId = formalArgs->Sel(0);
      if (nargs == ~1) {
	localEnv->Add(formalId, taskStack->GetWord(0));
      } else if (nargs == 0) {
	localEnv->Add(formalId, Store::IntToWord(0)); // unit
      } else { // construct
	Tuple *tuple = Tuple::New(nslots);
	for (u_int i = nslots; i--; )
	  tuple->Init(i, taskStack->GetWord(i));
	localEnv->Add(formalId, tuple->ToWord());
      }
    }
    break;
  case Pickle::TupArgs:
    {
      Vector *formalIds = Vector::FromWord(formalArgs->Sel(0));
      if (nargs == ~1) { // deconstruct
	word suspendWord = taskStack->GetWord(0);
	Tuple *tuple = Tuple::FromWord(suspendWord);
	if (tuple == INVALID_POINTER) {
	  taskStack->PopFrame(1);
	  taskStack->
	    PushCall(Closure::FromWordDirect(GlobalPrimitives::Future_await));
	  taskStack->PushFrame(1);
	  taskStack->PutWord(0, suspendWord);
	  return Result(Result::CONTINUE, 1);
	}
	Assert(tuple->GetWidth() == nargs);
	for (u_int i = nargs; i--; )
	  localEnv->Add(formalIds->Sub(i), tuple->Sel(i));
      } else {
	Assert(formalIds->GetLength() == nargs);
	for (u_int i = nargs; i--; )
	  localEnv->Add(formalIds->Sub(i), taskStack->GetWord(i));
      }
    }
    break;
  }
  taskStack->PopFrame(nslots + FRAME_SIZE);
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
    case Pickle::PutCon: // of id * con * idRef vector * instr
      {
	Vector *idRefs = Vector::FromWord(pc->Sel(2));
	u_int nargs = idRefs->GetLength();
	TagVal *conBlock = TagVal::FromWord(pc->Sel(1));
	word suspendWord;
	switch (Pickle::GetCon(conBlock)) {
	case Pickle::Con:
	  suspendWord = localEnv->Lookup(conBlock->Sel(0));
	  break;
	case Pickle::StaticCon:
	  suspendWord = conBlock->Sel(0);
	  break;
	default:
	  Error("AbstractInterpreter::Run: invalid con tag");
	  break;
	}
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
	Closure *closure = Closure::New(Prepare(pc->Sel(2)), nglobals);
	for (u_int i = nglobals; i--; )
	  closure->Init(i, GetIdRef(idRefs->Sub(i), globalEnv, localEnv));
	localEnv->Add(pc->Sel(0), closure->ToWord());
	pc = TagVal::FromWord(pc->Sel(3));
      }
      break;
    case Pickle::AppPrim: // of idDef * string * idRef vector * instr option
      {
	TagVal *instrOpt = TagVal::FromWord(pc->Sel(3));
	if (instrOpt != INVALID_POINTER) { // SOME instr
	  // Save our state for return:
	  Vector *formalIds = Vector::New(1);
	  formalIds->Init(0, pc->Sel(0));
	  TagVal *formalArgs = TagVal::New(Pickle::TupArgs, 1);
	  formalArgs->Init(0, formalIds->ToWord());
	  PushState(taskStack, this, TagVal::FromWord(instrOpt->Sel(0)),
		    globalEnv, localEnv, formalArgs);
	}
	// Push a call frame for the primitive:
	String *name = String::FromWord(pc->Sel(1));
	taskStack->PushCall(Closure::FromWordDirect(Primitive::Lookup(name)));
	Vector *actualIdRefs = Vector::FromWord(pc->Sel(2));
	u_int nargs = actualIdRefs->GetLength();
	taskStack->PushFrame(nargs);
	for (u_int i = nargs; i--; )
	  taskStack->PutWord(i, GetIdRef(actualIdRefs->Sub(i),
					 globalEnv, localEnv));
	return Result(Result::CONTINUE, nargs);
      }
      break;
    case Pickle::AppVar: // of idDef args * idRef * idRef args * instr option
      {
	word suspendWord = GetIdRef(pc->Sel(1), globalEnv, localEnv);
	Closure *closure = Closure::FromWord(suspendWord);
	if (closure == INVALID_POINTER) SUSPEND(suspendWord);
	TagVal *instrOpt = TagVal::FromWord(pc->Sel(3));
	if (instrOpt != INVALID_POINTER) { // SOME instr
	  // Save our state for return:
	  PushState(taskStack, this, TagVal::FromWord(instrOpt->Sel(0)),
		    globalEnv, localEnv, TagVal::FromWord(pc->Sel(0)));
	}
	taskStack->PushCall(closure);
	TagVal *actualArgs = TagVal::FromWord(pc->Sel(2));
	switch (Pickle::GetArgs(actualArgs)) {
	case Pickle::OneArg:
	  taskStack->PushFrame(1);
	  taskStack->PutWord(0, GetIdRef(actualArgs->Sel(0),
					 globalEnv, localEnv));
	  return Result(Result::CONTINUE, -1);
	case Pickle::TupArgs:
	  {
	    Vector *actualIdRefs = Vector::FromWord(actualArgs->Sel(0));
	    u_int nargs = actualIdRefs->GetLength();
	    taskStack->PushFrame(nargs);
	    for (u_int i = nargs; i--; )
	      taskStack->PutWord(i, GetIdRef(actualIdRefs->Sub(i),
					     globalEnv, localEnv));
	    return Result(Result::CONTINUE, nargs);
	  }
	}
      }
      break;
    case Pickle::AppConst: // of idDef args * value * idRef args * instr option
      {
	//--** AppConst not implemented yet
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
	  Assert(tuple->GetWidth() == idDefs->GetLength());
	  for (u_int i = nargs; i--; ) {
	    TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
	    if (idDef != INVALID_POINTER) // IdDef id
	      localEnv->Add(idDef->Sel(0), tuple->Sel(i));
	  }
	}
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::Raise: // of idRef
      {
	taskStack->PushFrame(1);
	taskStack->PutWord(0, GetIdRef(pc->Sel(0), globalEnv, localEnv));
	return Result(Result::RAISE);
      }
      break;
    case Pickle::Try: // of instr * idDef * instr
      {
	TagVal *formalArgs = TagVal::New(Pickle::TupArgs, 1);
	Vector *vector = Vector::New(1);
	vector->Init(0, pc->Sel(1));
	formalArgs->Init(0, vector->ToWord());
	PushState(taskStack, this, TagVal::FromWord(pc->Sel(2)),
		  globalEnv, localEnv, formalArgs);
	taskStack->PushFrame(1);
	taskStack->PutUnmanagedPointer(0, NULL);
	pc = TagVal::FromWord(pc->Sel(0));
      }
      break;
    case Pickle::EndTry: // of instr
      {
	taskStack->PopFrame(1);
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
	      Assert(tagVal->GetWidth() == idDefs->GetLength());
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
      // of idRef * (con * instr) vector
      //          * (con * idDef vector * instr) vector * instr
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
	    TagVal *conBlock = TagVal::FromWord(triple->Sel(0));
	    switch (Pickle::GetCon(conBlock)) {
	    case Pickle::Con:
	      suspendWord = localEnv->Lookup(conBlock->Sel(0));
	      break;
	    case Pickle::StaticCon:
	      suspendWord = conBlock->Sel(0);
	      break;
	    }
	    Constructor *testConstructor = Constructor::FromWord(suspendWord);
	    if (testConstructor == INVALID_POINTER) SUSPEND(suspendWord);
	    if (testConstructor == constructor) {
	      Vector *idDefs = Vector::FromWord(triple->Sel(1));
	      Assert(conVal->GetWidth() == idDefs->GetLength());
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
	    TagVal *conBlock = TagVal::FromWord(pair->Sel(0));
	    switch (Pickle::GetCon(conBlock)) {
	    case Pickle::Con:
	      suspendWord = localEnv->Lookup(conBlock->Sel(0));
	      break;
	    case Pickle::StaticCon:
	      suspendWord = conBlock->Sel(0);
	      break;
	    }
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
	  taskStack->PushFrame(1);
	  taskStack->PutWord(0, GetIdRef(returnArgs->Sel(0),
					 globalEnv, localEnv));
	  return Result(Result::CONTINUE, -1);
	case Pickle::TupArgs:
	  {
	    Vector *returnIdRefs = Vector::FromWord(returnArgs->Sel(0));
	    u_int nargs = returnIdRefs->GetLength();
	    taskStack->PushFrame(nargs);
	    for (u_int i = nargs; i--; )
	      taskStack->PutWord(i, GetIdRef(returnIdRefs->Sub(i),
					     globalEnv, localEnv));
	    return Result(Result::CONTINUE, nargs);
	  }
	}
      }
      break;
    }
  }
  PushState(taskStack, this, pc, globalEnv, localEnv);
  return Result(Result::PREEMPT, 0);
}
